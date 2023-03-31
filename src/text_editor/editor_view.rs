use std::collections::HashMap;
use std::ops::{Deref, DerefMut, Range};
use std::path::{Path};
use std::sync::mpsc;
use std::sync::mpsc::Sender;
use std::sync::Mutex;
use std::thread;
use druid::kurbo::{BezPath, Line, PathEl};
use druid::piet::{PietText, Text, TextAttribute, TextLayout, TextLayoutBuilder};
use druid::{Affine, Application, BoxConstraints, ClipboardFormat, Color, Data, Env, Event, EventCtx, ExtEventSink, FontStyle, HotKey, KeyEvent, LayoutCtx, LifeCycle, LifeCycleCtx, MouseButton, PaintCtx, Point, Rect, RenderContext, Size, SysMods, UpdateCtx, Widget, WidgetId};
use lazy_static::lazy_static;
use ropey::Rope;
use syntect::parsing::SyntaxReference;
use tracing::{debug, error, trace};
use crate::text_buffer::{EditStack, position, rope_utils, SelectionLineRange};
use crate::text_buffer::syntax::{StateCache, StyledLinesCache, SYNTAXSET};
use crate::text_editor::{ChildWidget, EDITOR_LEFT_PADDING, env, FILE_REMOVED, FOCUS_EDITOR, FONT_NAME, FONT_SIZE, FONT_WEIGTH, HIGHLIGHT, RELOAD_FROM_DISK, REQUEST_NEXT_SEARCH, RESET_HELD_STATE, SCROLL_TO, SELECT_LINE, WIDGET_ATTACHED};
use crate::text_editor::palette_view::{DialogResult, PALETTE_CALLBACK, PaletteBuilder, PaletteCommandType};

#[derive(Debug, Default)]
struct SelectionPath {
    elem: Vec<PathEl>,
    last_range: Option<SelectionLineRange>,
    last_x: f64,
}

impl Deref for SelectionPath {
    type Target = Vec<PathEl>;
    fn deref(&self) -> &Self::Target {
        &self.elem
    }
}

impl DerefMut for SelectionPath {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.elem
    }
}

impl SelectionPath {
    fn new() -> Self {
        Self {
            elem: Vec::new(),
            last_range: None,
            last_x: 0.5,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CommonMetrics {
    pub(crate) font_advance: f64,
    font_baseline: f64,
    font_descent: f64,
    pub(crate) font_height: f64,

    pub(crate) font_size: f64,

    page_len: u64,
}

impl CommonMetrics {
    pub fn new(text_ctx: &mut PietText, font_name: &str, size: Size) -> Self {
        let mut metrics = CommonMetrics::default();
        let font = text_ctx.font_family(font_name).unwrap();
        let layout = text_ctx
            .new_text_layout("8")
            .default_attribute(TextAttribute::Weight(FONT_WEIGTH))
            .font(font, metrics.font_size)
            .build()
            .unwrap();
        metrics.font_advance = layout.size().width;
        metrics.font_baseline = layout.line_metric(0).unwrap().baseline;
        metrics.font_height = layout.line_metric(0).unwrap().height;
        metrics.font_descent = metrics.font_height - metrics.font_baseline;
        metrics.page_len = (size.height / metrics.font_height).round() as u64;
        metrics
    }

    pub fn from_env(env: &Env) -> Self {
        CommonMetrics {
            font_baseline: env.get(env::FONT_BASELINE),
            font_advance: env.get(env::FONT_ADVANCE),
            font_descent: env.get(env::FONT_DESCENT),
            font_height: env.get(env::FONT_HEIGHT),
            font_size: env.get(env::FONT_SIZE),
            page_len: env.get(env::PAGE_LEN),
        }
    }

    pub fn to_env(self, env: &mut Env) {
        env.set(env::FONT_BASELINE, self.font_baseline);
        env.set(env::FONT_ADVANCE, self.font_advance);
        env.set(env::FONT_DESCENT, self.font_descent);
        env.set(env::FONT_HEIGHT, self.font_height);
        env.set(env::FONT_SIZE, self.font_size);
        env.set(env::PAGE_LEN, self.page_len);
    }
}

impl Default for CommonMetrics {
    fn default() -> Self {
        CommonMetrics {
            font_advance: 0.0,
            font_baseline: 0.0,
            font_descent: 0.0,
            font_height: 0.0,
            font_size: FONT_SIZE,
            page_len: 0,
        }
    }
}
#[derive(Debug, PartialEq, Eq)]
enum HeldState {
    None,
    Grapheme,
    Word,
    Line,
}

impl HeldState {
    fn is_held(&self) -> bool {
        *self != HeldState::None
    }
}

#[derive(Clone)]
pub enum BackgroundWorkerMessage {
    Start(WidgetId, WidgetId, ExtEventSink, StyledLinesCache),
    Stop(WidgetId),
    UpdateBuffer(WidgetId, SyntaxReference, Rope, usize),
    Focus(WidgetId)
}

pub type EditorEventHandler = Box<dyn FnMut(&mut EventCtx, &Event, &mut EditStack) -> ()>;

pub enum EditorKeyBindings {
    None,
    Cua,
    Custom(EditorEventHandler)
}

pub struct EditorView {
    delta_y: f64,
    delta_x: f64,
    page_len: usize,
    metrics: CommonMetrics,
    font_name: String,

    bg_color: Color,
    fg_color: Color,
    fg_sel_color: Color,
    bg_sel_color: Color,

    size: Size,
    owner_id: WidgetId,

    longest_line_len: f64,

    held_state: HeldState,

    highlighted_line: StyledLinesCache,

    event_handler: Option<EditorEventHandler>
}

struct State<'a> {
    pub editor_id: WidgetId,
    pub events: ExtEventSink,
    pub syntax: &'a SyntaxReference,
    pub current_index: usize,
    pub chunk_len: usize,
    pub rope: Rope,
    pub highlight_cache: StateCache,
    pub highlighted_line: StyledLinesCache
}

impl<'a> State <'a> {
    pub fn process_chunk(&mut self, owner_id: WidgetId) {
        if self.current_index < self.rope.len_lines() {
            self.highlight_cache.update_range(
                self.highlighted_line.clone(),
                &self.syntax,
                &self.rope,
                self.current_index,
                self.current_index + self.chunk_len,
            );
            let _ = self.events.submit_command(
                HIGHLIGHT,
                (self.current_index, self.current_index + self.chunk_len),
                owner_id,
            );
            self.current_index += self.chunk_len;
            // subsequent chunck are bigger, for better performance
            self.chunk_len = 1000;
        }
    }
}

lazy_static! {
    static ref BACKGROUND_TX: Mutex<Sender<BackgroundWorkerMessage>> = {
        let (tx, rx) = mpsc::channel::<BackgroundWorkerMessage>();

        thread::spawn(move || {
            debug!("Background worker started!");
            let mut states = HashMap::new();
            loop {
                match rx.recv() {
                    Ok(message) => match message {
                        BackgroundWorkerMessage::Start(owner_id, editor_id, events, lines) => {
                            trace!("added editor {:?} {:?}", owner_id, editor_id);
                            let mut state = State {
                                editor_id,
                                events,
                                syntax: SYNTAXSET.find_syntax_plain_text(),
                                current_index: 0,
                                chunk_len: 100,
                                rope: Rope::new(),
                                highlight_cache: StateCache::new(),
                                highlighted_line: lines,
                            };
                            state.process_chunk(owner_id);
                            states.insert(owner_id, state);
                        }
                        BackgroundWorkerMessage::Stop(id) => {
                            trace!("removing editor {:?}", id);
                            states.remove(&id);
                        },
                        BackgroundWorkerMessage::UpdateBuffer(id, s, rope, start) => {
                            if let Some(state) = states.get_mut(&id) {
                                state.syntax = SYNTAXSET.find_syntax_by_name(&s.name).unwrap();
                                state.rope = rope;
                                state.current_index = start;
                                state.chunk_len = 1000;

                                state.process_chunk(id);
                            } else {
                                error!("editor state not found {id:?}");
                            }
                        }
                        BackgroundWorkerMessage::Focus(id) => {
                            if let Some(state) = states.get_mut(&id) {
                                trace!("Sending FOCUS_EDITOR command");
                                state.events.submit_command(FOCUS_EDITOR, (), state.editor_id).expect("submit failed");
                            } else {
                                error!("editor state not found {id:?}");
                            }
                        }
                    },
                    _ => (),
                }

            }
        });

        Mutex::new(tx)
    };
}



impl Widget<EditStack> for EditorView {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, editor: &mut EditStack, _env: &Env) {
        let old_dx = self.delta_x;
        let old_dy = self.delta_y;
        let old_editor = editor.clone();

        let handled = self.handle_event(event, ctx, editor);
        if handled {
            ctx.set_handled();
        }
        if !old_editor.buffer.same(&editor.buffer) {
            self.put_caret_in_visible_range(ctx, editor);
        }

        #[allow(clippy::float_cmp)] // The equality will be true if we don't touch at all at self.delta_[xy]
        if old_dx != self.delta_x || old_dy != self.delta_y {
            ctx.request_paint();
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, editor: &EditStack, env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                ctx.submit_command(
                    WIDGET_ATTACHED
                        .with(ChildWidget::Editor(ctx.widget_id()))
                        .to(self.owner_id),
                );

                let start = BackgroundWorkerMessage::Start(self.owner_id, ctx.widget_id(), ctx.get_external_handle(), self.highlighted_line.clone());
                if let Ok(tx) = BACKGROUND_TX.lock() {
                    tx.send(start).expect("message send failed");
                }

                self.bg_color = env.get(crate::theme::EDITOR_BACKGROUND);
                self.fg_color = env.get(crate::theme::EDITOR_FOREGROUND);
                self.fg_sel_color = env.get(crate::theme::SELECTION_BACKGROUND);
                self.bg_sel_color = env.get(crate::theme::EDITOR_FOREGROUND);

                self.update_highlighter(editor, 0);
            }
            LifeCycle::BuildFocusChain => {
                ctx.register_for_focus();
            },
            LifeCycle::FocusChanged(flag) => {
                trace!("Editor {:?} focus changed: {flag:?}", ctx.widget_id());
                ctx.request_paint();
            }
            _ => (),
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditStack, data: &EditStack, _env: &Env) {
        if !old_data.buffer.same_content(&data.buffer) {
            let line = old_data
                .first_caret()
                .start_line(&old_data.buffer)
                .min(data.first_caret().start_line(&data.buffer));
            self.update_highlighter(data, line.index);
        }
        if !old_data.file.syntax.name.same(&data.file.syntax.name) {
            self.update_highlighter(data, 0);
        }
        if !old_data.same(data) {
            ctx.request_paint();
        }
    }

    fn layout(&mut self, layout_ctx: &mut LayoutCtx, bc: &BoxConstraints, _data: &EditStack, _env: &Env) -> Size {
        self.metrics = CommonMetrics::new(layout_ctx.text(), &self.font_name, bc.max());
        let h = if bc.max().height < self.metrics.font_height {
            self.metrics.font_height + 2.
        } else {
            bc.max().height
        };
        self.size = Size::new(bc.max().width, h);

        self.metrics = CommonMetrics::new(layout_ctx.text(), &self.font_name, self.size);
        self.page_len = (self.size.height / self.metrics.font_height).round() as usize;

        self.size
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditStack, env: &Env) {
        self.paint_editor(data, ctx, env);
    }
}

impl EditorView {

    pub fn focus_editor(id: WidgetId) {
        if let Ok(tx) = BACKGROUND_TX.lock() {
            tx.send(BackgroundWorkerMessage::Focus(id)).unwrap();
        }
    }

    pub fn new(owner_id: WidgetId, key_bindings: EditorKeyBindings) -> Self {

        let event_handler = match key_bindings {
            EditorKeyBindings::None => None,
            EditorKeyBindings::Cua => Some(EditorView::make_cua_key_bindings()),
            EditorKeyBindings::Custom(handler) => Some(handler)
        };

        let e = EditorView {
            bg_color: Color::BLACK,
            fg_color: Color::WHITE,
            fg_sel_color: Color::BLACK,
            bg_sel_color: Color::WHITE,

            metrics: Default::default(),
            font_name: FONT_NAME.to_string(),
            delta_x: 0.0,
            delta_y: 0.0,
            page_len: 0,

            size: Size::new(1.0, 1.0),
            owner_id,
            longest_line_len: 0.,
            held_state: HeldState::None,
            highlighted_line: StyledLinesCache::new(),
            event_handler,
        };

        e
    }

    pub fn make_cua_key_bindings() -> EditorEventHandler {
        let select_all_hotkey = HotKey::new(SysMods::Cmd, "a");
        let cut_hotkey = HotKey::new(SysMods::Cmd, "x");
        let copy_hotkey = HotKey::new(SysMods::Cmd, "c");
        let paste_hotkey = HotKey::new(SysMods::Cmd, "v");
        let undo_hotkey = HotKey::new(SysMods::Cmd, "z");
        let redo_hotkey = HotKey::new(SysMods::CmdShift, "Z");
        let redo2_hotkey = HotKey::new(SysMods::Cmd, "y");

        Box::new(move |ctx, event, editor| {
            match event {
                Event::KeyDown(event) => {
                    if cut_hotkey.matches(event) {
                        Application::global().clipboard().put_string(editor.selected_text());
                        editor.delete();
                        ctx.set_handled();
                    } else if copy_hotkey.matches(event) {
                        Application::global().clipboard().put_string(editor.selected_text());
                        ctx.set_handled();
                    } else if paste_hotkey.matches(event) {
                        let clipboard = Application::global().clipboard();
                        let supported_types = &[ClipboardFormat::TEXT];
                        let best_available_type = clipboard.preferred_format(supported_types);
                        if let Some(format) = best_available_type {
                            if  let Some(data) = clipboard.get_format(format) {
                                editor.insert(String::from_utf8_lossy(&data).as_ref());
                            }
                        }
                        ctx.set_handled();
                    } else if undo_hotkey.matches(event) {
                        editor.undo();
                        ctx.set_handled();
                    } else if redo_hotkey.matches(event) || redo2_hotkey.matches(event) {
                        editor.redo();
                        ctx.set_handled();
                    } else if select_all_hotkey.matches(event) {
                        editor.select_all();
                        ctx.set_handled();
                    }
                }
                _ => ()
            }
        })
    }

    fn update_highlighter(&self, data: &EditStack, line: usize) {
        if let Ok(tx) = BACKGROUND_TX.lock() {
            match tx.send(BackgroundWorkerMessage::UpdateBuffer(self.owner_id,
                                                                           data.file.syntax.clone(),
                                                                           data.buffer.rope.clone(),
                                                                           line,
            )) {
                Ok(()) => (),
                Err(_e) => {
                    tracing::error!("Error sending data to the background worker!");
                }
            }
        }
    }

    fn stop_background_worker(&self) {
        if let Ok(tx) = BACKGROUND_TX.lock() {
            match tx.send(BackgroundWorkerMessage::Stop(self.owner_id)) {
                Ok(()) => (),
                Err(_e) => {
                    tracing::error!("Error stopping the background worker!");
                }
            }
        }
    }

    fn handle_event(&mut self, event: &Event, ctx: &mut EventCtx, editor: &mut EditStack) -> bool {
        if let Some(handler) = self.event_handler.as_deref_mut() {
            handler(ctx, event, editor);
        }

        if ctx.is_handled() {
            return true;
        }

        match event {
            Event::KeyDown(event) => {
                match event {
                    #[cfg(windows)]
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowDown,
                        mods,
                        ..
                    } if mods.alt() && mods.ctrl() => {
                        editor.duplicate_down();
                        return true;
                    }
                    #[cfg(windows)]
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowUp,
                        mods,
                        ..
                    } if mods.alt() && mods.ctrl() => {
                        editor.duplicate_up();
                        return true;
                    }
                    #[cfg(not(windows))]
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowDown,
                        mods,
                        ..
                    } if mods.alt() && mods.shift() => {
                        editor.duplicate_down();
                        return true;
                    }
                    #[cfg(not(windows))]
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowUp,
                        mods,
                        ..
                    } if mods.alt() && mods.shift() => {
                        editor.duplicate_up();
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowRight,
                        mods,
                        ..
                    } => {
                        editor.forward(mods.shift(), mods.ctrl());
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowLeft,
                        mods,
                        ..
                    } => {
                        editor.backward(mods.shift(), mods.ctrl());
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowUp,
                        mods,
                        ..
                    } => {
                        editor.up(mods.shift());
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::ArrowDown,
                        mods,
                        ..
                    } => {
                        editor.down(mods.shift());
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::PageUp,
                        mods,
                        ..
                    } => {
                        for _ in 0..self.page_len {
                            editor.up(mods.shift());
                        }
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::PageDown,
                        mods,
                        ..
                    } => {
                        for _ in 0..self.page_len {
                            editor.down(mods.shift())
                        }
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::End,
                        mods,
                        ..
                    } => {
                        editor.end(mods.shift());
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::Home,
                        mods,
                        ..
                    } => {
                        editor.home(mods.shift());
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::Tab,
                        ..
                    } => {
                        editor.tab();
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::Escape,
                        ..
                    } => {
                        editor.cancel_mutli_carets();
                        editor.cancel_selection();
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::Backspace,
                        ..
                    } => {
                        editor.backspace();
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::Delete,
                        ..
                    } => {
                        editor.delete();
                        return true;
                    }
                    KeyEvent {
                        key: druid::keyboard_types::Key::Enter,
                        ..
                    } => {
                        editor.insert(editor.file.linefeed.to_str());
                        return true;
                    }
                    _ => (),
                }

                if let druid::keyboard_types::Key::Character(text) = event.key.clone() {
                    if event.mods.ctrl() || event.mods.alt() || event.mods.meta() {
                        return false;
                    }
                    if text.chars().count() == 1 && text.chars().next().unwrap().is_ascii_control() {
                        return false;
                    }

                    editor.insert(&text);
                    return true;
                }
                false
            }
            Event::Wheel(event) => {
                ctx.submit_command(
                    SCROLL_TO
                        .with((
                            Some(self.delta_x - event.wheel_delta.x),
                            Some(self.delta_y - event.wheel_delta.y),
                        ))
                        .to(self.owner_id),
                );

                if ctx.is_active() && event.buttons.contains(MouseButton::Left) {
                    let (x, y) = self.pix_to_point(event.pos.x, event.pos.y, ctx, editor);
                    let p = editor.point(x, y);
                    editor.move_main_caret_to(p, true, false);
                }
                ctx.request_paint();
                ctx.set_handled();
                true
            }
            Event::MouseDown(event) => {
                if matches!(event.button, MouseButton::Left) {
                    let (x, y) = self.pix_to_point(event.pos.x, event.pos.y, ctx, editor);
                    editor.cancel_mutli_carets();
                    // FIXME: Update is not called if the caret position is not modified,
                    let p = editor.point(x, y);
                    match event.count {
                        1 => {
                            editor.move_main_caret_to(p, event.mods.shift(), false);
                            self.held_state = HeldState::Grapheme;
                        }
                        2 => {
                            editor.move_main_caret_to(p, event.mods.shift(), true);
                            self.held_state = HeldState::Word;
                        }
                        3 => {
                            editor.select_line(p.line, event.mods.shift());
                            self.held_state = HeldState::Line;
                        }
                        4 => {
                            editor.select_all();
                            self.held_state = HeldState::None;
                        }
                        _ => (),
                    }
                    ctx.set_active(true);
                }
                ctx.request_focus();
                ctx.request_paint();
                ctx.set_handled();
                true
            }
            Event::MouseUp(_event) => {
                ctx.set_active(false);
                self.held_state = HeldState::None;
                true
            }
            Event::MouseMove(event) => {
                if self.held_state.is_held() && ctx.is_active() && event.buttons.contains(MouseButton::Left) {
                    let (x, y) = self.pix_to_point(event.pos.x, event.pos.y, ctx, editor);
                    let p = editor.point(x, y);
                    match self.held_state {
                        HeldState::Grapheme => editor.move_main_caret_to(p, true, false),
                        HeldState::Word => editor.move_main_caret_to(p, true, true),
                        HeldState::Line => editor.select_line(p.line, true),
                        HeldState::None => unreachable!(),
                    }
                    return true;
                }
                false
            }

            Event::Command(cmd) if cmd.is(druid::commands::SAVE_FILE_AS) => {
                let file_info = cmd.get_unchecked(druid::commands::SAVE_FILE_AS).clone();
                if file_info.path().exists() {
                    self.dialog()
                        //.items(item!["Yes", "No"])
                        .title("File exists! Overwrite?")
                        .on_select(move |result, ctx, text_editor, data| {
                            if result == DialogResult::Ok {
                                if let Err(e) = text_editor.save_as(data, file_info.path()) {
                                    text_editor.alert(&format!("Error writing file: {}", e)).show(ctx);
                                }
                            };
                        })
                        .show(ctx);
                    true
                } else {
                    if let Err(e) = self.save_as(editor, file_info.path()) {
                        self.alert(&format!("Error writing file: {}", e)).show(ctx);
                    }
                    true
                }
            }

            Event::Command(cmd) if cmd.is(druid::commands::SAVE_FILE) => {
                if let Err(e) = self.save(editor) {
                    self.alert(&format!("Error writing file: {}", e)).show(ctx);
                }
                true
            }

            Event::Command(cmd) if cmd.is(druid::commands::OPEN_FILE) => {
                if let Some(file_info) = cmd.get(druid::commands::OPEN_FILE) {
                    if let Err(_) = self.open(editor, file_info.path()) {
                        self.alert("Error loading file").show(ctx);
                    }
                }
                true
            }

            Event::Command(cmd) if cmd.is(REQUEST_NEXT_SEARCH) => {
                if let Some(data) = cmd.get(REQUEST_NEXT_SEARCH) {
                    editor.search_next(data);
                }
                true
            }
            Event::Command(cmd) if cmd.is(SELECT_LINE) => {
                let (line, expand) = *cmd.get_unchecked(SELECT_LINE);
                editor.buffer.select_line(line.into(), expand);
                true
            }
            Event::Command(cmd) if cmd.is(SCROLL_TO) => {
                let d = *cmd.get_unchecked(SCROLL_TO);
                self.delta_x = d.0.unwrap_or(self.delta_x);
                self.delta_y = d.1.unwrap_or(self.delta_y);
                true
            }
            Event::Command(cmd) if cmd.is(RESET_HELD_STATE) => {
                self.held_state = HeldState::None;
                false
            }
            Event::Command(cmd) if cmd.is(HIGHLIGHT) => {
                let d = *cmd.get_unchecked(HIGHLIGHT);

                let vr = self.visible_range();

                if vr.contains(&d.0)
                    || vr.contains(&d.1)
                    || (vr.start >= d.0 && vr.end <= d.1)
                {
                    ctx.request_paint();
                }
                true
            }

            Event::Command(cmd) if cmd.is(RELOAD_FROM_DISK) => {
                if editor.is_dirty() {
                    ctx.set_handled();
                    self.dialog()
                        //.items(item!["Yes", "No"])
                        .title("File was modified outside of NonePad\nDiscard unsaved change and reload?")
                        .on_select(|result, ctx, text_editor, data| {
                            if result == DialogResult::Ok {
                                if let Err(e) = data.reload() {
                                    text_editor
                                        .alert(&format!(
                                            "Error while reloading {}: {}",
                                            data.filename.clone().unwrap_or_default().to_string_lossy(),
                                            e
                                        ))
                                        .show(ctx);
                                } else {
                                    data.reset_dirty();
                                }
                            }
                        })
                        .show(ctx);
                } else {
                    if let Err(e) = editor.reload() {
                        self.alert(&format!(
                            "Error while reloading {}: {}",
                            editor.filename.clone().unwrap_or_default().to_string_lossy(),
                            e
                        ))
                            .show(ctx);
                    }
                }
                true
            }

            Event::Command(cmd) if cmd.is(FILE_REMOVED) => {
                editor.set_dirty();
                true
            }

            Event::Command(cmd) if cmd.is(FOCUS_EDITOR) => {
                trace!("trying to focus editor view: {:?}", ctx.widget_id());
                ctx.request_focus();
                true
            }

            Event::Command(cmd) if cmd.is(PALETTE_CALLBACK) => {
                let item = cmd.get_unchecked(PALETTE_CALLBACK);
                match &item.1 {
                    PaletteCommandType::EditorPalette(action) => {
                        (action)(item.0.clone(), ctx, self, editor);
                        true
                    }
                    PaletteCommandType::EditorDialog(action) => {
                        let dialog_result = if item.0.index == 0 {
                            DialogResult::Ok
                        } else {
                            DialogResult::Cancel
                        };
                        (action)(dialog_result, ctx, self, editor);
                        true
                    }
                    _ => false
                }
            }

            druid::Event::WindowCloseRequested => {
                if editor.is_dirty() {
                    ctx.set_handled();
                    self.dialog()
                        .title("Discard unsaved changes?")
                        .on_select(|result, ctx, _view, edit| {
                            if result == DialogResult::Ok {
                                edit.reset_dirty();
                                ctx.submit_command(druid::commands::CLOSE_WINDOW);
                            }
                        })
                        .show(ctx);

                    true
                } else {
                    false
                }
            }

            Event::WindowDisconnected => {
                self.stop_background_worker();
                false
            }

            Event::WindowConnected => {
                ctx.request_focus();
                false
            }

            _ => false,
        }
    }

    fn visible_range(&self) -> Range<usize> {
        if self.metrics.font_height > 0.0 {
            (-self.delta_y / self.metrics.font_height).max(0.0) as usize
                ..((-self.delta_y + self.size.height) / self.metrics.font_height).max(0.0) as usize + 1
        } else {
            0..1
        }
    }

    fn add_bounded_range_selection<L: TextLayout>(
        &mut self,
        y: f64,
        range: Range<position::Relative>,
        layout: &L,
        path: &mut SelectionPath,
    ) {
        let s = layout.hit_test_text_position(range.start.into());
        let e = layout.hit_test_text_position(range.end.into());

        path.clear();
        path.push(PathEl::MoveTo(Point::new(s.point.x.ceil() + 0.5, y.ceil() + 0.5)));
        path.push(PathEl::LineTo(Point::new(e.point.x.ceil() + 0.5, y.ceil() + 0.5)));
        path.push(PathEl::LineTo(Point::new(
            e.point.x.ceil() + 0.5,
            (self.metrics.font_height + y).ceil() + 0.5,
        )));
        path.push(PathEl::LineTo(Point::new(
            s.point.x.ceil() + 0.5,
            (self.metrics.font_height + y).ceil() + 0.5,
        )));
        path.push(PathEl::ClosePath);
    }

    fn add_range_from_selection<L: TextLayout>(
        &mut self,
        y: f64,
        range: Range<position::Relative>,
        layout: &L,
        path: &mut Vec<PathEl>,
    ) -> f64 {
        let s = layout.hit_test_text_position(range.start.into());
        let e = layout.hit_test_text_position(range.end.into());

        path.clear();
        path.push(PathEl::MoveTo(Point::new(
            s.point.x.ceil() + 0.5,
            (self.metrics.font_height + y).ceil() + 0.5,
        )));
        path.push(PathEl::LineTo(Point::new(s.point.x.ceil() + 0.5, y.ceil() + 0.5)));
        path.push(PathEl::LineTo(Point::new(
            (e.point.x + self.metrics.font_advance).ceil() + 0.5,
            y.ceil() + 0.5,
        )));
        path.push(PathEl::LineTo(Point::new(
            (e.point.x + self.metrics.font_advance).ceil() + 0.5,
            (self.metrics.font_height + y).ceil() + 0.5,
        )));
        s.point.x //.ceil()+0.5
    }

    fn add_range_to_selection<L: TextLayout>(
        &mut self,
        y: f64,
        range: Range<position::Relative>,
        layout: &L,
        path: &mut SelectionPath,
    ) {
        let e = layout.hit_test_text_position(range.end.into());
        match &path.last_range {
            Some(SelectionLineRange::RangeFrom(_)) if range.end == 0 => {
                path.push(PathEl::ClosePath);
            }
            Some(SelectionLineRange::RangeFull) if range.end == 0 => {
                path.push(PathEl::LineTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::ClosePath);
            }
            Some(SelectionLineRange::RangeFrom(_)) if path.last_x > e.point.x => {
                path.push(PathEl::ClosePath);
                path.push(PathEl::MoveTo(Point::new(e.point.x.ceil() + 0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(
                    e.point.x.ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::ClosePath);
            }
            Some(SelectionLineRange::RangeFrom(_)) | Some(SelectionLineRange::RangeFull) => {
                path.push(PathEl::LineTo(Point::new(e.point.x.ceil() + 0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(
                    e.point.x.ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::ClosePath);
            }
            None => {
                path.clear();
                path.push(PathEl::MoveTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(e.point.x.ceil() + 0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(
                    e.point.x.ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
                path.push(PathEl::ClosePath);
            }
            _ => {
                unreachable!();
            }
        }
    }

    fn add_range_full_selection<L: TextLayout>(
        &mut self,
        y: f64,
        range: Range<position::Relative>,
        layout: &L,
        path: &mut SelectionPath,
    ) {
        let e = layout.hit_test_text_position(range.end.into());
        match &path.last_range {
            Some(SelectionLineRange::RangeFrom(_)) if path.last_x > e.point.x => {
                path.push(PathEl::ClosePath);
                path.push(PathEl::MoveTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    y.ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
            }
            Some(SelectionLineRange::RangeFrom(_)) if path.last_x <= e.point.x => {
                // insert a point at the begining of the line
                path[0] = PathEl::LineTo(Point::new(path.last_x.ceil() + 0.5, y.ceil() + 0.5));
                path.insert(0, PathEl::MoveTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    y.ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
            }
            None => {
                // the precedent line was outside the visible range
                path.clear();
                path.push(PathEl::MoveTo(Point::new(0.5, y.ceil() + 0.5)));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    y.ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
            }
            _ => {
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    y.ceil() + 0.5,
                )));
                path.push(PathEl::LineTo(Point::new(
                    (e.point.x + self.metrics.font_advance).ceil() + 0.5,
                    (self.metrics.font_height + y).ceil() + 0.5,
                )));
            }
        }
    }

    fn paint_editor(&mut self, editor: &EditStack, ctx: &mut PaintCtx, env: &Env) -> bool {
        let font = ctx.render_ctx.text().font_family(&self.font_name).unwrap();
        let rect = Rect::new(0.0, 0.0, self.size.width, self.size.height);
        ctx.render_ctx.fill(rect, &self.bg_color);

        let clip_rect = ctx.size().to_rect().inset((1., 0., 0., 0.));
        ctx.render_ctx.clip(clip_rect);
        ctx.render_ctx
            .transform(Affine::translate((self.delta_x + EDITOR_LEFT_PADDING, 0.0)));

        let mut line = String::new();
        let mut indices = Vec::new();
        let mut ranges = Vec::new();
        let mut selection_path = Vec::new();
        let mut current_path = SelectionPath::new();

        // Draw selection first
        // TODO: cache layout to reuse it when we will draw the text
        let mut dy = (self.delta_y / self.metrics.font_height).fract() * self.metrics.font_height;
        for line_idx in self.visible_range() {
            editor.displayable_line(position::Line::from(line_idx), &mut line, &mut indices, &mut Vec::new());
            let layout = ctx
                .render_ctx
                .text()
                .new_text_layout(line.clone()) // TODO: comment ne pas faire de clone?
                .default_attribute(TextAttribute::Weight(FONT_WEIGTH))
                .font(font.clone(), self.metrics.font_size)
                .build()
                .unwrap();

            editor.selection_on_line(line_idx, &mut ranges);

            for range in &ranges {
                match range {
                    SelectionLineRange::Range(r) => {
                        // Simple case, the selection is contain on one line
                        self.add_bounded_range_selection(
                            dy,
                            indices[r.start]..indices[r.end],
                            &layout,
                            &mut current_path,
                        )
                    }
                    SelectionLineRange::RangeFrom(r) => {
                        current_path.last_x = self.add_range_from_selection(
                            dy,
                            indices[r.start]..position::Relative::from(line.len() - 1),
                            &layout,
                            &mut current_path,
                        )
                    }
                    SelectionLineRange::RangeTo(r) => self.add_range_to_selection(
                        dy,
                        position::Relative::from(0)..indices[r.end],
                        &layout,
                        &mut current_path,
                    ),
                    SelectionLineRange::RangeFull => self.add_range_full_selection(
                        dy,
                        position::Relative::from(0)..position::Relative::from(line.len() - 1),
                        &layout,
                        &mut current_path,
                    ),
                }
                current_path.last_range = Some(range.clone());
                if let Some(PathEl::ClosePath) = current_path.last() {
                    selection_path.push(std::mem::take(&mut current_path));
                }
            }

            dy += self.metrics.font_height;
        }

        // if path is unclosed, it can only be because the lastest visible line was a RangeFull
        // We need to close it
        match current_path.last() {
            Some(PathEl::ClosePath) => (),
            _ => {
                current_path.push(PathEl::LineTo(Point::new(0.5, dy.ceil() + 0.5)));
                current_path.push(PathEl::ClosePath);
                selection_path.push(std::mem::take(&mut current_path));
            }
        }

        for path in selection_path {
            let path = BezPath::from_vec(path.elem);
            let brush = ctx.render_ctx.solid_brush(self.fg_sel_color.clone());
            ctx.render_ctx.fill(&path, &brush);
            let brush = ctx.render_ctx.solid_brush(self.bg_sel_color.clone());
            ctx.render_ctx.stroke(&path, &brush, 1.);
        }

        let mut dy = (self.delta_y / self.metrics.font_height).fract() * self.metrics.font_height;
        for line_idx in self.visible_range() {
            editor.displayable_line(position::Line::from(line_idx), &mut line, &mut indices, &mut Vec::new());
            let mut layout = ctx
                .render_ctx
                .text()
                .new_text_layout(line.clone())
                .default_attribute(TextAttribute::Weight(FONT_WEIGTH))
                .font(font.clone(), self.metrics.font_size)
                .text_color(self.fg_color.clone());
            if line_idx < editor.len_lines() {
                if let Some(highlight) = self.highlighted_line.lines.lock().unwrap().get(line_idx) {
                    for h in highlight.iter() {
                        let color = TextAttribute::TextColor(Color::rgba8(
                            h.style.foreground.r,
                            h.style.foreground.g,
                            h.style.foreground.b,
                            h.style.foreground.a,
                        ));
                        let start = indices.get(h.range.start);
                        let end = indices.get(h.range.end);
                        if start.is_some() && end.is_some() {
                            if h.style.font_style.contains(syntect::highlighting::FontStyle::ITALIC) {
                                layout = layout.range_attribute(
                                    start.unwrap().index..end.unwrap().index,
                                    TextAttribute::Style(FontStyle::Italic),
                                );
                            }
                            layout = layout.range_attribute(start.unwrap().index..end.unwrap().index, color);
                        }
                    }
                }
            }
            let layout = layout.build().unwrap();

            ctx.render_ctx.draw_text(&layout, (0.0, dy));

            self.longest_line_len = self.longest_line_len.max(layout.image_bounds().width());

            if ctx.has_focus() {
                editor.carets_on_line(position::Line::from(line_idx)).for_each(|c| {
                    let metrics = layout.hit_test_text_position(indices[c.relative().index].index);
                    ctx.render_ctx.stroke(
                        Line::new(
                            (metrics.point.x.ceil(), (self.metrics.font_height + dy).ceil()),
                            (metrics.point.x.ceil(), dy.ceil()),
                        ),
                        &env.get(crate::theme::EDITOR_CURSOR_FOREGROUND),
                        2.0,
                    );
                });
            }

            dy += self.metrics.font_height;
        }

        false
    }

    fn pix_to_point(&self, x: f64, y: f64, ctx: &mut EventCtx, editor: &EditStack) -> (usize, usize) {
        let x = (x - self.delta_x - EDITOR_LEFT_PADDING).max(0.);
        let y = (y - self.delta_y).max(0.);
        let line = ((y / self.metrics.font_height) as usize).min(editor.len_lines() - 1);

        let mut buf = String::new();
        let mut i = Vec::new();
        editor.displayable_line(line.into(), &mut buf, &mut Vec::new(), &mut i);

        let layout = self.text_layout(ctx.text(), buf);
        let rel = rope_utils::relative_to_column(
            i[layout.hit_test_point((x, 0.0).into()).idx],
            line.into(),
            &editor.buffer,
        )
            .index;

        (rel, line)
    }

    fn text_layout(&self, text: &mut PietText, buf: String) -> impl druid::piet::TextLayout {
        let font = text.font_family(&self.font_name).unwrap();
        text.new_text_layout(buf)
            .default_attribute(TextAttribute::Weight(FONT_WEIGTH))
            .font(font, self.metrics.font_size)
            .build()
            .unwrap()
    }

    fn put_caret_in_visible_range(&mut self, ctx: &mut EventCtx, editor: &EditStack) {
        if editor.has_many_carets() {
            return;
        }
        let caret = editor.main_caret();
        let y = caret.line().index as f64 * self.metrics.font_height;

        if y > -self.delta_y + self.size.height - self.metrics.font_height {
            self.delta_y = -y + self.size.height - self.metrics.font_height;
        }
        if y < -self.delta_y {
            self.delta_y = -y;
        }

        let mut buf = String::new();
        let mut i = Vec::new();
        editor.displayable_line(caret.line(), &mut buf, &mut i, &mut Vec::new());

        let layout = self.text_layout(ctx.text(), buf);

        let hit = layout.hit_test_text_position(i[caret.relative().index].index);
        let x = hit.point.x;
        if x > -self.delta_x + self.size.width - self.metrics.font_advance - EDITOR_LEFT_PADDING {
            self.delta_x = -x + self.size.width - self.metrics.font_advance - EDITOR_LEFT_PADDING;
        }
        if x < -self.delta_x {
            self.delta_x = -x;
        }

        ctx.submit_command(
            SCROLL_TO
                .with((Some(self.delta_x), Some(self.delta_y)))
                .to(self.owner_id),
        );
    }

    pub fn navigate_to_line(&mut self, ctx: &mut EventCtx, editor: &mut EditStack, line: position::Line) {
        if line.index < editor.len_lines() {
            let start = line.start(&editor.buffer);
            editor.cancel_mutli_carets();
            editor.move_main_caret_to(start, false, false);
            self.put_caret_in_visible_range(ctx, editor);
        }
    }

    pub fn save_as<P>(&mut self, editor: &mut EditStack, filename: P) -> anyhow::Result<()>
        where
            P: AsRef<Path>,
    {
        editor.save(&filename)?;
        editor.filename = Some(filename.as_ref().to_path_buf());
        self.update_highlighter(editor, 0);
        Ok(())
    }

    pub fn save(&mut self, editor: &mut EditStack) -> anyhow::Result<()> {
        anyhow::ensure!(editor.filename.is_some(), "editor.filename must not be None");
        editor.save(editor.filename.clone().as_ref().unwrap())?;
        self.update_highlighter(editor, 0);
        Ok(())
    }

    pub fn open<P>(&mut self, editor: &mut EditStack, filename: P) -> anyhow::Result<()>
        where
            P: AsRef<Path>,
    {
        editor.open(filename)?;
        self.update_highlighter(editor, 0);

        Ok(())
    }
}