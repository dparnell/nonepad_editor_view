use std::cell::RefCell;
use std::rc::Rc;
use druid::{BoxConstraints, Data, Lens, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx, Size, UpdateCtx, Widget, WidgetPod, WidgetId, Selector, Command};
use druid::im::Vector;
use crate::text_editor::editor_view::{EDITOR_PALETTE_CALLBACK, EditorKeyBindings};
use crate::text_editor::palette_view::{CLOSE_PALETTE, DialogResult, Item, Palette, PALETTE_CALLBACK, PaletteBuilder, PaletteCommandType, PaletteResult, PaletteView, PaletteViewState, SHOW_DIALOG_FOR_EDITOR, SHOW_PALETTE_FOR_EDITOR, ShowPalette};
use crate::text_editor::RESET_HELD_STATE;

pub(crate) const SHOW_PALETTE: Selector = Selector::new("nonepad.palette.show_for_context");
const PALETTE_RESULT: Selector<PaletteResult> = Selector::new("nonepad.palette.result");

pub trait IsDirty {
    fn is_dirty(&self) -> bool;
    fn reset_dirty(&mut self);
}

const FOCUS_PALETTE: Selector<()> = Selector::new("nonepad.palette.focus");
pub(crate) const FOCUSED_EDITOR: Selector<WidgetId> = Selector::new("nonepad.palette.focused_editor");

pub struct PaletteManager<State: druid::Data + IsDirty> {
    inner: Box<WidgetPod<State, Box<dyn Widget<State>>>>,
    palette: WidgetPod<PaletteViewState, PaletteView>,
    key_bindings: Option<Rc<RefCell<EditorKeyBindings>>>
}

#[derive(Clone, Lens)]
pub struct PaletteManagerState<State: druid::Data + IsDirty> {
    in_palette: bool,
    editor: Option<WidgetId>,
    palette_for: Option<WidgetId>,
    inner_state: Box<State>,
    event_callback: Option<Rc<dyn Fn(&mut EventCtx, &Event, &mut State)>>,
    palette_state: PaletteViewState
}

impl<State: druid::Data + IsDirty> Data for PaletteManagerState<State> {
    fn same(&self, other: &Self) -> bool {
        other.in_palette == self.in_palette && other.palette_state.same(&self.palette_state) && other.inner_state.same(&self.inner_state)
    }
}

impl<State: druid::Data + IsDirty> PaletteManager<State> {
    pub fn build(inner: Box<dyn Widget<State>>, state: State, event_callback: Option<Rc<dyn Fn(&mut EventCtx, &Event, &mut State)>>, key_bindings: Option<Rc<RefCell<EditorKeyBindings>>>) -> (Self, PaletteManagerState<State>){
        let widget = PaletteManager {
            inner: Box::new(WidgetPod::new(inner)),
            palette: WidgetPod::new(PaletteView::new()),
            key_bindings
        };

        let widget_state = PaletteManagerState {
            in_palette: false,
            editor: None,
            palette_for: None,
            inner_state: Box::new(state),
            event_callback,
            palette_state: PaletteViewState::default()
        };

        (widget, widget_state)
    }
}

const EDITOR_OFFSET: u64 = 0x80000000;

impl<State: druid::Data + IsDirty> Widget<PaletteManagerState<State>> for PaletteManager<State> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut PaletteManagerState<State>, env: &Env) {
        if let Some(callback) = &data.event_callback {
            (callback)(ctx, event, &mut *data.inner_state);
        }
        if ctx.is_handled() {
            return;
        }

        if let Some(key_bindings) = &self.key_bindings {
            let bindings = key_bindings.borrow_mut();

            match event {
                Event::KeyDown(event) => {
                    for cmd in &bindings.window_commands {
                        if cmd.matches(event) {
                            cmd.exec(ctx);
                            if ctx.is_handled() {
                                return;
                            }
                        }
                    }
                }

                _ => ()
            }
        }

        match event {
            druid::Event::MouseUp(_) => ctx.submit_command(RESET_HELD_STATE),

            druid::Event::Command(cmd) if cmd.is(PALETTE_CALLBACK) => {
                let item = cmd.get_unchecked(PALETTE_CALLBACK);
                match &item.1 {
                    PaletteCommandType::WindowPalette(action) => {
                        (action)(item.0.clone(), ctx);
                        ctx.set_handled();
                        return;
                    }
                    PaletteCommandType::WindowDialog(action) => {
                        let dialog_result = if item.0.index == 0 {
                            DialogResult::Ok
                        } else {
                            DialogResult::Cancel
                        };
                        (action)(dialog_result, ctx);
                        ctx.set_handled();
                        return;
                    }
                    _ => ()
                }
            }

            druid::Event::Command(cmd) if cmd.is(SHOW_PALETTE) => {
                // show the command palette
                if let Some(key_bindings) = &self.key_bindings {
                    let mut items = Vector::new();
                    let bindings = key_bindings.borrow();
                    for (idx, c) in bindings.window_commands.iter().enumerate() {
                        let hotkey = c.shortcut.clone().map(|s| s.to_string());
                        items.push_back(Item::new(c.description.as_str(), "", hotkey, idx as u64))
                    }

                    for (idx, c) in bindings.editor_commands.iter().enumerate() {
                        if let Some(desc) = &c.description {
                            let hotkey = c.shortcut.clone().map(|s| s.to_string());
                            items.push_back(Item::new(desc.as_str(), "", hotkey, EDITOR_OFFSET + idx as u64))
                        }
                    }

                    items.sort();

                    if !items.is_empty() {
                        self.palette()
                            .items(items)
                            .on_action(|result, ctx| {
                                ctx.submit_command(PALETTE_RESULT.with(result).to(ctx.widget_id()));
                            })
                            .show(ctx);
                    }
                }

                ctx.set_handled();
                return;
            }
            druid::Event::Command(cmd) if cmd.is(PALETTE_RESULT) => {
                let result = cmd.get_unchecked(PALETTE_RESULT);
                if let Some(key_bindings) = &self.key_bindings {

                    let bindings = key_bindings.borrow();
                    if result.tag < EDITOR_OFFSET {
                        // it is a window command
                        if let Some(cmd) = bindings.window_commands.get(result.tag as usize) {
                            ctx.submit_command(Command::from(cmd.selector));
                        }
                    } else {
                        // it is an editor command
                        if let Some(cmd) = bindings.editor_commands.get((result.tag - EDITOR_OFFSET) as usize) {
                            if let Some(editor) = data.editor {
                                //println!("Sending EDITOR_PALETTE_CALLBACK to {editor:?}");
                                ctx.submit_command(EDITOR_PALETTE_CALLBACK.with((cmd.tag, cmd.exec)).to(editor));
                            }
                        }
                    }
                }
            }
            druid::Event::Command(cmd) if cmd.is(FOCUS_PALETTE) => {
                self.palette.widget_mut().take_focus(ctx);
            }

            druid::Event::Command(cmd) if cmd.is(FOCUSED_EDITOR) => {
                let editor = cmd.get_unchecked(FOCUSED_EDITOR);
                //println!("Focused editor: {editor:?}");

                data.editor = Some(editor.clone());
                ctx.set_handled();
                return;
            }

            druid::Event::Command(cmd) if cmd.is(SHOW_DIALOG_FOR_EDITOR) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_DIALOG_FOR_EDITOR).clone();
                data.palette_for = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::EditorDialog(widget, f)),
                );

                ctx.submit_command(Command::from(FOCUS_PALETTE));
                return;
            },

            druid::Event::Command(cmd) if cmd.is(SHOW_PALETTE_FOR_EDITOR) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_PALETTE_FOR_EDITOR).clone();
                data.palette_for = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::EditorPalette(widget, f)),
                );

                ctx.submit_command(Command::from(FOCUS_PALETTE));
                return;
            },

            druid::Event::Command(cmd) if cmd.is(SHOW_DIALOG_FOR_WINDOW) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_DIALOG_FOR_WINDOW).clone();
                data.palette_for = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::WindowDialog(f)),
                );

                ctx.submit_command(Command::from(FOCUS_PALETTE));
                return;
            },

            druid::Event::Command(cmd) if cmd.is(SHOW_PALETTE_FOR_WINDOW) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_PALETTE_FOR_WINDOW).clone();
                data.palette_for = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::WindowPalette(f)),
                );

                ctx.submit_command(Command::from(FOCUS_PALETTE));
                return;
            },

            druid::Event::Command(cmd) if cmd.is(CLOSE_PALETTE) => {
                if let Some(editor) = data.editor {
                    ctx.set_focus(editor);
                } else {
                    ctx.focus_prev();
                }
                data.in_palette = false;
                data.palette_for = None;
                ctx.request_paint();
                return;
            },

            _ => ()
        }

        if data.in_palette {
            self.palette.event(ctx, event, &mut data.palette_state, env);
        } else {
            self.inner.event(ctx, event, &mut data.inner_state, env);
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &PaletteManagerState<State>, env: &Env) {
        if event.should_propagate_to_hidden() {
            self.palette.lifecycle(ctx, event, &data.palette_state, env);
            self.inner.lifecycle(ctx, event, &data.inner_state, env);
        } else {
            if data.in_palette {
                self.palette.lifecycle(ctx, event, &data.palette_state, env);
            }
            self.inner.lifecycle(ctx, event, &data.inner_state, env);
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &PaletteManagerState<State>, data: &PaletteManagerState<State>, env: &Env) {
        if old_data.in_palette != data.in_palette {
            ctx.children_changed();
        }
        self.inner.update(ctx, &data.inner_state, env);
        if data.in_palette {
            self.palette.update(ctx, &data.palette_state, env)
        }
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &PaletteManagerState<State>, env: &Env) -> Size {
        if data.in_palette {
            self.inner.layout(ctx, bc, &data.inner_state, env);
            self.palette.layout(ctx, bc, &data.palette_state, env)
        } else {
            self.inner.layout(ctx, bc, &data.inner_state, env)
        }    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &PaletteManagerState<State>, env: &Env) {
        self.inner.paint(ctx, &data.inner_state, env);
        if data.in_palette {
            self.palette.paint(ctx, &data.palette_state, env);
        }
    }
}

impl<State: druid::Data + IsDirty> PaletteBuilder<PaletteManagerState<State>> for PaletteManager<State> {}

impl<State: druid::Data + IsDirty> Palette<PaletteResult, PaletteManager<State>, PaletteManagerState<State>> {
    pub fn show(self, ctx: &mut EventCtx) {
        ctx.show_palette(self.title.unwrap_or_default(), self.items, self.action);
    }
}

impl<State: druid::Data + IsDirty> Palette<DialogResult, PaletteManager<State>, PaletteManagerState<State>> {
    pub fn show(self, ctx: &mut EventCtx) {
        ctx.show_palette(self.title.unwrap_or_default(), self.items, self.action);
    }
}

pub(super) const SHOW_DIALOG_FOR_WINDOW: Selector<(
    WidgetId,
    String,
    Option<Vector<Item>>,
    Option<Rc<dyn Fn(DialogResult, &mut EventCtx)>>,
)> = Selector::new("nonepad.dialog.show_for_window");

pub(super) const SHOW_PALETTE_FOR_WINDOW: Selector<(
    WidgetId,
    String,
    Option<Vector<Item>>,
    Option<Rc<dyn Fn(PaletteResult, &mut EventCtx)>>,
)> = Selector::new("nonepad.palette.show_for_window");

impl<'a, 'b, 'c> ShowPalette<DialogResult> for EventCtx<'b, 'c> {
    fn show_palette(
        &mut self,
        title: String,
        items: Option<Vector<Item>>,
        callback: Option<Rc<dyn Fn(DialogResult, &mut EventCtx)>>,
    ) {
        self.submit_command(SHOW_DIALOG_FOR_WINDOW.with((self.widget_id(), title, items, callback)));
    }
}

impl<'a, 'b, 'c> ShowPalette<PaletteResult> for EventCtx<'b, 'c> {
    fn show_palette(
        &mut self,
        title: String,
        items: Option<Vector<Item>>,
        callback: Option<Rc<dyn Fn(PaletteResult, &mut EventCtx)>>,
    ) {
        self.submit_command(SHOW_PALETTE_FOR_WINDOW.with((self.widget_id(), title, items, callback)));
    }
}