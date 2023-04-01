use std::cell::RefCell;
use std::rc::Rc;
use druid::{BoxConstraints, Env, Event, EventCtx, FontWeight, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx, Selector, Size, UpdateCtx, Widget, WidgetExt, WidgetId};
use druid::widget::Flex;
use tracing::trace;
use super::text_buffer::{EditStack};

pub mod editor_view;
pub mod gutter;
pub mod palette_manager;
pub mod palette_view;
pub mod scroll_bar;

use crate::text_editor::editor_view::{CommonMetrics, EditorEventHandler, EditorKeyBindings, EditorView};
use crate::text_editor::gutter::Gutter;
use crate::text_editor::scroll_bar::{ScrollBar, ScrollBarDirection, ScrollBarSpacer};

mod env {
    use druid::Key;

    pub const FONT_SIZE: Key<f64> = Key::new("nonepad.editor.font_height");
    pub const FONT_ADVANCE: Key<f64> = Key::new("nonepad.editor.font_advance");
    pub const FONT_BASELINE: Key<f64> = Key::new("nonepad.editor.font_baseline");
    pub const FONT_DESCENT: Key<f64> = Key::new("nonepad.editor.font_descent");
    pub const FONT_HEIGHT: Key<f64> = Key::new("nonepad.editor.fonth_height");
    pub const PAGE_LEN: Key<u64> = Key::new("nonepad.editor.page_len");
}
#[cfg(windows)]
pub const FONT_NAME: &str = "Consolas";
#[cfg(target_os = "linux")]
pub const FONT_NAME: &str = "DejaVu Sans Mono";
#[cfg(target_os = "macos")]
pub const FONT_NAME: &str = "Menlo";
pub const FONT_SIZE: f64 = 14.;
pub const FONT_WEIGTH: FontWeight = FontWeight::SEMI_BOLD;
pub const EDITOR_LEFT_PADDING: f64 = 2.;
pub const SCROLLBAR_X_PADDING: f64 = 2.;
pub const REQUEST_NEXT_SEARCH: Selector<String> = Selector::new("nonepad.editor.request_next_search");
pub const RESET_HELD_STATE: Selector<()> = Selector::new("nonepad.all.reste_held_state");
pub const SCROLL_TO: Selector<(Option<f64>, Option<f64>)> = Selector::new("nonepad.editor.scroll_to_rect");
pub const SELECT_LINE: Selector<(usize, bool)> = Selector::new("nonepad.editor.select_line");
pub const HIGHLIGHT: Selector<(usize, usize)> = Selector::new("nonepad.editor.highlight");
pub const RELOAD_FROM_DISK: Selector<()> = Selector::new("nonepad.editor.reload_from_disk");
pub const FILE_REMOVED: Selector<()> = Selector::new("nonepad.editor.file_removed");
pub const SET_EDITOR_EVENT_HANDLER: Selector<Option<EditorEventHandler>> = Selector::new("nonepad.editor.event_handler");
pub const FOCUS_EDITOR: Selector<()> = Selector::new("nonepad.editor.focus_editor");

pub(crate) enum ChildWidget {
    Editor(WidgetId),
    Gutter(WidgetId),
    VScroll(WidgetId),
    HScroll(WidgetId)
}
pub(crate) const WIDGET_ATTACHED: Selector<ChildWidget> = Selector::new("nonepad.editor.widget_attached");

pub struct TextEditor {
    gutter_id: Option<WidgetId>,
    editor_id: Option<WidgetId>,
    vscroll_id: Option<WidgetId>,
    hscroll_id: Option<WidgetId>,
    id: WidgetId,
    inner: Flex<EditStack>,
    metrics: CommonMetrics,
}

impl TextEditor {
    pub fn text_height(&self, data: &EditStack) -> f64 {
        data.len_lines().saturating_sub(3) as f64 * self.metrics.font_height
    }
    pub fn text_width(&self, data: &EditStack) -> f64 {
        data.buffer.max_visible_line_grapheme_len().saturating_sub(3) as f64 * self.metrics.font_advance
    }

    fn new(key_bindings: Rc<RefCell<EditorKeyBindings>>) -> Self {
        let id = WidgetId::next();

        TextEditor {
            gutter_id: None,
            editor_id: None,
            vscroll_id: None,
            hscroll_id: None,
            id,
            inner: Flex::row()
                .with_child(Gutter::new(id))
                .with_flex_child(
                    Flex::column()
                        .with_flex_child(EditorView::new(id, key_bindings), 1.0)
                        .with_child(ScrollBar::new(id, ScrollBarDirection::Horizontal)),
                    1.0,
                )
                .must_fill_main_axis(true)
                .with_child(
                    Flex::column()
                        .with_flex_child(
                            ScrollBar::new(id, ScrollBarDirection::Vertical),
                            1.0,
                        )
                        .with_child(ScrollBarSpacer::default()),
                ),
            metrics: Default::default(),
        }
    }
}

impl Widget<EditStack> for TextEditor {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditStack, env: &Env) {
        let mut new_env = env.clone();
        self.metrics.to_env(&mut new_env);
        match event {
            Event::Command(cmd) if cmd.is(WIDGET_ATTACHED) => {
                let child = cmd.get_unchecked(WIDGET_ATTACHED);

                match child {
                    ChildWidget::Editor(id) => self.editor_id = Some(id.clone()),
                    ChildWidget::Gutter(id) => self.gutter_id = Some(id.clone()),
                    ChildWidget::VScroll(id) => self.vscroll_id = Some(id.clone()),
                    ChildWidget::HScroll(id) => self.hscroll_id = Some(id.clone())
                }
            }

            Event::Command(cmd) if cmd.is(SCROLL_TO) => {
                // clamp to size
                let d = *cmd.get_unchecked(SCROLL_TO);
                let x = d.0.map(|x| x.clamp(-self.text_width(&data), 0.0));
                let y = d.1.map(|y| y.clamp(-self.text_height(&data), 0.0));
                //dbg!(x,y);
                if let Some(id) = self.editor_id {
                    ctx.submit_command(SCROLL_TO.with((x, y)).to(id));
                }
                if let Some(id) = self.gutter_id {
                    ctx.submit_command(SCROLL_TO.with((x, y)).to(id));
                }
                if let Some(id) = self.vscroll_id {
                    ctx.submit_command(SCROLL_TO.with((x, y)).to(id));
                }
                if let Some(id) = self.hscroll_id {
                    ctx.submit_command(SCROLL_TO.with((x, y)).to(id));
                }
                ctx.is_handled();
            }

            Event::Command(cmd) if cmd.is(FOCUS_EDITOR) => {
                trace!("sending focus request to {:?}", self.editor_id);
                EditorView::focus_editor(self.id);
            }

            _ => self.inner.event(ctx, event, data, &new_env),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditStack, env: &Env) {
        let mut new_env = env.clone();
        self.metrics.to_env(&mut new_env);

        match event {
           LifeCycle::FocusChanged(true) => {
               trace!("got focus: {:?}", ctx.widget_id());
               ctx.submit_command(druid::Command::new(FOCUS_EDITOR, (), ctx.widget_id()));
           },
            _ => self.inner.lifecycle(ctx, event, data, &new_env)
        }
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditStack, data: &EditStack, env: &Env) {
        let mut new_env = env.clone();
        self.metrics.to_env(&mut new_env);
        self.inner.update(ctx, old_data, data, &new_env)
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &EditStack, env: &Env) -> Size {
        self.metrics = CommonMetrics::new(ctx.text(), FONT_NAME, bc.max());
        let mut new_env = env.clone();
        self.metrics.to_env(&mut new_env);
        self.inner.layout(ctx, bc, data, &new_env)
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditStack, env: &Env) {
        let mut new_env = env.clone();
        self.metrics.to_env(&mut new_env);
        self.inner.paint(ctx, data, &new_env)
    }
}

pub fn new(key_bindings: Rc<RefCell<EditorKeyBindings>>) -> impl Widget<EditStack> {
    let t = TextEditor::new(key_bindings);
    let id = t.id;
    t.with_id(id)
}
