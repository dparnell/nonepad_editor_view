use druid::{BoxConstraints, Env, Event, EventCtx, FontWeight, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx, Selector, Size, UpdateCtx, Widget, WidgetExt, WidgetId};
use druid::widget::Flex;
use super::text_buffer::{EditStack};

mod editor_view;
mod gutter;
mod scroll_bar;

use crate::text_editor::editor_view::{CommonMetrics, EditorEventHandler, EditorView};
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

pub struct TextEditor {
    gutter_id: WidgetId,
    editor_id: WidgetId,
    vscroll_id: WidgetId,
    hscroll_id: WidgetId,
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

    fn new(editor_event_handler: Option<EditorEventHandler>) -> Self {
        let id = WidgetId::next();
        let gutter_id = WidgetId::next();
        let editor_id = WidgetId::next();
        let vscroll_id = WidgetId::next();
        let hscroll_id = WidgetId::next();

        TextEditor {
            gutter_id,
            editor_id,
            vscroll_id,
            hscroll_id,
            id,
            inner: Flex::row()
                .with_child(Gutter::new(id).with_id(gutter_id))
                .with_flex_child(
                    Flex::column()
                        .with_flex_child(EditorView::new(id, editor_event_handler).with_id(editor_id), 1.0)
                        .with_child(ScrollBar::new(id, ScrollBarDirection::Horizontal).with_id(hscroll_id)),
                    1.0,
                )
                .must_fill_main_axis(true)
                .with_child(
                    Flex::column()
                        .with_flex_child(
                            ScrollBar::new(id, ScrollBarDirection::Vertical).with_id(vscroll_id),
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
            Event::Command(cmd) if cmd.is(SCROLL_TO) => {
                // clamp to size
                let d = *cmd.get_unchecked(SCROLL_TO);
                let x = d.0.map(|x| x.clamp(-self.text_width(&data), 0.0));
                let y = d.1.map(|y| y.clamp(-self.text_height(&data), 0.0));
                //dbg!(x,y);
                ctx.submit_command(SCROLL_TO.with((x, y)).to(self.editor_id));
                ctx.submit_command(SCROLL_TO.with((x, y)).to(self.gutter_id));
                ctx.submit_command(SCROLL_TO.with((x, y)).to(self.vscroll_id));
                ctx.submit_command(SCROLL_TO.with((x, y)).to(self.hscroll_id));
                ctx.is_handled();
            }

            _ => self.inner.event(ctx, event, data, &new_env),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &EditStack, env: &Env) {
        let mut new_env = env.clone();
        self.metrics.to_env(&mut new_env);

        self.inner.lifecycle(ctx, event, data, &new_env)
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

pub fn new(editor_event_handler: Option<EditorEventHandler>) -> impl Widget<EditStack> {
    let t = TextEditor::new(editor_event_handler);
    let id = t.id;
    t.with_id(id)
}
