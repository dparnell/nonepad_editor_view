use druid::{BoxConstraints, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, MouseButton, PaintCtx, RenderContext, Size, UpdateCtx, Widget, WidgetId};
use crate::text_buffer::EditStack;
use crate::text_editor::{FONT_NAME, FONT_WEIGTH, RESET_HELD_STATE, SCROLL_TO, SELECT_LINE};
use std::ops::Range;
use druid::kurbo::Line;
use druid::piet::{Text, TextAttribute, TextLayoutBuilder};
use crate::text_editor::editor_view::CommonMetrics;

#[derive(Debug)]
pub struct Gutter {
    metrics: CommonMetrics,
    page_len: usize,
    size: Size,
    dy: f64,
    owner_id: WidgetId,
    is_held: bool,
}

impl Widget<EditStack> for Gutter {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditStack, _env: &Env) {
        match event {
            Event::Command(cmd) if cmd.is(SCROLL_TO) => {
                self.dy = cmd.get_unchecked(SCROLL_TO).1.unwrap_or(self.dy);
                ctx.request_paint();
                ctx.set_handled();
            }
            Event::Command(cmd) if cmd.is(RESET_HELD_STATE) => {
                self.is_held = false;
            }
            Event::MouseMove(m) => {
                if self.is_held && ctx.is_active() && m.buttons.contains(MouseButton::Left) {
                    let y = (m.pos.y - self.dy).max(0.);
                    let line = ((y / self.metrics.font_height) as usize).min(data.len_lines() - 1);
                    ctx.submit_command(SELECT_LINE.with((line, true)).to(self.owner_id));
                    ctx.request_paint();
                    ctx.set_handled();
                }
            }
            Event::MouseDown(m) => {
                ctx.set_active(true);
                let y = (m.pos.y - self.dy).max(0.);
                let line = ((y / self.metrics.font_height) as usize).min(data.len_lines() - 1);
                ctx.submit_command(SELECT_LINE.with((line, m.mods.shift())).to(self.owner_id));
                ctx.request_paint();
                ctx.set_handled();
                self.is_held = true;
            }
            Event::MouseUp(_event) => {
                ctx.set_active(false);
                ctx.set_handled();
                self.is_held = false;
            }
            Event::Wheel(m) => {
                //self.dy -= m.wheel_delta.y;
                ctx.submit_command(
                    SCROLL_TO
                        .with((None, Some(self.dy - m.wheel_delta.y)))
                        .to(self.owner_id),
                );
                ctx.request_paint();
                ctx.set_handled();
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, _ctx: &mut LifeCycleCtx, _event: &LifeCycle, _data: &EditStack, _env: &Env) {}

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &EditStack, data: &EditStack, _env: &Env) {
        if old_data.len_lines() != data.len_lines() {
            ctx.request_layout();
        }
    }

    fn layout(&mut self, _layout_ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &EditStack, _env: &Env) -> Size {
        self.metrics = CommonMetrics::from_env(_env);
        self.size = Size::new(self.width(data), bc.max().height);

        self.page_len = (self.size.height / self.metrics.font_height).round() as usize;
        self.size
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &EditStack, env: &Env) {
        self.paint_gutter(data, ctx, env)
    }
}

impl Gutter {
    pub(crate) fn new(owner_id: WidgetId) -> Self {
        Gutter {
            metrics: Default::default(),
            page_len: 0,
            size: Default::default(),
            dy: 0.0,
            owner_id,
            is_held: false,
        }
    }
    fn visible_line_range(&self) -> Range<usize> {
        let start = -(self.dy / self.metrics.font_height) as usize;
        let end = start + self.page_len + 1;
        Range { start, end }
    }

    fn width(&self, data: &EditStack) -> f64 {
        (data.len_lines().to_string().chars().count() as f64 + 3.0) * self.metrics.font_advance
    }
    fn paint_gutter(&mut self, editor: &EditStack, ctx: &mut PaintCtx, env: &Env) {
        ctx.clip(self.size.to_rect());
        ctx.render_ctx
            .fill(self.size.to_rect(), &env.get(crate::theme::EDITOR_BACKGROUND));
        // Draw line number
        let font = ctx.text().font_family(FONT_NAME).unwrap();
        let mut dy = (self.dy / self.metrics.font_height).fract() * self.metrics.font_height;
        let line_number_char_width = format!(" {}", editor.len_lines()).len();
        for line_idx in self.visible_line_range() {
            if line_idx >= editor.len_lines() {
                break;
            }
            let layout = ctx
                .render_ctx
                .text()
                .new_text_layout(format!("{:1$}", line_idx, line_number_char_width))
                .default_attribute(TextAttribute::Weight(FONT_WEIGTH))
                .font(font.clone(), self.metrics.font_size)
                .text_color(env.get(crate::theme::EDITOR_LINE_NUMBER_FOREGROUND))
                .build()
                .unwrap();
            ctx.render_ctx.draw_text(&layout, (0.0, dy));
            dy += self.metrics.font_height;
        }

        ctx.render_ctx.stroke(
            Line::new(
                ((self.width(editor) - self.metrics.font_advance).ceil() + 0.5, 0.),
                (
                    (self.width(editor) - self.metrics.font_advance).ceil() + 0.5,
                    self.size.height,
                ),
            ),
            &env.get(crate::theme::EDITOR_LINE_NUMBER_FOREGROUND),
            1.0,
        );
    }
}