use std::ops::Range;
use druid::{BoxConstraints, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, MouseButton, PaintCtx, Point, Rect, RenderContext, Size, UpdateCtx, Widget, WidgetId};
use crate::text_buffer::EditStack;
use crate::text_editor::{ChildWidget, RESET_HELD_STATE, SCROLL_TO, SCROLLBAR_X_PADDING, WIDGET_ATTACHED};
use crate::text_editor::editor_view::CommonMetrics;

#[derive(Debug, PartialEq, Eq)]
pub enum ScrollBarDirection {
    Horizontal,
    Vertical,
}

#[derive(Debug)]
pub struct ScrollBar {
    owner_id: WidgetId,
    direction: ScrollBarDirection,
    len: f64,
    range: Range<f64>,
    metrics: CommonMetrics,
    mouse_delta: f64,
    is_held: bool,
    is_hovered: bool,
    delta: f64,
}

impl ScrollBar {
    pub(crate) fn new(owner_id: WidgetId, direction: ScrollBarDirection) -> Self {
        Self {
            owner_id,
            direction,
            len: 0.,
            range: Range { start: 0., end: 0. },
            metrics: Default::default(),
            mouse_delta: 0.,
            is_held: false,
            is_hovered: false,
            delta: 0.,
        }
    }

    fn text_len(&self, data: &EditStack) -> f64 {
        if self.is_vertical() {
            data.len_lines().saturating_sub(3) as f64 * self.metrics.font_height
        } else {
            data.buffer.max_visible_line_grapheme_len().saturating_sub(3) as f64 * self.metrics.font_advance
        }
    }

    fn handle_len(&self, data: &EditStack) -> f64 {
        (self.len.powi(2) / (self.text_len(data) + self.len)).max(self.metrics.font_height)
    }

    fn effective_len(&self, data: &EditStack) -> f64 {
        self.len - self.handle_len(data)
    }

    fn rect(&self) -> Rect {
        if self.is_vertical() {
            Rect::new(
                0.0,
                self.range.start,
                self.metrics.font_advance + SCROLLBAR_X_PADDING,
                self.range.end,
            )
        } else {
            Rect::new(
                self.range.start,
                0.0,
                self.range.end,
                self.metrics.font_advance + SCROLLBAR_X_PADDING,
            )
        }
    }

    fn is_vertical(&self) -> bool {
        self.direction == ScrollBarDirection::Vertical
    }
}

impl Widget<EditStack> for ScrollBar {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut EditStack, _env: &Env) {
        // if self.text_len(data) < 0.1 {
        //     return;
        // }
        match event {
            Event::Command(cmd) if cmd.is(SCROLL_TO) => {
                if self.text_len(data) < 0.1 {
                    return;
                }
                if self.is_vertical() {
                    if let Some(dy) = cmd.get_unchecked(SCROLL_TO).1 {
                        self.delta = dy;
                        let len = self.effective_len(data);
                        let dy = dy * self.len / self.text_len(data);

                        self.range.start = -dy * len / self.len;
                        self.range.end = self.range.start + self.handle_len(data);

                        ctx.request_paint();
                        ctx.set_handled();
                    }
                } else if let Some(dx) = cmd.get_unchecked(SCROLL_TO).0 {
                    self.delta = dx;
                    let len = self.effective_len(data);

                    let dx = dx * self.len / self.text_len(data);
                    self.range.start = -dx * len / self.len;
                    self.range.end = self.range.start + self.handle_len(data);

                    ctx.request_paint();
                    ctx.set_handled();
                }
            }
            Event::Command(cmd) if cmd.is(RESET_HELD_STATE) => {
                self.is_held = false;
                self.is_hovered = false;
                ctx.request_paint();
            }
            Event::MouseDown(m) => {
                if self.rect().contains(Point::new(m.pos.x, m.pos.y)) {
                    ctx.set_active(true);
                    self.mouse_delta = if self.is_vertical() {
                        m.pos.y - self.range.start
                    } else {
                        m.pos.x - self.range.start
                    };
                    ctx.request_paint();
                    ctx.set_handled();
                    self.is_held = true;
                }
            }
            Event::MouseUp(_) => {
                ctx.set_active(false);
                ctx.set_handled();
                self.is_held = false;
            }
            Event::MouseMove(m) => {
                if self.rect().contains(Point::new(m.pos.x, m.pos.y)) {
                    self.is_hovered = true;
                } else {
                    self.is_hovered = false;
                }
                if self.is_held && ctx.is_active() && m.buttons.contains(MouseButton::Left) {
                    if self.is_vertical() {
                        ctx.submit_command(
                            SCROLL_TO
                                .with((
                                    None,
                                    Some((self.mouse_delta - m.pos.y) * self.text_len(data) / self.effective_len(data)),
                                ))
                                .to(self.owner_id),
                        );
                    } else {
                        ctx.submit_command(
                            SCROLL_TO
                                .with((
                                    Some((self.mouse_delta - m.pos.x) * self.text_len(data) / self.effective_len(data)),
                                    None,
                                ))
                                .to(self.owner_id),
                        );
                    }

                    ctx.set_handled();
                }
                ctx.request_paint();
            }
            _ => (),
        }
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, _data: &EditStack, _env: &Env) {
        match event {
            LifeCycle::WidgetAdded => {
                let child = match self.direction {
                    ScrollBarDirection::Horizontal => ChildWidget::HScroll(ctx.widget_id()),
                    ScrollBarDirection::Vertical => ChildWidget::VScroll(ctx.widget_id())
                };
                ctx.submit_command(
                    WIDGET_ATTACHED
                        .with(child)
                        .to(self.owner_id),
                );
            }
            _ => ()
        }
    }

    fn update(&mut self, _ctx: &mut UpdateCtx, _old_data: &EditStack, _data: &EditStack, _env: &Env) {
        //todo!()
    }

    fn layout(&mut self, _ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &EditStack, env: &Env) -> Size {
        self.metrics = CommonMetrics::from_env(env);

        self.len = if self.is_vertical() {
            bc.max().height
        } else {
            bc.max().width
        };

        self.range = if self.text_len(data) < 0.1 {
            Range {
                start: 0.,
                end: self.len,
            }
        } else {
            let start = -self.delta * (self.effective_len(data)) / self.text_len(data);
            let end = start + self.handle_len(data);
            Range { start, end }
        };
        // let weight = if self.text_len(data) <= 0.1 {
        //     0.
        // } else {
        //     self.metrics.font_advance
        // };
        if self.is_vertical() {
            Size::new(self.metrics.font_advance + SCROLLBAR_X_PADDING, self.len)
        } else {
            Size::new(self.len, self.metrics.font_advance + SCROLLBAR_X_PADDING)
        }
    }

    fn paint(&mut self, ctx: &mut PaintCtx, _data: &EditStack, env: &Env) {
        //if self.is_vertical() {
        let r = ctx.size().to_rect();
        ctx.clip(r);
        ctx.fill(r, &env.get(crate::theme::EDITOR_BACKGROUND));
        if self.is_held {
            ctx.fill(
                self.rect().inflate(-1.0, -1.0).to_rounded_rect(3.),
                &env.get(crate::theme::SCROLLBAR_SLIDER_ACTIVE_BACKGROUND),
            );
        } else if self.is_hovered {
            ctx.fill(
                self.rect().inflate(-1.0, -1.0).to_rounded_rect(3.),
                &env.get(crate::theme::SCROLLBAR_SLIDER_HOVER_BACKGROUND),
            );
        } else {
            ctx.fill(
                self.rect().inflate(-1.0, -1.0).to_rounded_rect(3.),
                &env.get(crate::theme::SCROLLBAR_SLIDER_BACKGROUND),
            );
        }
        //}
    }
}

#[derive(Debug, Default)]
pub struct ScrollBarSpacer {
    metrics: CommonMetrics,
}

impl Widget<EditStack> for ScrollBarSpacer {
    fn event(&mut self, _ctx: &mut EventCtx, _event: &Event, _data: &mut EditStack, _env: &Env) {}

    fn lifecycle(&mut self, _ctx: &mut LifeCycleCtx, _event: &LifeCycle, _data: &EditStack, _env: &Env) {}

    fn update(&mut self, _ctx: &mut UpdateCtx, _old_data: &EditStack, _data: &EditStack, _env: &Env) {}

    fn layout(&mut self, _ctx: &mut LayoutCtx, _bc: &BoxConstraints, _data: &EditStack, env: &Env) -> Size {
        self.metrics = CommonMetrics::from_env(env);
        Size::new(
            self.metrics.font_advance + SCROLLBAR_X_PADDING,
            self.metrics.font_advance + SCROLLBAR_X_PADDING,
        )
    }

    fn paint(&mut self, ctx: &mut PaintCtx, _data: &EditStack, env: &Env) {
        let r = ctx.size().to_rect();
        ctx.clip(r);
        ctx.fill(r, &env.get(crate::theme::EDITOR_BACKGROUND));
    }
}