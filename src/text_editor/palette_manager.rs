use druid::{BoxConstraints, Data, Lens, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx, Size, UpdateCtx, Widget, WidgetPod, WidgetId};
use crate::text_editor::palette_view::{CLOSE_PALETTE, DialogResult, PaletteBuilder, PaletteCommandType, PaletteView, PaletteViewState, SHOW_DIALOG_FOR_EDITOR, SHOW_PALETTE_FOR_EDITOR};
use crate::text_editor::RESET_HELD_STATE;

pub trait IsDirty {
    fn is_dirty(&self) -> bool;
    fn reset_dirty(&mut self);
}

pub trait PaletteData: druid::Data + IsDirty {}

pub struct PaletteManager<State: PaletteData> {
    inner: WidgetPod<State, Box<dyn Widget<State>>>,
    palette: WidgetPod<PaletteViewState, PaletteView>,
}

#[derive(Clone, Data, Lens)]
pub struct PaletteManagerState<State: PaletteData> {
    in_palette: bool,
    #[data(ignore)]
    editor: Option<WidgetId>,
    inner_state: State,
    palette_state: PaletteViewState
}

impl<State: PaletteData> PaletteManager<State> {
    pub fn build(inner: Box<dyn Widget<State>>, state: State) -> (Self, PaletteManagerState<State>){
        let widget = PaletteManager {
            inner: WidgetPod::new(inner),
            palette: WidgetPod::new(PaletteView::new())
        };

        let widget_state = PaletteManagerState {
            in_palette: false,
            editor: None,
            inner_state: state,
            palette_state: PaletteViewState::default()
        };

        (widget, widget_state)
    }
}

impl<State: PaletteData> Widget<PaletteManagerState<State>> for PaletteManager<State> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut PaletteManagerState<State>, env: &Env) {
        if ctx.is_handled() {
            return;
        }

        match event {
            druid::Event::MouseUp(_) => ctx.submit_command(RESET_HELD_STATE),

            druid::Event::Command(cmd) if cmd.is(SHOW_DIALOG_FOR_EDITOR) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_DIALOG_FOR_EDITOR).clone();
                data.editor = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::DialogEditor(f)),
                );
                self.palette.widget_mut().take_focus(ctx);
                return;
            },

            druid::Event::Command(cmd) if cmd.is(SHOW_PALETTE_FOR_EDITOR) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_PALETTE_FOR_EDITOR).clone();
                data.editor = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::Editor(f)),
                );
                self.palette.widget_mut().take_focus(ctx);
                return;
            },

            druid::Event::Command(cmd) if cmd.is(CLOSE_PALETTE) => {
                if let Some(editor) = data.editor {
                    ctx.set_focus(editor);
                } else {
                    ctx.focus_prev();
                }
                data.in_palette = false;
                data.editor = None;
                ctx.request_paint();
                return;
            },
/*
            druid::Event::WindowCloseRequested => {
                if data.inner_state.is_dirty() {
                    ctx.set_handled();
                    self.dialog()
                        .title("Discard unsaved change?")
                        .on_select(|result, ctx, _, data| {
                            if result == DialogResult::Ok {
                                data.inner_state.reset_dirty();
                                ctx.submit_command(druid::commands::CLOSE_WINDOW);
                            }
                        })
                        .show(ctx);
                }
            }
*/
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
        }    }

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

// impl PaletteBuilder<PaletteManagerState<dyn PaletteData>> for PaletteManager<dyn PaletteData> {}
