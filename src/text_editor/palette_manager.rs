use std::rc::Rc;
use druid::{BoxConstraints, Data, Lens, Env, Event, EventCtx, LayoutCtx, LifeCycle, LifeCycleCtx, PaintCtx, Size, UpdateCtx, Widget, WidgetPod, WidgetId, Selector, Command};
use druid::im::Vector;
use crate::text_editor::palette_view::{CLOSE_PALETTE, DialogResult, Item, Palette, PALETTE_CALLBACK, PaletteBuilder, PaletteCommandType, PaletteResult, PaletteView, PaletteViewState, SHOW_DIALOG_FOR_EDITOR, SHOW_PALETTE_FOR_EDITOR, ShowPalette};
use crate::text_editor::RESET_HELD_STATE;

pub trait IsDirty {
    fn is_dirty(&self) -> bool;
    fn reset_dirty(&mut self);
}

const RESET_DIRTY: Selector<()> = Selector::new("nonepad.dialog.reset_dirty");
const FOCUS_PALETTE: Selector<()> = Selector::new("nonepad.palette.focus");

pub struct PaletteManager<State: druid::Data + IsDirty> {
    inner: Box<WidgetPod<State, Box<dyn Widget<State>>>>,
    palette: WidgetPod<PaletteViewState, PaletteView>,
}

#[derive(Clone, Lens)]
pub struct PaletteManagerState<State: druid::Data + IsDirty> {
    in_palette: bool,
    editor: Option<WidgetId>,
    inner_state: Box<State>,
    palette_state: PaletteViewState
}

impl<State: druid::Data + IsDirty> Data for PaletteManagerState<State> {
    fn same(&self, other: &Self) -> bool {
        other.in_palette == self.in_palette && other.palette_state.same(&self.palette_state) && other.inner_state.same(&self.inner_state)
    }
}

impl<State: druid::Data + IsDirty> PaletteManager<State> {
    pub fn build(inner: Box<dyn Widget<State>>, state: State) -> (Self, PaletteManagerState<State>){
        let widget = PaletteManager {
            inner: Box::new(WidgetPod::new(inner)),
            palette: WidgetPod::new(PaletteView::new())
        };

        let widget_state = PaletteManagerState {
            in_palette: false,
            editor: None,
            inner_state: Box::new(state),
            palette_state: PaletteViewState::default()
        };

        (widget, widget_state)
    }
}

impl<State: druid::Data + IsDirty> Widget<PaletteManagerState<State>> for PaletteManager<State> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut PaletteManagerState<State>, env: &Env) {
        if ctx.is_handled() {
            return;
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
                    _ => (),
                }
            }

            druid::Event::Command(cmd) if cmd.is(FOCUS_PALETTE) => {
                self.palette.widget_mut().take_focus(ctx);
            }

            druid::Event::Command(cmd) if cmd.is(SHOW_DIALOG_FOR_EDITOR) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_DIALOG_FOR_EDITOR).clone();
                data.editor = Some(widget.clone());
                self.palette.widget_mut().init(
                    &mut data.palette_state,
                    title,
                    list.clone(),
                    action.map(|f| PaletteCommandType::EditorDialog(f)),
                );

                ctx.submit_command(Command::from(FOCUS_PALETTE));
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
                    action.map(|f| PaletteCommandType::EditorPalette(f)),
                );

                ctx.submit_command(Command::from(FOCUS_PALETTE));
                return;
            },

            druid::Event::Command(cmd) if cmd.is(SHOW_DIALOG_FOR_WINDOW) => {
                data.in_palette = true;
                ctx.request_layout();
                let (widget, title, list, action) = cmd.get_unchecked(SHOW_DIALOG_FOR_WINDOW).clone();
                data.editor = Some(widget.clone());
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
                data.editor = Some(widget.clone());
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
                data.editor = None;
                ctx.request_paint();
                return;
            },

            druid::Event::Command(cmd) if cmd.is(RESET_DIRTY) => {
                data.inner_state.reset_dirty();
                return;
            },

            druid::Event::WindowCloseRequested => {
                if data.inner_state.is_dirty() {
                    ctx.set_handled();
                    self.dialog()
                        .title("Discard unsaved change?")
                        .on_action(|result, ctx| {
                            if result == DialogResult::Ok {
                                ctx.submit_command(Command::from(RESET_DIRTY));
                                ctx.submit_command(druid::commands::CLOSE_WINDOW);
                            }
                        })
                        .show(ctx);
                }
            }

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
)> = Selector::new("nonepad.dialog.show_for_window");

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