// On Windows platform, don't show a console when opening the app.
#![windows_subsystem = "windows"]

use std::cell::RefCell;
use std::rc::Rc;
use druid::widget::prelude::*;
use druid::{AppLauncher, Data, Lens, WidgetExt, WindowDesc};
use nonepad_editor_view::text_buffer::EditStack;
use nonepad_editor_view::text_buffer::syntax::SYNTAXSET;
use nonepad_editor_view::text_editor;
use nonepad_editor_view::text_editor::editor_view::{EditorCommand, EditorKeyBindings};
use nonepad_editor_view::text_editor::palette_manager::{PaletteAppState, PaletteManager};
use nonepad_editor_view::theme::Theme;

#[derive(Clone, Data, Lens)]
struct SimpleState {
    edit_stack: EditStack,
}

impl PaletteAppState for SimpleState {
    fn is_dirty(&self) -> bool {
        self.edit_stack.is_dirty()
    }

    fn reset_dirty(&mut self) {
        self.edit_stack.reset_dirty()
    }

    fn get_state_mask(&self) -> u64 {
        u64::MAX
    }
}

pub fn main() {
    #[cfg(debug_assertions)]
    {
        let subscriber = tracing_subscriber::FmtSubscriber::builder().with_max_level(tracing::Level::TRACE).finish();

        tracing::subscriber::set_global_default(subscriber)
            .expect("setting default subscriber failed");
    }

    // create the initial app state
    let mut initial_state: SimpleState = SimpleState {
        edit_stack: EditStack::new(),
    };

    let mut key_bindings = EditorKeyBindings::cua().with_palette();
    for (idx, syntax) in SYNTAXSET.syntaxes().iter().enumerate() {
        let desc = format!("Set syntax: {}", syntax.name);
        key_bindings.editor_commands.push(EditorCommand::new(Some(desc.as_str()), None, idx as u64,|_ctx, tag, editor| {
            editor.file.syntax = SYNTAXSET.syntaxes().get(tag as usize).unwrap();
        }).unwrap());
    }

    let key_bindings = Rc::new(RefCell::new(key_bindings));

    initial_state.edit_stack.insert("-- enter some SQL text here\nselect * from mytable where id=1234 or name in ('Molly', 'Marcus')\n\n");
    initial_state.edit_stack.reset_dirty();
    initial_state.edit_stack.reset_undo_redo();

    initial_state.edit_stack.file.syntax = SYNTAXSET.find_syntax_by_name("SQL").expect("SQL Syntax not found");

    let root = build_root_widget(key_bindings.clone()).boxed();
    let (root, state) = PaletteManager::build(root, initial_state, None, Some(key_bindings));

    // describe the main window
    let main_window = WindowDesc::new(root)
        .title("Hello Edit")
        .window_size((800.0, 600.0));

    // start the application. Here we pass in the application state.
    AppLauncher::with_window(main_window)
        .configure_env(|env, _| {
            // load the default editor theme
            Theme::default().to_env(env);
        })
        .launch(state)
        .expect("Failed to launch application");
}

fn build_root_widget(bindings: Rc<RefCell<EditorKeyBindings>>) -> impl Widget<SimpleState> {
    text_editor::new(bindings).lens(SimpleState::edit_stack)
}
