// On Windows platform, don't show a console when opening the app.
#![windows_subsystem = "windows"]

use druid::widget::prelude::*;
use druid::{AppLauncher, Color, Data, Lens, WidgetExt, WindowDesc};
use nonepad_editor_view::text_buffer::EditStack;
use nonepad_editor_view::text_buffer::syntax::SYNTAXSET;
use nonepad_editor_view::text_editor;
use nonepad_editor_view::text_editor::editor_view::EditorKeyBindings;
use nonepad_editor_view::theme::Theme;

#[derive(Clone, Data, Lens)]
struct SimpleState {
    edit_stack: EditStack
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
        edit_stack: EditStack::new()
    };

    initial_state.edit_stack.insert("-- enter some SQL text here\nselect * from mytable where id=1234 or name in ('Molly', 'Marcus')\n\n");

    initial_state.edit_stack.file.syntax = SYNTAXSET.find_syntax_by_name("SQL").expect("SQL Syntax not found");

    // describe the main window
    let main_window = WindowDesc::new(build_root_widget())
        .title("Hello Edit")
        .window_size((800.0, 600.0));

    // start the application. Here we pass in the application state.
    AppLauncher::with_window(main_window)
        .configure_env(|env, _| {
            // load the default editor theme
            let theme = Theme::default();

            env.set(
                druid::theme::WINDOW_BACKGROUND_COLOR,
                Color::from_hex_str(&theme.vscode.colors.editor_background).unwrap(),
            );
            env.set(
                druid::theme::BORDER_DARK,
                Color::from_hex_str(&theme.vscode.colors.panel_border).unwrap(),
            );

            theme.to_env(env);
        })
        .launch(initial_state)
        .expect("Failed to launch application");
}

fn build_root_widget() -> impl Widget<SimpleState> {
    text_editor::new(EditorKeyBindings::Cua).lens(SimpleState::edit_stack)
}
