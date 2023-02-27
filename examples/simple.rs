// On Windows platform, don't show a console when opening the app.
#![windows_subsystem = "windows"]

use druid::widget::prelude::*;
use druid::{AppLauncher, Application, ClipboardFormat, Color, Data, HotKey, Lens, SysMods, WidgetExt, WindowDesc};
use once_cell::sync::Lazy;
use nonepad_editor::text_buffer::EditStack;
use nonepad_editor::text_buffer::syntax::SYNTAXSET;
use nonepad_editor::text_editor;
use nonepad_editor::theme::Theme;

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

const ALL_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::Cmd, "a"));
const CUT_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::Cmd, "x"));
const COPY_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::Cmd, "c"));
const PASTE_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::Cmd, "v"));
const UNDO_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::Cmd, "z"));
const REDO_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::CmdShift, "Z"));
const REDO2_HOTKEY: Lazy<HotKey> = Lazy::new(|| HotKey::new(SysMods::Cmd, "y"));

fn build_root_widget() -> impl Widget<SimpleState> {
    text_editor::new(Some(|ctx, event, _view, editor| {
        match event {
            Event::KeyDown(event) => {
                if CUT_HOTKEY.matches(event) {
                    Application::global().clipboard().put_string(editor.selected_text());
                    editor.delete();
                    ctx.set_handled();
                } else if COPY_HOTKEY.matches(event) {
                    Application::global().clipboard().put_string(editor.selected_text());
                    ctx.set_handled();
                } else if PASTE_HOTKEY.matches(event) {
                    let clipboard = Application::global().clipboard();
                    let supported_types = &[ClipboardFormat::TEXT];
                    let best_available_type = clipboard.preferred_format(supported_types);
                    if let Some(format) = best_available_type {
                        let data = clipboard
                            .get_format(format)
                            .expect("I promise not to unwrap in production");
                        editor.insert(String::from_utf8_lossy(&data).as_ref());
                    }
                    ctx.set_handled();
                } else if UNDO_HOTKEY.matches(event) {
                    editor.undo();
                    ctx.set_handled();
                } else if REDO_HOTKEY.matches(event) || REDO2_HOTKEY.matches(event) {
                    editor.redo();
                    ctx.set_handled();
                } else if ALL_HOTKEY.matches(event) {
                    editor.select_all();
                    ctx.set_handled();
                }
            }
            _ => ()
        }
    })).lens(SimpleState::edit_stack)
}
