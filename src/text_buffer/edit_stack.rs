use std::collections::HashMap;
use std::io::Result;
use std::ops::{Deref, DerefMut, Range, RangeFrom, RangeTo};
use std::path::{Path, PathBuf};

use super::buffer::Buffer;
use super::file::TextFileInfo;
use druid::{Data, WidgetId};
use lazy_static::lazy_static;
use crate::text_editor::editor_view::{EDITOR_CAN_REDO, EDITOR_CAN_UNDO, EDITOR_DIRTY_MASK, EDITOR_HAS_SELECTION, EDITOR_PRESENT_MASK};

#[derive(Debug, Clone, Default)]
pub struct EditStack {
    pub buffer: Buffer,
    undo_stack: Vec<Buffer>,
    redo_stack: Vec<Buffer>,
    pub file: TextFileInfo,
    pub filename: Option<PathBuf>,
    dirty: bool,
    pub widget_id: Option<WidgetId>
}

impl Data for EditStack {
    fn same(&self, other: &Self) -> bool {
        self.buffer.same(&other.buffer)
            && self.file == other.file
            && self.filename == other.filename
            && self.dirty == other.dirty
    }
}

lazy_static! {
    static ref AUTO_INSERT_CHARMAP: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();
        m.insert("{", "{}");
        m.insert("(", "()");
        m.insert("<", "<>");
        m.insert("[", "[]");
        m.insert("\"", "\"\"");
        m
    };
}

impl EditStack {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_event_mask(&self) -> u64 {
        let value = EDITOR_PRESENT_MASK;
        let value = value | if self.is_dirty() {
            EDITOR_DIRTY_MASK
        } else  {
            0
        };
        let value = value | if self.can_undo() {
            EDITOR_CAN_UNDO
        } else {
            0
        };
        let value = value | if self.can_redo() {
            EDITOR_CAN_REDO
        } else {
            0
        };
        let value = value | if self.has_selection() {
            EDITOR_HAS_SELECTION
        } else {
            0
        };

        value
    }

    pub fn selected_text(&self) -> String {
        self.buffer.selected_text(self.file.linefeed)
    }

    pub fn main_cursor_selected_text(&self) -> String {
        self.buffer.main_cursor_selected_text()
    }

    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self> {
        let file = TextFileInfo::load(&path)?;
        let buffer = Buffer::from_rope(file.1, file.0.indentation.visible_len());
        Ok(Self {
            buffer,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            file: file.0,
            filename: Some(path.as_ref().to_path_buf()),
            dirty: false,
            widget_id: None
        })
    }

    pub fn open<P>(&mut self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let editor = EditStack::from_file(path)?;
        let _ = std::mem::replace(self, editor);
        Ok(())
    }

    pub fn reload(&mut self) -> Result<()> {
        if let Some(f) = &self.filename.clone() {
            self.open(f)
        } else {
            // unreachable?
            Ok(())
        }
    }

    pub fn is_dirty(&self) -> bool {
        self.dirty
    }

    pub fn reset_dirty(&mut self) {
        self.dirty = false;
    }

    pub fn reset_undo_redo(&mut self) {
        self.undo_stack.clear();
        self.redo_stack.clear();
    }

    pub fn set_dirty(&mut self) {
        self.dirty = true;
    }

    pub fn save<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.file.save_as(&self.buffer, &path)?;
        self.filename = Some(path.as_ref().to_path_buf());
        self.dirty = false;
        self.undo_stack.clear();
        self.redo_stack.clear();
        Ok(())
    }

    pub fn can_undo(&self) -> bool {
        !self.undo_stack.is_empty()
    }

    pub fn undo(&mut self) {
        if let Some(buffer) = self.undo_stack.pop() {
            let b = std::mem::take(&mut self.buffer);
            self.redo_stack.push(b);
            self.buffer = buffer;
        }
        if self.undo_stack.is_empty() {
            self.dirty = false;
        }
    }

    pub fn can_redo(&self) -> bool {
        !self.redo_stack.is_empty()
    }

    pub fn redo(&mut self) {
        if let Some(buffer) = self.redo_stack.pop() {
            let b = std::mem::take(&mut self.buffer);
            self.undo_stack.push(b);
            self.buffer = buffer;
        }
    }

    fn push_edit(&mut self, buffer: Buffer) {
        let b = std::mem::take(&mut self.buffer);
        self.undo_stack.push(b);
        self.buffer = buffer;
        self.redo_stack.clear();
        self.dirty = true;
    }

    pub fn insert(&mut self, text: &str) {
        let mut buf = self.buffer.clone();

        match text {
            linefeed if linefeed == self.file.linefeed.to_str() => {
                buf.insert(text, false);
                buf.indent(self.file.indentation);
            }
            s if AUTO_INSERT_CHARMAP.get(s).is_some() => {
                let inner_text = buf.selected_text(self.file.linefeed);
                buf.insert(AUTO_INSERT_CHARMAP[text], false);
                buf.backward(false, false);
                buf.insert(&inner_text, true);
            }
            _ => {
                buf.insert(text, false);
            }
        }

        self.push_edit(buf);
    }

    pub fn backspace(&mut self) {
        let mut buf = self.buffer.clone();

        // TODO check if old buf is same that new with the Data trait
        if buf.backspace() {
            self.push_edit(buf);
        }
    }

    pub fn delete(&mut self) {
        let mut buf = self.buffer.clone();

        if buf.delete() {
            self.push_edit(buf);
        }
    }

    pub fn tab(&mut self) {
        let mut buf = self.buffer.clone();
        buf.tab(self.file.indentation);
        self.push_edit(buf);
    }
}

impl Deref for EditStack {
    type Target = Buffer;
    fn deref(&self) -> &Self::Target {
        &self.buffer
    }
}

impl DerefMut for EditStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buffer
    }
}

#[derive(Debug, Clone)]
pub enum SelectionLineRange {
    Range(Range<usize>),
    RangeTo(RangeTo<usize>),
    RangeFrom(RangeFrom<usize>),
    RangeFull,
}
