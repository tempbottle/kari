use std::fmt;

#[derive(Clone, Debug)]
pub struct Position {
    pub line: u32,
    pub col: u32,
    pub file: Option<String>
}

#[derive(Clone, Debug)]
pub struct PositionRange {
    pub start: Position,
    pub end: Position
}

#[derive(Clone, Debug)]
pub struct PositionContainer<T: Clone + fmt::Debug>(pub T, pub PositionRange);

impl Position {
    pub fn new(line: u32, col: u32, file: Option<String>) -> Position {
        Position {
            line: line,
            col: col,
            file: file
        }
    }
}

impl PositionRange {
    pub fn new(start: Position, end: Position) -> PositionRange {
        PositionRange {
            start: start,
            end: end
        }
    }

    pub fn extend(&mut self, other: Position) {
        self.end = other;
    }

    pub fn extend_new(mut self, other: Position) -> PositionRange {
        self.extend(other);
        self
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.file {
            Some(ref file) => write!(f, "{}:({} {})", file, self.line, self.col),
            None => write!(f, "({} {})", self.line, self.col)
        }
    }
}

impl fmt::Display for PositionRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.start.file {
            Some(ref file) => write!(f, "{}:({} {})-({} {})",
                                     file,
                                     self.start.line,
                                     self.start.col,
                                     self.end.line,
                                     self.end.col),
            None => write!(f, "({} {})-({} {})",
                           self.start.line,
                           self.start.col,
                           self.end.line,
                           self.end.col)
        }
    }
}
