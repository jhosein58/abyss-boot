#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }
}

pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub struct SourceMap {
    line_starts: Vec<usize>,
}

impl SourceMap {
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];
        line_starts.extend(
            source
                .char_indices()
                .filter(|&(_, c)| c == '\n')
                .map(|(i, _)| i + 1),
        );
        Self { line_starts }
    }

    pub fn find_position(&self, offset: usize, source: &str) -> Option<Position> {
        let line_index = self.line_starts.partition_point(|&start| start <= offset) - 1;
        let line_start_offset = *self.line_starts.get(line_index)?;

        let line = line_index + 1;

        let line_content_up_to_offset = source.get(line_start_offset..offset)?;

        let column = line_content_up_to_offset.chars().count() + 1;

        Some(Position { line, column })
    }

    pub fn position_from_span(&self, span: &Span, source: &str) -> Option<Position> {
        self.find_position(span.start, source)
    }
}
