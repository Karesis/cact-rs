use std::ops::Range;
use miette::SourceSpan;
use chumsky::span::Span as ChumskySpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize
}
impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn start(self) -> usize {
        self.start
    }

    pub fn end(self) -> usize {
        self.end
    }

    pub fn to(self, other: Span) -> Span {
        Self::new(self.start(), other.end())
    }
}

impl From<std::ops::Range<usize>> for Span {
    fn from(range: std::ops::Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl Into<std::ops::Range<usize>> for Span {
    fn into(self) -> std::ops::Range<usize> {
        self.start..self.end
    }
}

/// for chumsky usage
impl ChumskySpan for Span {
    // "Span contexts have no inherent meaning to Chumsky... For example, Range<usize>’s
    // implementation of Span simply uses () as its context.""
    type Context = ();
    
    // "A type representing a span’s start or end offset... Typically, usize is used."
    type Offset = usize;

    // method that build a Span using Context and Range
    fn new(_context: Self::Context, range: Range<Self::Offset>) -> Self {
        // we didn't need context yet
        Self {
            start: range.start,
            end: range.end,
        }
    }

    // get context
    fn context(&self) -> Self::Context {
        ()
    }

    // get start
    fn start(&self) -> Self::Offset {
        self.start
    }

    // get end 
    fn end(&self) -> Self::Offset {
        self.end
    }
}

// for miette's usage
impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        // SourceSpan in miette are made with "start" and "length"
        Self::new(span.start.into(), (span.end - span.start).into())
    }
}