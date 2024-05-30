use std::{fs::File, io::Read, str::Chars};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    /// 1 -2 0b111 0x1a
    LiteralInt,
    /// 1.0 -3.14 1.
    LiteralFloat,
    /// "oui" "hello\n\t world"
    LiteralString,
    /// 'c' 'a' '\n'
    LiteralChar,
    /// true, false
    LiteralBool,

    /// Identifiers
    Identifier,
    /// Keyword let
    KeywordLet,
    /// Keyword mut
    KeywordMut,
    /// Keyword fun
    KeywordFun,
    /// Keyword struct
    KeywordStruct,
    /// Keyword impl
    KeywordImpl,

    /// //
    SlashSlash,
    // /// /*
    // SlashStar,
    // /// */
    // StarSlash,
    /// ///
    SlashSlashSlash,
    /// //!
    SlashSlashBang,

    /// =
    Equal,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Star,
    /// /
    Slash,
    /// %
    Percent,

    /// ^
    Caret,
    /// <<
    AngleAngleLeft,
    /// >>
    AngleAngleRight,
    /// &
    Ampersand,
    /// |
    Pipe,

    /// !
    Bang,
    /// <
    AngleLeft,
    /// >
    AngleRight,
    /// ==
    EqualEqual,
    /// !=,
    BangEqual,
    /// <=
    AngleLeftEqual,
    /// >=
    AngleRightEqual,
    /// &&
    AmpersandAmpersand,
    /// ||
    PipePipe,

    /// +=
    PlusEqual,
    /// -=
    MinusEqual,
    /// *=
    StarEqual,
    /// /=
    SlashEqual,
    /// %=
    PercentEqual,
    /// ^=
    CaretEqual,
    /// <<=
    AngleAngleLeftEqual,
    /// >>=
    AngleAngleRightEqual,
    /// &=
    AmpersandEqual,
    /// |=
    PipeEqual,

    /// (
    ParentheseOpen,
    /// )
    ParentheseClose,
    /// {
    BraceOpen,
    /// }
    BraceClose,
    /// [
    BracketOpen,
    /// ]
    BracketClose,

    /// @
    At,
    /// ~
    Tilde,
    /// #
    Pound,
    /// .
    Dot,
    /// ,
    Comma,
    /// :
    Colon,
    /// ;
    SemiColon,
    /// ?
    Question,
    /// $
    Dollar,
    /// €
    Euro,
    /// £
    UK,

    /// Any unicode with whitespace=yes
    Whitespace,
    /// Unknown tokens
    Unknown,
    /// End of file
    Eof,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

pub fn tokenize(mut file: File) -> Result<Vec<Token>, std::io::Error> {
    let content = {
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        content
    };

    let mut reader = Lexer::new(&content);

    let tokens = std::iter::from_fn(|| {
        let token = reader.advance_token();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    })
    .collect::<Vec<_>>();

    Ok(tokens)
}

pub struct Lexer<'a> {
    contents: &'a str,
    iter: Chars<'a>,
    position: usize,
    ended: bool,
}

impl<'a> Lexer<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            contents: s,
            iter: s.chars(),
            position: 0,
            ended: false,
        }
    }

    fn advance_token(&mut self) -> Token {
        if self.ended {
            return Token::new(TokenKind::Eof, 0);
        }

        let Some(first) = self.peek_first() else {
            self.ended = true;
            return Token::new(TokenKind::Eof, 0);
        };
        let second = self.peek_second();
        let third = self.peek_third();

        // 3-wide tokens
        if let (Some(second), Some(third)) = (second, third) {
            let kind = match (first, second, third) {
                ('/', '/', '/') => Some(TokenKind::SlashSlashSlash),
                ('/', '/', '!') => Some(TokenKind::SlashSlashBang),
                ('<', '<', '=') => Some(TokenKind::AngleAngleLeftEqual),
                ('>', '>', '=') => Some(TokenKind::AngleAngleRightEqual),
                _ => None,
            };
            if let Some(kind) = kind {
                self.advance_by(3);
                return Token::new(kind, 3);
            }
        }

        // 2-wide tokens
        if let Some(second) = second {
            let kind = match (first, second) {
                ('/', '/') => Some(TokenKind::SlashSlash),

                ('<', '<') => Some(TokenKind::AngleAngleLeft),
                ('>', '>') => Some(TokenKind::AngleAngleRight),

                ('/', '=') => Some(TokenKind::SlashEqual),
                ('+', '=') => Some(TokenKind::PlusEqual),
                ('-', '=') => Some(TokenKind::MinusEqual),
                ('*', '=') => Some(TokenKind::StarEqual),
                ('%', '=') => Some(TokenKind::PercentEqual),
                ('^', '=') => Some(TokenKind::CaretEqual),
                ('&', '=') => Some(TokenKind::AmpersandEqual),
                ('|', '=') => Some(TokenKind::PipeEqual),

                ('=', '=') => Some(TokenKind::EqualEqual),
                ('!', '=') => Some(TokenKind::BangEqual),
                ('&', '&') => Some(TokenKind::AmpersandAmpersand),
                ('|', '|') => Some(TokenKind::PipePipe),
                ('<', '=') => Some(TokenKind::AngleLeftEqual),
                ('>', '=') => Some(TokenKind::AngleRightEqual),
                _ => None,
            };
            if let Some(kind) = kind {
                self.advance_by(2);
                return Token::new(kind, 2);
            }
        }

        // 1-wide tokens
        {
            let kind = match first {
                '+' => Some(TokenKind::Plus),
                '-' => Some(TokenKind::Minus),
                '*' => Some(TokenKind::Star),
                '/' => Some(TokenKind::Slash),
                '%' => Some(TokenKind::Percent),
                '^' => Some(TokenKind::Caret),
                '&' => Some(TokenKind::Ampersand),
                '|' => Some(TokenKind::Pipe),

                '=' => Some(TokenKind::Equal),

                '!' => Some(TokenKind::Bang),
                '<' => Some(TokenKind::AngleLeft),
                '>' => Some(TokenKind::AngleRight),

                '(' => Some(TokenKind::ParentheseOpen),
                ')' => Some(TokenKind::ParentheseClose),
                '{' => Some(TokenKind::BracketOpen),
                '}' => Some(TokenKind::BracketClose),
                '[' => Some(TokenKind::BraceOpen),
                ']' => Some(TokenKind::BraceClose),

                '@' => Some(TokenKind::At),
                '~' => Some(TokenKind::Tilde),
                '#' => Some(TokenKind::Pound),
                '.' => Some(TokenKind::Dot),
                ',' => Some(TokenKind::Comma),
                ':' => Some(TokenKind::Colon),
                ';' => Some(TokenKind::SemiColon),
                '?' => Some(TokenKind::Question),
                '$' => Some(TokenKind::Dollar),
                '€' => Some(TokenKind::Euro),
                '£' => Some(TokenKind::UK),
                _ => None,
            };
            if let Some(kind) = kind {
                self.advance();
                return Token::new(kind, 1);
            }
        }

        // Indefinite sized tokens
        match first {
            w if w.is_whitespace() => self.advance_whitespace(),

            '_' => self.advance_identifier_or_keyword(),
            c if c.is_alphabetic() => self.advance_identifier_or_keyword(),

            d if d.is_digit(10) => self.advance_literal_number(),

            '\'' => self.advance_literal_char(),
            '"' => self.advance_literal_string(),

            _ => {
                self.advance();
                Token::new(TokenKind::Unknown, 1)
            }
        }
    }

    fn advance_whitespace(&mut self) -> Token {
        debug_assert!(self.peek_first().unwrap().is_whitespace());
        let start = self.position();
        self.advance_while(|w| w.is_whitespace());
        let end = self.position();

        Token::new(TokenKind::Whitespace, end - start)
    }

    fn advance_identifier_or_keyword(&mut self) -> Token {
        let start = self.position();
        let first_char = self.advance();
        assert!(first_char.is_alphabetic());

        // FIXME: This is probably a bit flawed
        self.advance_while(|d| d.is_alphanumeric() || d == '_');
        let end = self.position();

        let ident = &self.contents[start..end];
        // Try to recognize keywords
        let kind = match ident {
            "let" => TokenKind::KeywordLet,
            "mut" => TokenKind::KeywordMut,
            "fun" => TokenKind::KeywordFun,
            "struct" => TokenKind::KeywordStruct,
            "impl" => TokenKind::KeywordImpl,
            "true" | "false" => TokenKind::LiteralBool,
            _ => TokenKind::Identifier,
        };

        Token::new(kind, end - start)
    }

    fn advance_literal_number(&mut self) -> Token {
        let start = self.position();

        // We define a number as a sequence of numbers and letters with exactly one dot
        // FIXME: This is obviously flawed and doesn't take into account exponent notation among
        // many other things.
        self.advance_while(|d| d.is_ascii_alphanumeric());
        if self.peek_first() == Some('.') {
            self.advance_while(|d| d.is_ascii_alphanumeric());
        }

        let end = self.position();
        Token::new(TokenKind::LiteralInt, end - start)
    }

    fn advance_literal_char(&mut self) -> Token {
        debug_assert_eq!(self.peek_first(), Some('\''));
        let start = self.position();

        self.advance();
        loop {
            self.advance_while(|c| c != '\\' && c != '\'');
            // If escaped quote, skip both
            if self.peek_first() == Some('\\') && self.peek_second() == Some('\'') {
                self.advance_by(2);
            } else if self.peek_first() == Some('\'') {
                self.advance();
                break;
            }
        }
        let end = self.position();
        Token::new(TokenKind::LiteralChar, end - start)
    }

    fn advance_literal_string(&mut self) -> Token {
        debug_assert_eq!(self.peek_first(), Some('"'));
        let start = self.position();

        self.advance();
        loop {
            self.advance_while(|c| c != '\\' && c != '"');
            // Detect escaped quotes
            if self.peek_first() == Some('\\') && self.peek_second() == Some('"') {
                self.advance_by(2);
            } else if self.peek_first() == Some('"') {
                self.advance();
                break;
            }
        }
        let end = self.position();
        Token::new(TokenKind::LiteralString, end - start)
    }

    fn position(&mut self) -> usize {
        self.position
    }

    fn advance(&mut self) -> char {
        self.position += 1;
        self.iter.next().unwrap()
    }

    fn advance_by(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    fn advance_while(&mut self, mut f: impl FnMut(char) -> bool) {
        loop {
            if let Some(c) = self.peek_first() {
                if f(c) {
                    self.advance();
                    continue;
                }
            }
            break;
        }
    }

    fn peek_first(&self) -> Option<char> {
        if self.ended {
            None
        } else {
            self.iter.clone().next()
        }
    }

    fn peek_second(&self) -> Option<char> {
        if self.ended {
            None
        } else {
            self.iter.clone().skip(1).next()
        }
    }

    fn peek_third(&self) -> Option<char> {
        self.iter.clone().skip(2).next()
    }
}
