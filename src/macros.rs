/// Like the try! macro, but only works on Result<Option<_>, _> and returns
/// Some(Err(err)) instead of Err(err).
/// ```rust
/// assert!(try_some!(Ok(Some(1))), Some(1));
/// assert!(try_some!(Ok(None)), None);
/// try_some!(Err("some error")) // returns Some(Err("some error"))
/// ```
#[macro_export]
macro_rules! try_some {
    ($expr:expr) => {
        match $expr {
            Ok(Some(v)) => Some(v),
            Ok(None) => None,
            Err(e) => return Some(Err(From::from(e))),
        }
    };
}

/// Like the try! macro only that this returns Some(Err(err)) instead of Err(err).
/// ```rust
/// assert!(try_err!(Ok(1)), try!(Ok(1)));
/// assert!(try_err!(Err("some error"))) // returns Some(Err("some error"))
/// ```
#[macro_export]
macro_rules! try_err {
    ($expr:expr) => {
        match $expr {
            Ok(v) => v,
            Err(e) => return Some(Err(From::from(e))),
        }
    };
}

/// Accept only the given `TokenKind`.
///
/// The first arg has to have an accessible field `lexer: iter::Peekable<_>`.
/// The second arg is the name of a `TokenKind` variant, e.g. `StringLiteral`.
/// If the variant contains data, a third arg is needed and can be any indent.
///
/// `Ok(Some(TokenKind))` is returned if it is the correct token without data.
/// `Ok(Some(data))` is returned if it is the correct token with data.
/// `Ok(None)` is returned if another token is found.
/// If an error is found, it will be returned.
///
/// ```rust
/// // In a method of GgaParser wrapping ",doc*"
/// assert_eq!(accept!(self, CommaSeparator), Ok(Some(TokenKind::CommaSeparator)));
/// assert_eq!(accept!(self, CommaSeparator), Ok(None));
/// match accept!(self, StringLiteral, s) {
///     Ok(Some(s)) => assert_eq!(s.len(), 3),
///     _ => unreachable!(),
/// }
/// assert!(accept!(self, Checksum, c).is_err());
/// ```
#[macro_export]
macro_rules! accept {
    ($parser:expr, $toktype:ident) => {
        match $parser.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::$toktype,
                ..
            })) => {
                $parser.lexer.next();
                Ok(Some(TokenKind::$toktype))
            }
            Some(&Err(_)) => {
                if let Some(Err(e)) = $parser.lexer.next() {
                    Err(e)
                } else {
                    unreachable!()
                }
            }
            _ => Ok(None),
        }
    };

    ($parser:expr, $toktype:ident, $tokdata:ident) => {
        match $parser.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::$toktype(_),
                ..
            })) => {
                if let Some(Ok(Token {
                    kind: TokenKind::$toktype($tokdata),
                    ..
                })) = $parser.lexer.next()
                {
                    Ok(Some($tokdata))
                } else {
                    unreachable!()
                }
            }
            Some(&Err(_)) => {
                if let Some(Err(e)) = $parser.lexer.next() {
                    Err(e)
                } else {
                    unreachable!()
                }
            }
            _ => Ok(None),
        }
    };
}

/// Expect the given `TokenKind`.
///
/// The first arg has to be a parser with an accessible field `lexer`.
/// The second arg is the name of a `TokenKind` variant, e.g. `StringLiteral`.
/// If the variant contains data, a third arg is needed and can be any indent.
///
/// `Ok(TokenKind)` is returned if it is the correct token without data.
/// `Ok(data)` is returned if it is the correct token with data.
/// `Err(ParseError::UnexpectedToken)` is returned if another token is found.
/// If an error is found, it will be returned.
///
/// ```rust
/// // In a method of GgaParser wrapping ",doc*"
/// assert_eq!(expect!(self, CommaSeparator), Ok(TokenKind::CommaSeparator));
/// assert_eq!(expect!(self, CommaSeparator), Err(ParseError::UnexpectedToken));
/// match expect!(self, StringLiteral, s) {
///     Ok(s) => assert_eq!(s.len(), 3),
///     _ => unreachable!(),
/// }
/// assert!(expect!(self, Checksum, c).is_err());
/// ```
#[macro_export]
macro_rules! expect {
    ($parser:expr, $toktype:ident) => {
        match accept!($parser, $toktype) {
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(ParseError::UnexpectedToken),
            Err(e) => Err(e.into()),
        }
    };
    ($parser:expr, $toktype:ident, $tokdata:ident) => {
        match accept!($parser, $toktype, $tokdata) {
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(ParseError::UnexpectedToken),
            Err(e) => Err(e.into()),
        }
    };
}
