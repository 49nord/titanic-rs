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

// TODO: Document more what is happening and how to use it.
#[macro_export]
macro_rules! accept {
    ($self:expr, $toktype:ident) => {
        match $self.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::$toktype,
                ..
            })) => Ok(Some(TokenKind::$toktype)),
            Some(&Err(_)) => {
                if let Some(Err(e)) = $self.lexer.next() {
                    Err(e)
                } else {
                    unreachable!()
                }
            }
            _ => Ok(None),
        }
    };

    ($self:expr, $toktype:ident, $tokdata:ident) => {
        match $self.lexer.peek() {
            Some(Ok(Token {
                kind: TokenKind::$toktype(_),
                ..
            })) => {
                if let Some(Ok(Token {
                    kind: TokenKind::$toktype($tokdata),
                    ..
                })) = $self.lexer.next()
                {
                    Ok(Some($tokdata))
                } else {
                    unreachable!()
                }
            }
            Some(&Err(_)) => {
                if let Some(Err(e)) = $self.lexer.next() {
                    Err(e)
                } else {
                    unreachable!()
                }
            }
            _ => Ok(None),
        }
    };
}

#[macro_export]
macro_rules! expect {
    ($self:expr, $toktype:ident) => {
        match accept!($self, $toktype) {
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(ParseError::UnexpectedToken),
            Err(e) => Err(e.into()),
        }
    };
    ($self:expr, $toktype:ident, $tokdata:ident) => {
        match accept!($self, $toktype, $tokdata) {
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(ParseError::UnexpectedToken),
            Err(e) => Err(e.into()),
        }
    };
}
