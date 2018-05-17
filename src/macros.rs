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

#[macro_export]
macro_rules! accept {
    ($self:expr, $tokpat:pat, $tokthen:block) => {
        match $self.lexer.peek() {
            Some(&Ok(Token { kind: $tokpat, .. })) => {
                if let Some(Ok(Token { kind: $tokpat, .. })) = $self.lexer.next() {
                    Ok(Some($tokthen))
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
    ($self:expr, $tokpat:pat, $tokthen:block) => {
        match accept!($self, $tokpat, $tokthen)? {
            Some(t) => Some(t),
            None => return Err(ParseError::UnexpectedToken),
        }
    };
}
