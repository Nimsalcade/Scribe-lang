use scribe_compiler::lexer::{Keyword, Lexer, LexerError, TokenKind};

fn collect_kinds(source: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(source);
    let mut kinds = Vec::new();
    loop {
        let token = lexer.next_token().expect("lexing failed");
        let is_eof = matches!(token.kind, TokenKind::Eof);
        kinds.push(token.kind);
        if is_eof {
            break;
        }
    }
    kinds
}

#[test]
fn lexes_basic_function_block() {
    let source = "fn main() -> int32:\n    let mutable total = 0\n";
    let kinds = collect_kinds(source);
    assert_eq!(
        kinds,
        vec![
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier("main".into()),
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Arrow,
            TokenKind::Identifier("int32".into()),
            TokenKind::Colon,
            TokenKind::Newline,
            TokenKind::Indent,
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Keyword(Keyword::Mutable),
            TokenKind::Identifier("total".into()),
            TokenKind::Equals,
            TokenKind::NumberLiteral("0".into()),
            TokenKind::Newline,
            TokenKind::Dedent,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn skips_comments_and_strings() {
    let source = "print(\"hi\") # trailing comment\n";
    let kinds = collect_kinds(source);
    assert_eq!(
        kinds,
        vec![
            TokenKind::Identifier("print".into()),
            TokenKind::LParen,
            TokenKind::TextLiteral("hi".into()),
            TokenKind::RParen,
            TokenKind::Newline,
            TokenKind::Eof,
        ]
    );
}

#[test]
fn errors_on_tab_indentation() {
    let source = "fn main():\n\tlet x = 1\n";
    let mut lexer = Lexer::new(source);
    // Consume some tokens: `fn`, `main`, `(`, `)` , `:`, newline.
    for _ in 0..6 {
        lexer.next_token().unwrap();
    }
    let err = lexer.next_token().expect_err("tabs should error");
    assert!(
        matches!(err, LexerError::TabIndentation { .. })
            || matches!(err, LexerError::TabCharacter { .. })
    );
}
