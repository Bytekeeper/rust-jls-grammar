use heck::ToSnakeCase;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use scraper::{Html, Selector};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Rhs {
    one_of: Vec<Element>,
}

#[derive(Debug)]
pub enum Element {
    Multi(Vec<Element>),
    Optional(Vec<Element>),
    Sequence(Vec<Element>),
    Terminal(String),
}

fn parse_rhs_opt(input: &str) -> IResult<&str, Element> {
    map(
        delimited(
            char('['),
            separated_list1(multispace1, parse_rhs_element),
            char(']'),
        ),
        |v| Element::Optional(v),
    )(input)
}

fn parse_rhs_multi(input: &str) -> IResult<&str, Element> {
    map(
        delimited(
            char('{'),
            separated_list1(multispace1, parse_rhs_element),
            char('}'),
        ),
        |v| Element::Multi(v),
    )(input)
}

fn parse_token_or_rule(input: &str) -> IResult<&str, Element> {
    map(
        alt((
            tag("}"),
            recognize(pair(
                satisfy(|c| !c.is_whitespace()),
                take_while(|c: char| !c.is_whitespace() && c != ']' && c != '}'),
            )),
        )),
        |token: &str| Element::Terminal(token.to_string()),
    )(input)
}

fn parse_rhs_element(input: &str) -> IResult<&str, Element> {
    alt((parse_rhs_opt, parse_rhs_multi, parse_token_or_rule))(input)
}

fn parse_rhs_seq(input: &str) -> IResult<&str, Element> {
    map(
        terminated(many1(preceded(multispace0, parse_rhs_element)), multispace0),
        |elements| Element::Sequence(elements),
    )(input)
}

fn parse_rhs(input: &[String]) -> Result<Rhs, nom::Err<nom::error::Error<&str>>> {
    let mut one_of = vec![];
    if input[0].trim() == "(one of)" {
        one_of.extend(many1(preceded(multispace0, parse_token_or_rule))(&input[1])?.1);
    } else {
        for seq in input {
            one_of.push(parse_rhs_seq(seq)?.1);
        }
    }
    Ok(Rhs { one_of })
}

fn main() -> anyhow::Result<()> {
    let html = [
        "https://docs.oracle.com/javase/specs/jls/se18/html/jls-4.html",
        "https://docs.oracle.com/javase/specs/jls/se18/html/jls-8.html",
        "https://docs.oracle.com/javase/specs/jls/se18/html/jls-9.html",
        "https://docs.oracle.com/javase/specs/jls/se18/html/jls-10.html",
        "https://docs.oracle.com/javase/specs/jls/se18/html/jls-15.html",
    ]
    .into_iter()
    .map(|url| ureq::get(url).call().unwrap().into_string().unwrap())
    .map(|html| Html::parse_document(&html));
    let selector = Selector::parse(".production").unwrap();
    let sel_lhs = Selector::parse(".lhs").unwrap();
    let sel_rhs = Selector::parse(".rhs").unwrap();
    let mut rules = HashMap::new();
    for d in html {
        for element in d.select(&selector) {
            let mut lhs = element
                .select(&sel_lhs)
                .next()
                .unwrap()
                .text()
                .collect::<String>();
            lhs.pop();
            let mut rhs_in = element.select(&sel_rhs);
            let mut rhs = vec![String::new()];
            for element in rhs_in.next().unwrap().children() {
                if matches!(element.value(), scraper::node::Node::Element(element) if element.name() == "br")
                {
                    if !rhs.last().unwrap().trim().is_empty() {
                        rhs.push(String::new());
                    } else {
                        rhs.last_mut().unwrap().clear();
                    }
                } else {
                    let sub = match element.value() {
                        scraper::node::Node::Element(_) => {
                            scraper::element_ref::ElementRef::wrap(element)
                                .unwrap()
                                .text()
                                .collect::<String>()
                        }
                        scraper::node::Node::Text(text) => text.text.clone().into(),
                        _ => panic!(),
                    };
                    rhs.last_mut().unwrap().push_str(&sub);
                }
            }
            if rhs.last().unwrap().trim().is_empty() {
                rhs.pop();
            }
            // eprintln!(
            //     "{:?} => {:?}",
            //     lhs.next().unwrap().text().collect::<String>(),
            //     rhs
            // );
            // eprintln!("{:?}", parse_rhs(&rhs).unwrap());
            // eprintln!("{lhs}");
            if lhs == "FloatingPointType" {
                eprintln!("{:#?}", rhs);
            }
            rules.insert(lhs, parse_rhs(&rhs).unwrap());
        }
    }
    for x in rules.values().flat_map(|r| r.one_of.iter()) {
        let mut v = vec![];
        deep_flatten(x, &mut v);
        for t in v
            .iter()
            .filter(|t| t.chars().next().unwrap().is_uppercase())
        {
            if !rules.contains_key(*t) {
                // eprintln!("Missing: {t}");
            }
        }
    }

    println!("pub struct Span {{}}");
    for (rule, production) in &rules {
        if production.one_of.len() == 1 {
            println!("pub struct {rule} {{");
            add_fields(&production.one_of[0]);
        } else {
            println!("pub enum {rule} {{");
            for e in &production.one_of {
                add_fields(e);
            }
        }
        println!("}}");
    }
    Ok(())
}

pub fn add_fields(p: &Element) {
    match p {
        Element::Multi(elements) => println!("    pub multi: Vec<({})>,", "missing"),
        Element::Sequence(elements) => {
            for e in elements {
                add_fields(e);
            }
        }
        Element::Optional(elements) => {
            println!("    pub optional: Option<{}>,", "missing");
        }
        Element::Terminal(terminal) if terminal.chars().next().unwrap().is_lowercase() => {
            println!("    pub {}: Span,", terminal);
        }
        Element::Terminal(terminal) if terminal.chars().next().unwrap().is_uppercase() => {
            println!("    pub {}: {},", terminal.to_snake_case(), terminal);
        }
        Element::Terminal(terminal) => {
            // Symbol
            let name = match terminal.as_str() {
                "{" => "left_curly_brace",
                "}" => "right_curly_brace",
                "[" => "left_bracket",
                "]" => "right_bracket",
                "(" => "left_brace",
                ")" => "right_brace",
                ";" => "semicolon",
                "--" => "minus_minus",
                "++" => "plus_plus",
                "<<" => "shl",
                ">>" => "shr",
                "<<<" => "rol",
                ">>>" => "ror",
                "&" => "ampersand",
                "<" => "less_than",
                ">" => "greater_than",
                ":" => "colon",
                "=" => "equals_sign",
                "==" => "equals",
                "!=" => "not_equals",
                "<=" => "lt_equals",
                ">=" => "gt_equals",
                "<<=" => "shl_assign",
                ">>=" => "shr_assign",
                ">>>=" => "ror_assign",
                "<<<=" => "rol_assign",
                "&=" => "and_assign",
                "|=" => "or_assign",
                "*=" => "mul_assign",
                "/=" => "div_assign",
                "-=" => "minus_assign",
                "+=" => "plus_assign",
                "%=" => "mod_assign",
                "^=" => "neg_assign",
                "::" => "colon_colon",
                "!" => "not",
                "|" => "pipe",
                "%" => "mod",
                "/" => "slash",
                "~" => "tilde",
                "." => "dot",
                "+" => "plus",
                "-" => "minus",
                "*" => "star",
                "@" => "at",
                "<>" => "diamond",
                "^" => "caret",
                "?" => "question_mark",
                "->" => "arrow_right",
                "&&" => "logical_and",
                "||" => "logical_or",
                "..." => "varargs",
                _ => panic!("{terminal}"),
            };
            println!("    pub {}: Span,", name);
        }
        _ => panic!("{p:?}"),
    }
}

fn deep_flatten<'a>(e: &'a Element, target: &mut Vec<&'a str>) {
    match e {
        Element::Optional(elements) | Element::Multi(elements) | Element::Sequence(elements) => {
            for e in elements {
                deep_flatten(e, target);
            }
        }
        Element::Terminal(text) => target.push(text),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_name_test() {
        let res =parse_rhs_seq("\n                              {EnumConstantModifier}\n                              Identifier\n                              [( [ArgumentList] )]\n                              [ClassBody]\n                            \n                     ").unwrap();
        panic!("{res:#?}");
        parse_rhs_seq("\n                              ( PrimitiveType )\n                              UnaryExpression ").unwrap();
        parse_rhs_seq("\n                        \n                              ( ReferenceType {AdditionalBound} )\n                              UnaryExpressionNotPlusMinus ").unwrap();
        parse_rhs_seq("\n                        \n                              ( ReferenceType {AdditionalBound} )\n                              LambdaExpression ").unwrap();
        parse_rhs_seq("\n                           NormalClassDeclaration \n\n                           EnumDeclaration \n\n                           RecordDeclaration\n                         \n                  ").unwrap();
        let res =parse_rhs_seq("\n                                    {\n                                    [ExplicitConstructorInvocation]\n                                    [BlockStatements]\n                                    }\n                                  \n                           ").unwrap();
        panic!("{res:#?}");
    }
}
