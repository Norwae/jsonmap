use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

use nom::branch::alt;
use nom::bytes::complete::{escaped, tag};
use nom::character::complete::{alpha1, digit1, none_of, space0};
use nom::combinator::{map, map_res, opt, recognize};
use nom::Err;
use nom::error::Error as NomError;
use nom::IResult;
use nom::multi::{many0, many1, separated_list1};
use nom::number::complete::{double, f64, float};
use nom::sequence::{delimited, terminated, tuple};

use crate::query as q;
use crate::query::Query;

#[derive(Debug)]
pub struct ParseError {
    message: String
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("ParseError(")?;
        f.write_str(&self.message)?;
        f.write_str(")")
    }
}

impl Error for ParseError {}

impl <A: Debug> From<Err<A>> for ParseError {
    fn from(e: Err<A>) -> Self {
        ParseError {
            message: format!("Nom Error: {:?}", e)
        }
    }
}

pub trait QueryBuilder {
    fn construct_array(&mut self, declarations: Vec<Box<dyn Query>>) -> Box<dyn Query>;
    fn construct_object(&mut self, declarations: Vec<(&str, Box<dyn Query>)>) -> Box<dyn Query>;
    fn select_member(&mut self, name: &str) -> Box<dyn Query>;
    fn select_index(&mut self, idx: usize) -> Box<dyn Query>;
    fn and_then(&mut self, a: Box<dyn Query>, b: Box<dyn Query>) -> Box<dyn Query>;
    fn constant_bool(&mut self, v: bool) -> Box<dyn Query>;
    fn constant_nr(&mut self, nr: f64) -> Box<dyn Query>;
    fn constant_string(&mut self, string: &str) -> Box<dyn Query>;
    fn null(&mut self) -> Box<dyn Query>;
    fn root(&mut self) -> Box<dyn Query>;
    fn array_size(&mut self) -> Box<dyn Query>;
}

#[derive(Debug, Default)]
struct DefaultQueryBuilder;

impl QueryBuilder for DefaultQueryBuilder {
    fn construct_array(&mut self, declarations: Vec<Box<dyn Query>>) -> Box<dyn Query> {
        Box::new(q::construct_array(declarations))
    }

    fn construct_object(&mut self, declarations: Vec<(&str, Box<dyn Query>)>) -> Box<dyn Query> {
        Box::new(q::construct_object(declarations))
    }

    fn select_member(&mut self, name: &str) -> Box<dyn Query> {
        Box::new(q::select_member(name))
    }

    fn select_index(&mut self, idx: usize) -> Box<dyn Query> {
        Box::new(q::select_index(idx))
    }

    fn and_then(&mut self, a: Box<dyn Query>, b: Box<dyn Query>) -> Box<dyn Query> {
        Box::new(q::and_then(a, b))
    }

    fn constant_bool(&mut self, v: bool) -> Box<dyn Query> {
        Box::new(q::constant_bool(v))
    }

    fn constant_nr(&mut self, nr: f64) -> Box<dyn Query> {
        Box::new(q::constant_nr(nr))
    }

    fn constant_string(&mut self, string: &str) -> Box<dyn Query> {
        Box::new(q::constant_string(string))
    }

    fn null(&mut self) -> Box<dyn Query> {
        Box::new(q::null())
    }

    fn root(&mut self) -> Box<dyn Query> {
        Box::new(q::root())
    }

    fn array_size(&mut self) -> Box<dyn Query> {
        Box::new(q::array_size())
    }
}

fn literal<'s, QB: QueryBuilder>(qb: &RefCell<QB>, i: &'s str) -> IResult<&'s str, Box<dyn Query>> {
    alt((
        map(double, |f: f64| qb.borrow_mut().constant_nr(f)),
        map(delimited(tag("'"), recognize(many1(none_of("'"))), tag("'")), |str| qb.borrow_mut().constant_string(str)),
        map(tag("true"), |_| qb.borrow_mut().constant_bool(true)),
        map(tag("false"), |_| qb.borrow_mut().constant_bool(false)),
        map(tag("null"), |_| qb.borrow_mut().null()),
        map(tag("@"), |_| qb.borrow_mut().root()),
    ))(i)
}

fn simple_name(i: &str) -> IResult<&str, &str> {
    alpha1(i)
}

fn quoted_name(i: &str) -> IResult<&str, &str> {
    delimited(tag("\""), recognize(many1(none_of("\""))), tag("\""))(i)
}

fn name(i: &str) -> IResult<&str, &str> {
    terminated(alt((simple_name, quoted_name)), space0)(i)
}

fn index(i: &str) -> IResult<&str, usize> {
    terminated(delimited(tag("["), map_res(
        digit1,
        |slice: &str| slice.parse::<usize>(),
    ), tag("]")), space0)(i)
}

fn path_segment<'s, QB: QueryBuilder>(qb: &RefCell<QB>, i: &'s str) -> IResult<&'s str, Box<dyn Query>> {
    let (rest, (field, indices)) = tuple((name, many0(index)))(i)?;
    let select_name = qb.borrow_mut().select_member(field);
    let q = indices.into_iter().fold(select_name, |query, next| {
        qb.borrow_mut().and_then(query, qb.borrow_mut().select_index(next))
    });

    Ok((rest, q))
}

fn path_segment_sep(i: &str) -> IResult<&str, &str> {
    terminated(tag("."), space0)(i)
}

fn path<'s, QB: QueryBuilder>(qb: &RefCell<QB>, i: &'s str) -> IResult<&'s str, Box<dyn Query>> {
    let (rest, mut qs) = separated_list1(path_segment_sep, |input| path_segment(qb, input))(i)?;
    let mut result = qs.pop().unwrap();

    while let Some(q) = qs.pop() {
        result = qb.borrow_mut().and_then(q, result)
    }

    Ok((rest, result))
}

pub fn parse_query(input: &str) -> Result<Box<dyn Query>, ParseError> {
    parse_query_with(DefaultQueryBuilder, input)
}


pub fn parse_query_with<T: QueryBuilder>(qb: T, input: &str) -> Result<Box<dyn Query>, ParseError>  {
    let qb = RefCell::new(qb);
    let (rest, parsed) = alt((
        |s|literal(&qb, s),
        |s|path(&qb, s)
    ))(input)?;

    if rest.trim() == "" {
        Ok(parsed)
    } else {
        Err(ParseError{
            message: format!("Trailing input could not be parsed, found {}", rest)
        })
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::cell::RefCell;
    use std::fmt::{Display, format};
    use std::mem::swap;

    use crate::model::JsonElement;
    use crate::parser::{literal, path, QueryBuilder};
    use crate::query::{Query, QueryContext};

    #[derive(Clone, Debug, Eq, PartialEq)]
    struct QueryBuilderAction {
        method: &'static str,
        args: String,
    }


    struct TestUtil(Vec<QueryBuilderAction>);

    struct NilQuery;

    impl Query for NilQuery {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            ctx.current_element
        }
    }

    impl TestUtil {
        fn new() -> RefCell<Self> {
            RefCell::new(TestUtil(Vec::new()))
        }

        fn push_action<T: Display>(&mut self, method: &'static str, args: T) {
            self.0.push(QueryBuilderAction { method, args: format!("{}", args) })
        }
    }

    impl QueryBuilder for TestUtil {
        fn construct_array(&mut self, declarations: Vec<Box<dyn Query>>) -> Box<dyn Query> {
            self.push_action("construct_array", "");
            Box::new(NilQuery)
        }

        fn construct_object(&mut self, declarations: Vec<(&str, Box<dyn Query>)>) -> Box<dyn Query> {
            self.push_action("construct_object", "");
            Box::new(NilQuery)
        }

        fn select_member(&mut self, name: &str) -> Box<dyn Query> {
            self.push_action("select_member", name);
            Box::new(NilQuery)
        }

        fn select_index(&mut self, idx: usize) -> Box<dyn Query> {
            self.push_action("select_index", format!("{}", idx));
            Box::new(NilQuery)
        }

        fn and_then(&mut self, a: Box<dyn Query>, b: Box<dyn Query>) -> Box<dyn Query> {
            self.push_action("and_then", "");
            Box::new(NilQuery)
        }

        fn constant_bool(&mut self, v: bool) -> Box<dyn Query> {
            self.push_action("constant_bool", v);
            Box::new(NilQuery)
        }

        fn constant_nr(&mut self, nr: f64) -> Box<dyn Query> {
            self.push_action("constant_nr", nr);
            Box::new(NilQuery)
        }

        fn constant_string(&mut self, string: &str) -> Box<dyn Query> {
            self.push_action("constant_string", string);
            Box::new(NilQuery)
        }

        fn null(&mut self) -> Box<dyn Query> {
            self.push_action("null", "");
            Box::new(NilQuery)
        }

        fn root(&mut self) -> Box<dyn Query> {
            self.push_action("root", "");
            Box::new(NilQuery)
        }

        fn array_size(&mut self) -> Box<dyn Query> {
            self.push_action("array_size", "");
            Box::new(NilQuery)
        }
    }


    #[test]
    fn parse_simple_path() {
        let mut test = TestUtil::new();
        assert!(path(&test, "a.b.c").is_ok());
        assert_eq!(test.into_inner().0, vec![
            QueryBuilderAction { method: "select_member", args: "a".to_string() },
            QueryBuilderAction { method: "select_member", args: "b".to_string() },
            QueryBuilderAction { method: "select_member", args: "c".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
        ]);
    }

    #[test]
    fn parse_with_lots_of_whitespace() {
        let mut test = TestUtil::new();
        assert!(path(&mut test, "d  .   e .  f  ").is_ok());
        assert_eq!(test.into_inner().0, vec![
            QueryBuilderAction { method: "select_member", args: "d".to_string() },
            QueryBuilderAction { method: "select_member", args: "e".to_string() },
            QueryBuilderAction { method: "select_member", args: "f".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
        ]);
    }


    #[test]
    fn parse_quoted_field() {
        let mut test = TestUtil::new();
        assert!(path(&mut test, "a.\"complex quoted field name, including special characters like . or []\".c").is_ok());
        assert_eq!(test.into_inner().0, vec![
            QueryBuilderAction { method: "select_member", args: "a".to_string() },
            QueryBuilderAction { method: "select_member", args: "complex quoted field name, including special characters like . or []".to_string() },
            QueryBuilderAction { method: "select_member", args: "c".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
        ]);
    }

    #[test]
    fn parse_literals() {
        let mut test = TestUtil::new();
        assert!(literal(&test, "true").is_ok());
        assert!(literal(&test, "false").is_ok());
        assert!(literal(&test, "16").is_ok());
        assert!(literal(&test, "-17.3e-12").is_ok());
        assert!(literal(&test, "'Cheese Borger'").is_ok());
        assert!(literal(&test, "null").is_ok());
        assert!(literal(&test, "@").is_ok());

        assert_eq!(test.into_inner().0, vec![
            QueryBuilderAction { method: "constant_bool", args: "true".to_string() },
            QueryBuilderAction { method: "constant_bool", args: "false".to_string() },
            QueryBuilderAction { method: "constant_nr", args: "16".to_string() },
            QueryBuilderAction { method: "constant_nr", args: "-0.0000000000173".to_string() },
            QueryBuilderAction { method: "constant_string", args: "Cheese Borger".to_string() },
            QueryBuilderAction { method: "null", args: "".to_string() },
            QueryBuilderAction { method: "root", args: "".to_string() },
        ])
    }
}
