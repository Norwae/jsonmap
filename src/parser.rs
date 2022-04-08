use nom::branch::alt;
use nom::bytes::complete::{escaped, tag};
use nom::character::complete::{alpha1, digit1, none_of, space0};
use nom::combinator::{map_res, opt, recognize};
use nom::error::{Error as NomError, FromExternalError};
use nom::error::ErrorKind;
use nom::IResult;
use nom::multi::{many0, many1, separated_list1};
use nom::sequence::{delimited, terminated, tuple};

use crate::query as q;
use crate::query::Query;

pub trait QueryBuilder {
    fn construct_array(declarations: Vec<Box<dyn Query>>) -> Box<dyn Query>;
    fn construct_object(declarations: Vec<(&str, Box<dyn Query>)>) -> Box<dyn Query>;
    fn select_member(name: & str) -> Box<dyn Query>;
    fn select_index(idx: usize) -> Box<dyn Query>;
    fn and_then(a: Box<dyn Query>, b: Box<dyn Query>) -> Box<dyn Query>;
    fn constant_bool(v: bool) -> Box<dyn Query>;
    fn constant_nr(nr: f64) -> Box<dyn Query>;
    fn constant_string(string: &str) -> Box<dyn Query>;
    fn null() -> Box<dyn Query>;
    fn root() -> Box<dyn Query>;
    fn array_size() -> Box<dyn Query>;
}

#[derive(Debug, Default)]
struct DefaultQueryBuilder;

impl QueryBuilder for DefaultQueryBuilder {
    fn construct_array(declarations: Vec<Box<dyn Query>>) -> Box<dyn Query> {
        Box::new(q::construct_array(declarations))
    }

    fn construct_object(declarations: Vec<(&str, Box<dyn Query>)>) -> Box<dyn Query> {
        Box::new(q::construct_object(declarations))
    }

    fn select_member(name: &str) -> Box<dyn Query> {
        Box::new(q::select_member(name))
    }

    fn select_index(idx: usize) -> Box<dyn Query> {
        Box::new(q::select_index(idx))
    }

    fn and_then(a: Box<dyn Query>, b: Box<dyn Query>) -> Box<dyn Query> {
        Box::new(q::and_then(a, b))
    }

    fn constant_bool(v: bool) -> Box<dyn Query> {
        Box::new(q::constant_bool(v))
    }

    fn constant_nr(nr: f64) -> Box<dyn Query> {
        Box::new(q::constant_nr(nr))
    }

    fn constant_string(string: &str) -> Box<dyn Query> {
        Box::new(q::constant_string(string))
    }

    fn null() -> Box<dyn Query> {
        Box::new(q::null())
    }

    fn root() -> Box<dyn Query> {
        Box::new(q::root())
    }

    fn array_size() -> Box<dyn Query> {
        Box::new(q::array_size())
    }
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
    terminated(delimited(tag("["),map_res(
        digit1,
        |slice: &str| slice.parse::<usize>()
    ), tag("]")), space0)(i)
}

fn path_segment<QB: QueryBuilder>(i: &str) -> IResult<&str, Box<dyn Query>> {
    let (rest, (field, indices)) = tuple((name, many0(index)))(i)?;
    let select_name = QB::select_member(field);
    let q = indices.into_iter().fold(select_name, |query, next| {
        QB::and_then(query, QB::select_index(next))
    });

    Ok((rest, q))
}

fn path_segment_sep(i: &str) -> IResult<&str, &str> {
    terminated(tag("."), space0)(i)
}

fn path<QB: QueryBuilder>(i: &str) -> IResult<&str, Box<dyn Query>> {
    let (rest, mut qs) = separated_list1(path_segment_sep, path_segment::<QB>)(i)?;
    let mut result = qs.pop().unwrap();

    while let Some(q) = qs.pop() {
        result = QB::and_then(q, result)
    }

    Ok((rest, result))
}

pub fn parse_query(input: &str) -> Box<dyn Query> {
    parse_query_with::<DefaultQueryBuilder>(input)
}


pub fn parse_query_with<T: QueryBuilder>(input: &str) -> Box<dyn Query> {
    todo!()
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;
    use std::cell::RefCell;
    use std::mem::swap;
    use crate::model::JsonElement;
    use crate::parser::{path, QueryBuilder};
    use crate::query::{Query, QueryContext};


    #[derive(Clone,Debug,Eq, PartialEq)]
    struct QueryBuilderAction {
        method: &'static str,
        args: String
    }

    static mut ACTIONS: RefCell<Vec<QueryBuilderAction>> = RefCell::new(Vec::new());

    fn reset_actions() {
        unsafe {
            ACTIONS.borrow_mut().clear()
        }
    }

    fn snapshot_actions() -> Vec<QueryBuilderAction>{
        unsafe {
            let mut result = Vec::new();
            swap(&mut result, ACTIONS.borrow_mut().as_mut());
            result
        }
    }

    fn push_action<T : ToString>(method: &'static str, args: T) {
        let args = args.to_string();
        unsafe {
            ACTIONS.borrow_mut().push(QueryBuilderAction { method, args })
        }
    }


    struct TestUtil;
    impl Query for TestUtil {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            ctx.current_element
        }
    }

    impl QueryBuilder for TestUtil {
        fn construct_array(declarations: Vec<Box<dyn Query>>) -> Box<dyn Query> {
            push_action("construct_array", "");
            Box::new(TestUtil)
        }

        fn construct_object(declarations: Vec<(&str, Box<dyn Query>)>) -> Box<dyn Query> {
            push_action("construct_object", "");
            Box::new(TestUtil)
        }

        fn select_member(name: &str) -> Box<dyn Query> {
            push_action("select_member", name);
            Box::new(TestUtil)
        }

        fn select_index(idx: usize) -> Box<dyn Query> {
            push_action("select_index", format!("{}", idx));
            Box::new(TestUtil)
        }

        fn and_then(a: Box<dyn Query>, b: Box<dyn Query>) -> Box<dyn Query> {
            push_action("and_then", "");
            Box::new(TestUtil)
        }

        fn constant_bool(v: bool) -> Box<dyn Query> {
            push_action("constant_bool", v);
            Box::new(TestUtil)
        }

        fn constant_nr(nr: f64) -> Box<dyn Query> {
            push_action("constant_nr", nr);
            Box::new(TestUtil)
        }

        fn constant_string(string: &str) -> Box<dyn Query> {
            push_action("constant_string", string);
            Box::new(TestUtil)
        }

        fn null() -> Box<dyn Query> {
            push_action("null", "");
            Box::new(TestUtil)
        }

        fn root() -> Box<dyn Query> {
            push_action("root", "");
            Box::new(TestUtil)
        }

        fn array_size() -> Box<dyn Query> {
            push_action("array_size", "");
            Box::new(TestUtil)
        }
    }


    #[test]
    fn parse_simple_path() {
        assert!(path::<TestUtil>("a.b.c").is_ok());
        assert_eq!(snapshot_actions(), vec![
            QueryBuilderAction { method: "select_member", args: "a".to_string() },
            QueryBuilderAction { method: "select_member", args: "b".to_string() },
            QueryBuilderAction { method: "select_member", args: "c".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
        ]);
    }

    #[test]
    fn parse_with_lots_of_whitespace(){
        assert!(path::<TestUtil>("d  .   e .  f  ").is_ok());
        assert_eq!(snapshot_actions(), vec![
            QueryBuilderAction { method: "select_member", args: "d".to_string() },
            QueryBuilderAction { method: "select_member", args: "e".to_string() },
            QueryBuilderAction { method: "select_member", args: "f".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
        ]);
    }


    #[test]
    fn parse_quoted_field() {
        assert!(path::<TestUtil>("a.\"complex quoted field name, including special characters like . or []\".c").is_ok());
        assert_eq!(snapshot_actions(), vec![
            QueryBuilderAction { method: "select_member", args: "a".to_string() },
            QueryBuilderAction { method: "select_member", args: "complex quoted field name, including special characters like . or []".to_string() },
            QueryBuilderAction { method: "select_member", args: "c".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
            QueryBuilderAction { method: "and_then", args: "".to_string() },
        ]);
    }
}
