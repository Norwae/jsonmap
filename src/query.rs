use std::borrow::Cow;
use std::ops::BitAnd;
use crate::model::JsonElement;

#[derive(Debug, Clone)]
pub struct QueryContext<'a> {
    current_element: Cow<'a, JsonElement>,
    root_element: &'a JsonElement
}

impl <'a> From<&'a JsonElement> for QueryContext<'a> {
    fn from(json: &'a JsonElement) -> Self {
        QueryContext {
            current_element: Cow::Borrowed(json),
            root_element: json
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct Safety {
    unbounded_size: bool
}

impl Safety {
    fn is_safe(&self) -> bool {
        !self.unbounded_size
    }
}

static SAFE: Safety = Safety { unbounded_size: false };

impl BitAnd for Safety {
    type Output = Safety;

    fn bitand(self, rhs: Self) -> Self::Output {
        Safety { unbounded_size: self.unbounded_size | rhs.unbounded_size}
    }
}

pub trait Query {
    fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement>;
    fn safety(&self) -> Safety {
        SAFE
    }
}

struct Const(JsonElement);

impl Query for Const {
    fn perform<'a>(&self, _ctx: QueryContext<'a>) -> Cow<'a, JsonElement> {
        Cow::Owned(self.0.clone())
    }
}

fn construct_array(declarations: Vec<Box<dyn Query>>) -> impl Query {
    impl Query for Vec<Box<dyn Query>> {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            let results = self.iter().map(|q|q.perform(ctx.clone()).into_owned()).collect();

            Cow::Owned(JsonElement::Array(results))
        }

        fn safety(&self) -> Safety {
            self.iter().fold(SAFE, |previous_safe, next| previous_safe & next.safety())
        }
    }

    declarations
}

fn construct_object<'a>(declarations: Vec<(&'a str, Box<dyn Query>)>) -> impl Query + 'a {
    impl <'name> Query for Vec<(&'name str, Box<dyn Query>)> {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            let key_value_pairs = self.iter().map(|tpl|{
                let (k, query) = tpl;
                (k.to_string(), query.perform(ctx.clone()).into_owned())
            }).collect();

            Cow::Owned(JsonElement::Object(key_value_pairs))
        }

        fn safety(&self) -> Safety {
            self.iter().fold(SAFE, |previous_safe, next| previous_safe & next.1.safety())
        }
    }

    declarations
}

fn select_member<'a>(name: &'a str) -> impl Query + 'a  {
    impl <'name> Query for &'name str {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            match ctx.current_element {
                Cow::Borrowed(JsonElement::Object(vec)) => {
                    vec.iter()
                        .find(|e| &e.0.as_str() == self)
                        .map(|t|Cow::Borrowed(&t.1))
                        .unwrap_or(Cow::Owned(JsonElement::Null))
                }
                Cow::Owned(JsonElement::Object(vec)) => {
                    vec.into_iter()
                        .find(|e| &e.0.as_str() == self)
                        .map(|t|Cow::Owned(t.1))
                        .unwrap_or(Cow::Owned(JsonElement::Null))
                }
                _ => Cow::Owned(JsonElement::Null)
            }
        }
    }
    name
}

fn select_index(idx: usize) -> impl Query {
    impl Query for usize {
        fn perform<'a>(&self, ctx: QueryContext<'a>) -> Cow<'a, JsonElement> {
            match ctx.current_element {
                Cow::Borrowed(JsonElement::Array(vec)) if *self < vec.len() => {
                    Cow::Borrowed(&vec[*self])
                }
                Cow::Owned(JsonElement::Array(mut vec)) if *self < vec.len() => {
                    Cow::Owned(vec.swap_remove(*self))
                }
                _ => Cow::Owned(JsonElement::Null)
            }
        }
    }
    idx
}

fn and_then(a: Box<dyn Query>, b: Box<dyn Query>) -> impl Query {
    struct Compose(Box<dyn Query>, Box<dyn Query>);
    impl Query for Compose {
        fn perform<'a>(&self, ctx: QueryContext<'a>) -> Cow<'a, JsonElement> {
            let root_element = ctx.root_element;
            let current_element = self.0.perform(ctx);
            let ctx2 = QueryContext { current_element, root_element};
            self.1.perform(ctx2)
        }

        fn safety(&self) -> Safety {
            self.0.safety() & self.1.safety()
        }
    }

    Compose(a, b)
}

fn constant_bool(v: bool) -> impl Query {
    Const(JsonElement::Bool(v))
}

fn constant_nr(nr: f64) -> impl Query {
    Const(JsonElement::Number(nr))
}

fn constant_string(string: &str) -> impl Query {
    Const(JsonElement::Text(string.to_string()))
}

fn null() -> impl Query {
    Const(JsonElement::Null)
}

fn root() -> impl Query {
    struct RootQuery;
    impl Query for RootQuery {
        fn perform<'a>(&self, ctx: QueryContext<'a>) -> Cow<'a, JsonElement>{
            Cow::Borrowed(ctx.root_element)
        }

        fn safety(&self) -> Safety {
            Safety { unbounded_size: true }
        }
    }

    RootQuery
}

#[cfg(test)]
mod tests {
    macro_rules! assert_destructures_into {
        ($actual:expr, $target:pat) => {
            assert_destructures_into!($actual, $target, true)
        };
        ($actual:expr, $target:pat, $guard:expr) => {
            if let $target = ($actual).into_owned() {
                assert!($guard)
            } else {
                assert!(false)
            }
        }
    }

    mod constants {
        use crate::query::*;
        use crate::model::JsonElement;

        #[test]
        fn apply_constants(){
            let input = JsonElement::Array(vec![
                JsonElement::Object(
                    vec![("a".to_string(), JsonElement::Null)]
                ),
                JsonElement::Bool(true),
                JsonElement::Number(18f64)
            ]);
            let ctx = QueryContext {
                current_element: Cow::Borrowed(&input),
                root_element: &input
            };

            assert_destructures_into!(null().perform(ctx.clone()), JsonElement::Null);
            assert_destructures_into!(constant_bool(true).perform(ctx.clone()), JsonElement::Bool(true));
            assert_destructures_into!(constant_string("abc").perform(ctx.clone()), JsonElement::Text(txt), txt == "abc");
            assert_destructures_into!(constant_nr(19f64).perform(ctx), JsonElement::Number(n), n == 19f64);
        }
    }
    mod basics {
        use crate::query::*;

        #[test]
        fn compose() {
            let input = JsonElement::Array(vec![
                JsonElement::Object(
                    vec![("a".to_string(), JsonElement::Null)]
                ),
                JsonElement::Bool(true),
                JsonElement::Number(18f64)
            ]);
            let ctx = QueryContext {
                current_element: Cow::Borrowed(&input),
                root_element: &input
            };

            assert_destructures_into!(and_then(Box::new(root()), Box::new(constant_bool(true))).perform(ctx), JsonElement::Bool(true))
        }

        #[test]
        fn index_projection() {
            let input = JsonElement::Array(vec![
                JsonElement::Object(
                    vec![("a".to_string(), JsonElement::Null)]
                ),
                JsonElement::Bool(true),
                JsonElement::Number(18f64)
            ]);
            let ctx = QueryContext {
                current_element: Cow::Borrowed(&input),
                root_element: &input
            };

            assert_destructures_into!(select_index(2).perform(ctx), JsonElement::Number(nr), (nr - 18f64).abs() < 1e-40);
            let ctx = QueryContext {
                current_element: Cow::Owned(input.clone()),
                root_element: &input
            };
            assert_destructures_into!(select_index(2).perform(ctx), JsonElement::Number(nr), (nr - 18f64).abs() < 1e-40)
        }

        #[test]
        fn member_projection() {
            let input = JsonElement::Object(
                    vec![("a".to_string(), JsonElement::Null),
                         ("c".to_string(), JsonElement::Text("Moo".to_string())),
                         ("b".to_string(), JsonElement::Text("Bar".to_string())),
                    ]
                );
            let ctx = QueryContext {
                current_element: Cow::Borrowed(&input),
                root_element: &input
            };

            assert_destructures_into!(select_member("c").perform(ctx), JsonElement::Text(str), "Moo" == str);
            let ctx = QueryContext {
                current_element: Cow::Owned(input.clone()),
                root_element: &input
            };

            assert_destructures_into!(select_member("c").perform(ctx), JsonElement::Text(str), "Moo" == str);
        }

        #[test]
        fn construction(){

            let input = JsonElement::Array(vec![
                JsonElement::Object(
                    vec![("a".to_string(), JsonElement::Null)]
                ),
                JsonElement::Bool(true),
                JsonElement::Number(18f64)
            ]);
            let ctx = QueryContext::from(&input);
            assert_destructures_into!(construct_array(vec![Box::new(null()), Box::new(constant_bool(true))]).perform(ctx.clone()),
                JsonElement::Array(elems),
                if let JsonElement::Bool(true) = elems[1] { if let JsonElement::Null = elems[0] { true } else { false }  } else { false}
            );

            assert_destructures_into!(construct_object(vec![("foo", Box::new(null()))]).perform(ctx.clone()),
                JsonElement::Object(elems), elems[0].0 == "foo")
        }
    }
    mod safety {
        use crate::query::*;

        #[test]
        fn save_by_default() {
            impl Query for () {
                fn perform<'q>(&self, _ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
                    unreachable!()
                }
            }

            assert!(().safety().is_safe())
        }
        #[test]
        fn unsafe_root_reference() {
            assert!(!root().safety().is_safe())
        }

        #[test]
        fn conditionally_save_composition() {
            assert!(!and_then(Box::new(root()), Box::new(constant_bool(false))).safety().is_safe());
            assert!(and_then(Box::new(constant_string("foo")), Box::new(constant_bool(false))).safety().is_safe())
        }

        #[test]
        fn conditionally_save_array_construct() {
            assert!(!construct_array(vec![Box::new(root()), Box::new(constant_bool(false))]).safety().is_safe());
            assert!(construct_array(vec![Box::new(constant_string("bar")), Box::new(constant_bool(false))]).safety().is_safe());
        }

        #[test]
        fn conditionally_save_object_construct() {

            assert!(!construct_object(vec![("a", Box::new(root())), ("b", Box::new(constant_bool(false)))]).safety().is_safe());
            assert!(construct_object(vec![("a", Box::new(constant_string("baz"))), ("b", Box::new(constant_bool(false)))]).safety().is_safe());
        }
    }
}