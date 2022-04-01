use std::borrow::{Borrow, Cow};
use std::ops::BitAnd;
use crate::model::JsonElement;

#[derive(Debug, Clone)]
pub struct QueryContext<'a> {
    current_element: Cow<'a, JsonElement>,
    root_element: &'a JsonElement
}

#[derive(Eq, PartialEq, Debug, Clone, Copy)]
pub struct Safety {
    pub unbounded_size: bool
}

static SAFE: Safety = Safety { unbounded_size: true};

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

fn select_index(idx: usize) -> impl Query {
    struct SelectIndex(usize);

    impl Query for SelectIndex {
        fn perform<'a>(&self, ctx: QueryContext<'a>) -> Cow<'a, JsonElement> {
            match ctx.current_element {
                Cow::Borrowed(JsonElement::Array(vec)) if self.0 < vec.len() => {
                    Cow::Borrowed(&vec[self.0])
                }
                Cow::Owned(JsonElement::Array(mut vec)) if self.0 < vec.len() => {
                    let elem = vec.swap_remove(self.0);
                    Cow::Owned(elem)
                }
                _ => Cow::Owned(JsonElement::Null)
            }
        }
    }
    SelectIndex(idx)
}

fn and_then<A: Query, B: Query>(a: A, b: B) -> impl Query {
    struct Compose<A, B>(A, B);
    impl <A: Query, B: Query> Query for Compose<A, B> {
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

            assert_destructures_into!(and_then(root(), constant_bool(true)).perform(ctx), JsonElement::Bool(true))
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

            assert_destructures_into!(select_index(2).perform(ctx), JsonElement::Number(nr), (nr - 18f64).abs() < 1e-40)
        }
    }
}