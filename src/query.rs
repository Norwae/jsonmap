use std::borrow::Cow;
use std::ops::BitAnd;
use crate::model::JsonElement;

#[derive(Debug, Clone)]
pub struct QueryContext<'a> {
    pub(crate) current_element: Cow<'a, JsonElement>,
    pub(crate) root_element: &'a JsonElement
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

pub fn construct_array(declarations: Vec<Box<dyn Query>>) -> impl Query {
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

pub fn construct_object(declarations: Vec<(&str, Box<dyn Query>)>) -> impl Query {
    struct ConstructObject(Vec<(String, Box<dyn Query>)>);
    impl Query for ConstructObject {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            let key_value_pairs = self.0.iter().map(|tpl|{
                let (k, query) = tpl;
                (k.clone(), query.perform(ctx.clone()).into_owned())
            }).collect();

            Cow::Owned(JsonElement::Object(key_value_pairs))
        }

        fn safety(&self) -> Safety {
            self.0.iter().fold(SAFE, |previous_safe, next| previous_safe & next.1.safety())
        }
    }

    ConstructObject(declarations.into_iter().map(|(n, q)|(n.to_string(), q)).collect())
}

pub fn select_member(name: &str) -> impl Query  {
    struct SelectMember(String);
    impl  Query for SelectMember {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            match ctx.current_element {
                Cow::Borrowed(JsonElement::Object(vec)) => {
                    vec.iter()
                        .find(|(next, _)| next.as_str() == self.0)
                        .map(|t|Cow::Borrowed(&t.1))
                        .unwrap_or(Cow::Owned(JsonElement::Null))
                }
                Cow::Owned(JsonElement::Object(vec)) => {
                    vec.into_iter()
                        .find(|(e, _)| e.as_str() == self.0)
                        .map(|t|Cow::Owned(t.1))
                        .unwrap_or(Cow::Owned(JsonElement::Null))
                }
                _ => Cow::Owned(JsonElement::Null)
            }
        }
    }
    SelectMember(name.to_string())
}

pub fn select_index(idx: usize) -> impl Query {
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

pub fn and_then(a: Box<dyn Query>, b: Box<dyn Query>) -> impl Query {
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

pub fn constant_bool(v: bool) -> impl Query {
    Const(JsonElement::Bool(v))
}

pub fn constant_nr(nr: f64) -> impl Query {
    Const(JsonElement::Number(nr))
}

pub fn constant_string(string: &str) -> impl Query {
    Const(JsonElement::Text(string.to_string()))
}

pub fn null() -> impl Query {
    Const(JsonElement::Null)
}

pub fn root() -> impl Query {
    struct Root;
    impl Query for Root {
        fn perform<'a>(&self, ctx: QueryContext<'a>) -> Cow<'a, JsonElement>{
            Cow::Borrowed(ctx.root_element)
        }

        fn safety(&self) -> Safety {
            Safety { unbounded_size: true }
        }
    }

    Root
}

pub fn array_size() -> impl Query {
    struct ArraySize;
    impl Query for ArraySize {
        fn perform<'q>(&self, ctx: QueryContext<'q>) -> Cow<'q, JsonElement> {
            match ctx.current_element.as_ref() {
                JsonElement::Array(vec) => Cow::Owned(JsonElement::Number(vec.len() as f64)),
                _ => Cow::Owned(JsonElement::Null)
            }
        }
    }
    ArraySize
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
    mod example {
        use crate::query::*;

        #[test]
        fn flatten_address() {
            let input = JsonElement::Object(vec![
                ("person".to_string(), JsonElement::Object(vec![
                    ("first_names".to_string(), JsonElement::Text("Lizzy".to_string())),
                    ("last_names".to_string(), JsonElement::Text("Sparks".to_string()))
                ])),
                ("addresses".to_string(), JsonElement::Array(vec![
                    JsonElement::Object(vec![
                        ("street".to_string(), JsonElement::Text("910 Howizzer Lane".to_string()))
                    ])
                ]))
            ]);
            let transform =
                construct_object(vec![
                    ("firstname", Box::new(and_then(Box::new(select_member("person")), Box::new(select_member("first_names"))))),
                    ("lastname", Box::new(and_then(Box::new(select_member("person")), Box::new(select_member("last_names"))))),
                    ("street", Box::new(and_then(Box::new(select_member("addresses")), Box::new(and_then(
                        Box::new(select_index(0)),
                        Box::new(select_member("street")))))
                    )),
                    ("nr_of_addrs", Box::new(and_then(Box::new(select_member("addresses")), Box::new(array_size()))))
                ]);

            let transformed = transform.perform(QueryContext::from(&input)).into_owned();
            assert_eq!(transformed, JsonElement::Object(vec![
                ("firstname".to_string(), JsonElement::Text("Lizzy".to_string())),
                ("lastname".to_string(), JsonElement::Text("Sparks".to_string())),
                ("street".to_string(), JsonElement::Text("910 Howizzer Lane".to_string())),
                ("nr_of_addrs".to_string(), JsonElement::Number(1f64))
            ]));
        }
    }
}