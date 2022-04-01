#[derive(Debug, Clone, PartialEq)]
pub enum JsonElement {
    Null,
    Bool(bool),
    Number(f64),
    Text(String),
    Array(Vec<JsonElement>),
    Object(Vec<(String, JsonElement)>)
}
