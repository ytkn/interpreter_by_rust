#[derive(Clone)]
pub enum Object {
    INTEGER(i32),
    BOOLEAN(bool),
    RERUTN(Box<Object>),
    NULL,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::INTEGER(x) => x.to_string(),
            Object::BOOLEAN(x) => x.to_string(),
            Object::RERUTN(x) => x.inspect(),
            Object::NULL => "null".to_string(),
        }
    }
    pub fn object_type(&self) -> String {
        match self {
            Object::INTEGER(_) => "INTEGER".to_string(),
            Object::BOOLEAN(_) => "BOOLEAN".to_string(),
            Object::RERUTN(_) => "RERUTN".to_string(),
            Object::NULL => "NULL".to_string(),
        }
    }
}
