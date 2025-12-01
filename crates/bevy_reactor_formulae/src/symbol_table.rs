use bevy::platform::collections::HashMap;

#[derive(Default, Debug)]
pub struct SymbolTable<T> {
    by_name: HashMap<&'static str, usize>,
    by_index: Vec<T>,
}

impl<T> SymbolTable<T> {
    pub fn empty() -> Self {
        Self {
            by_name: HashMap::default(),
            by_index: Vec::new(),
        }
    }

    pub fn insert(&mut self, name: &'static str, value: T) -> usize {
        let id = self.by_index.len();
        self.by_name.insert(name, id);
        self.by_index.push(value);
        id
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.by_index.get(index)
    }
}
