use std::collections::HashMap;

pub struct SourceManager {
    pub sources: Vec<String>,
    pub source_ids: HashMap<String, usize>,
}

impl SourceManager {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
            source_ids: HashMap::new(),
        }
    }

    pub fn add_source(&mut self, source: String) -> usize {
        if let Some(id) = self.source_ids.get(&source) {
            *id
        } else {
            let id = self.sources.len();
            self.sources.push(source);
            self.source_ids.insert(self.sources[id].clone(), id);
            id
        }
    }

    pub fn get_source(&self, id: usize) -> &str {
        &self.sources[id]
    }

    pub fn source_id(&self, source: &str) -> Option<usize> {
        self.source_ids.get(source).copied()
    }

    pub fn read_source(&mut self, path: &str) -> Result<(usize, &str), std::io::Error> {
        if let Some(id) = self.source_id(path) {
            Ok((id, self.get_source(id)))
        } else {
            if let Ok(source) = std::fs::read_to_string(path) {
                let id = self.add_source(source);
                Ok((id, self.get_source(id)))
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "File not found",
                ))
            }
        }
    }
}