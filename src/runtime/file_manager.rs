use std::path::Path;


pub struct FileManager {
    pub(crate) module_search_paths: Vec<String>,
    pub(crate) file_search_path: Vec<String>,
}

impl FileManager {
    fn search_file(&self, name: &str, typ: &str, root: Option<&str>, paths: &[String]) -> Option<String> {
        let suffix = format!(".{}", typ);

        if Path::new(&format!("{}/{}", root.unwrap_or("."), name)).is_file() {
            return Some(format!("{}/{}", root.unwrap_or("."), name));
        } else if !name.ends_with(&suffix) && Path::new(&format!("{}{}.{}", root.unwrap_or(""), name, typ)).is_file() {
            return Some(format!("{}/{}.{}", root.unwrap_or("."), name, typ));
        }

        for path in paths {
            let path = format!("{}/{}", path, name);
            let path = Path::new(&path);

            if path.is_file() {
                return Some(path.to_str().unwrap().to_string());
            }
        }

        if !name.ends_with(&suffix) {
            let name_of_type = format!("{}{}", name, suffix);

            for path in paths {
                let path = format!("{}/{}", path, name_of_type);
                let path = Path::new(&path);

                if path.is_file() {
                    return Some(path.to_str().unwrap().to_string());
                }
            }
        }

        None
    }

    pub fn library_file_path(&self, name: &str, root: Option<&str>) -> Option<String> {
        self.search_file(name, "sld", root, &self.module_search_paths)
    }  

    pub fn file_path(&self, name: &str, root: Option<&str>) -> Option<String> {
        self.search_file(name, "scm", root, &self.file_search_path)
    }

    pub fn add_library_search_path(&mut self, path: String) {
        self.module_search_paths.push(path);
    }

    pub fn prepend_library_search_path(&mut self, path: String) {
        self.module_search_paths.insert(0, path);
    }

    pub fn add_file_search_path(&mut self, path: String) {
        self.file_search_path.push(path);
    }

    pub fn prepend_file_search_path(&mut self, path: String) {
        self.file_search_path.insert(0, path);
    }

    pub fn new() -> Self {
        Self {
            module_search_paths: vec![],
            file_search_path: vec![],
        }
    }
}