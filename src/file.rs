//
// Lettlang is a programming language
// Copyright (C) 2022  Carl Erik Patrik Iwarson
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

use std::collections::HashMap;

pub struct LettlangFile {
    pub id: u64,
    pub name: String,
    pub content: String,
}

impl LettlangFile {
    pub fn new(id: u64, name: String, content: String) -> Self {
        Self { id, name, content }
    }
}

pub struct FileTracker {
    map: HashMap<u64, LettlangFile>,
}

impl FileTracker {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn add(&mut self, file: LettlangFile) {
        self.map.insert(file.id, file);
    }

    pub fn get(&self, id: u64) -> Option<&LettlangFile> {
        self.map.get(&id)
    }
}
