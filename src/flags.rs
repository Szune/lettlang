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

use std::ops::{BitAnd, BitOrAssign};

#[inline]
pub fn has<T>(value: T, flag: T) -> bool
where
    T: BitAnd<Output = T> + Copy + PartialEq + Eq,
{
    value & flag == flag
}

#[inline]
pub fn add<T>(value: &mut T, flag: T)
where
    T: BitOrAssign + Copy,
{
    *value |= flag;
}
