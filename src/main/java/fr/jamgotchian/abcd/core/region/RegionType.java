/*
 * Copyright (C) 2011 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.region;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public enum RegionType {
    BASIC_BLOCK,
    BLOCK,
    IF_THEN,
    IF_THEN_ELSE_JOIN,
    IF_THEN_BREAK_ELSE_BREAK,
    SWITCH_CASE,
    INFINITE_LOOP,
    DO_WHILE_LOOP,
    WHILE_LOOP,
    TRY_CATCH_FINALLY,
    LABELED,
    EMPTY
}
