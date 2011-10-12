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
package fr.jamgotchian.abcd.core.controlflow;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public enum ParentType {
    UNDEFINED,
    ROOT,
    TRIVIAL,
    BASIC_BLOCK,
    IF_THEN_ELSE,
    IF_THEN,
    IF_NOT_THEN,
    SEQUENCE,
    SINGLE_EXIT_LOOP,
    SINGLE_EXIT_LOOP_INVERTED_COND,
    BREAK_LABEL,
    TRY_CATCH_FINALLY,
    INLINED_FINALLY,
    SWITCH_CASE
}
