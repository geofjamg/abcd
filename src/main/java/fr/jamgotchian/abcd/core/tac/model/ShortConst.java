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
package fr.jamgotchian.abcd.core.tac.model;

import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ShortConst implements Const {

    private final short value;

    public ShortConst(short value) {
        this.value = value;
    }

    public short getValue() {
        return value;
    }

    public Set<JavaType> getPossibleTypes() {
        Set<JavaType> types = new HashSet<JavaType>(2);
        types.add(JavaType.SHORT);
        if (value == 0 || value == 1) {
            types.add(JavaType.BOOLEAN);
        }
        return Collections.unmodifiableSet(types);
    }

    @Override
    public String toString() {
        return Short.toString(value);
    }
}
