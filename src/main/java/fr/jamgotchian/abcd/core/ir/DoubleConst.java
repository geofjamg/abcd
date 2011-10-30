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
package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Collections;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DoubleConst implements Const {

    private final double value;

    public DoubleConst(double value) {
        this.value = value;
    }

    public double getValue() {
        return value;
    }

    public Set<JavaType> getPossibleTypes() {
        return Collections.singleton(JavaType.DOUBLE);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DoubleConst) {
            return value == ((DoubleConst) obj).value;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return (int) value;
    }

    @Override
    public String toString() {
        return Double.toString(value);
    }
}