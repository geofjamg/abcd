/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.type.JavaType;
import fr.jamgotchian.abcd.core.common.ABCDException;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariableDeclaration {

    private final int index;

    private final JavaType type;

    public LocalVariableDeclaration(int index, JavaType type) {
        if (type == null) {
            throw new ABCDException("type == null");
        }
        this.index = index;
        this.type = type;
    }

    public int getIndex() {
        return index;
    }

    public JavaType getType() {
        return type;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof LocalVariableDeclaration)) {
            return false;
        }
        LocalVariableDeclaration decl = (LocalVariableDeclaration) obj;
        return index == decl.index && type.equals(decl.type);
    }

    @Override
    public int hashCode() {
        return index + type.hashCode();
    }
}
