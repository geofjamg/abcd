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

package fr.jamgotchian.abcd.core.ast;

import java.util.Set;
import javax.lang.model.element.Modifier;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Field {

    private final Set<Modifier> modifiers;

    private final String name;

    private final String typeName;

    public Field(Set<Modifier> modifiers, String name, String typeName) {
        this.modifiers = modifiers;
        this.name = name;
        this.typeName = typeName;
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }

    public String getName() {
        return name;
    }

    public String getTypeName() {
        return typeName;
    }

    public <R, A> R accept(ClassVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
