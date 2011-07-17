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

import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Collections;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ClassConst implements Const {

    private final ClassName className;

    private final JavaType type;

    public ClassConst(ClassName className, ClassNameFactory factory) {
        this.className = className;
        this.type = JavaType.newRefType(factory.newClassName(Class.class.getName()));
    }

    public ClassName getClassName() {
        return className;
    }

    public Set<JavaType> getPossibleTypes() {
        return Collections.singleton(type);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof ClassConst) {
            return className.equals(((ClassConst) obj).className);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return className.hashCode();
    }

    @Override
    public String toString() {
        return className.getQualifiedName() + ".class";
    }
}