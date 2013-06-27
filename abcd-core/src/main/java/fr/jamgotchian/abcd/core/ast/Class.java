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

import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Modifier;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Class {

    private final Package _package;

    private final ClassName name;

    private final JavaType thisType;

    private final ClassName superName;

    private final ClassKind kind;

    private final Set<Modifier> modifiers;

    private final List<ClassName> interfaces;

    private final List<Field> fields;

    private final List<Class> innerClasses;

    private final List<Method> methods;

    public Class(Package _package, ClassName name, ClassName superName,
                 List<ClassName> interfaces, ClassKind kind,
                 Set<Modifier> modifiers) {
        this._package = _package;
        this.name = name;
        thisType = JavaType.newRefType(name);
        this.superName = superName;
        this.interfaces = interfaces;
        this.kind = kind;
        this.modifiers = modifiers;
        fields = new ArrayList<>();
        innerClasses = new ArrayList<>();
        methods = new ArrayList<>();
    }

    public Package getPackage() {
        return _package;
    }

    public ClassName getName() {
        return name;
    }

    public JavaType getThisType() {
        return thisType;
    }

    public ClassName getSuperName() {
        return superName;
    }

    public ClassKind getKind() {
        return kind;
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }

    public List<ClassName> getInterfaces() {
        return interfaces;
    }

    public List<Field> getFields() {
        return fields;
    }

    public void addField(Field field) {
        fields.add(field);
    }

    public List<Class> getInnerClasses() {
        return innerClasses;
    }

    public void addInnerClass(Class _class) {
        innerClasses.add(_class);
    }

    public List<Method> getMethods() {
        return methods;
    }

    public void addMethod(Method method) {
        method.setClass(this);
        methods.add(method);
    }

    public <R, A> R accept(ClassVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
