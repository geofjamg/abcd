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
    
    private final String name;

    private final String superName;

    private final Set<Modifier> modifiers;

    private final List<String> interfaces;

    private final List<Field> fields;

    private final List<Class> innerClasses;
    
    private final List<Method> methods;

    public Class(Package _package, String name, String superName, Set<Modifier> modifiers) {
        this._package = _package;
        this.name = name;
        this.superName = superName;
        this.modifiers = modifiers;
        interfaces = new ArrayList<String>();
        fields = new ArrayList<Field>();
        innerClasses = new ArrayList<Class>();
        methods = new ArrayList<Method>();
    }

    public Package getPackage() {
        return _package;
    }

    public String getQualifiedName() {
        return _package.getName() + "." + name;
    }
    
    public String getName() {
        return name;
    }

    public String getSuperName() {
        return superName;
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }

    public List<String> getInterfaces() {
        return interfaces;
    }

    public void addInterface(String _interface) {
        interfaces.add(_interface);
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
        methods.add(method);
    }

    public <R, A> R accept(ClassVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
