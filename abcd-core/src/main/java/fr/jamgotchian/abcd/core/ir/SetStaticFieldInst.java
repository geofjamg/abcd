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

import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SetStaticFieldInst extends IRInstImpl {

    private final ClassName scope;

    private final String fieldName;

    private final JavaType fieldType;

    private final Variable value;

    SetStaticFieldInst(ClassName scope, String fieldName, JavaType fieldType,
                       Variable value) {
        this.scope = scope;
        this.fieldName = fieldName;
        this.fieldType = fieldType;
        this.value = value;
    }

    public ClassName getScope() {
        return scope;
    }

    public String getFieldName() {
        return fieldName;
    }

    public JavaType getFieldType() {
        return fieldType;
    }

    public Variable getValue() {
        return value;
    }

    @Override
    public Set<Variable> getUses() {
        return Sets.newHashSet(value);
    }

    @Override
    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
