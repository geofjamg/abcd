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

import com.google.common.collect.Sets;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SetFieldInst implements TACInst {

    private final Variable object;

    private final String fieldName;

    private final Variable value;

    SetFieldInst(Variable object, String fieldName, Variable value) {
        this.object = object;
        this.fieldName = fieldName;
        this.value = value;
    }

    public Variable getObject() {
        return object;
    }

    public String getFieldName() {
        return fieldName;
    }

    public Variable getValue() {
        return value;
    }

    public Set<Variable> getUses() {
        return Sets.newHashSet(object, value);
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
