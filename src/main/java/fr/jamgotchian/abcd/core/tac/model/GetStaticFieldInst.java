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

import fr.jamgotchian.abcd.core.type.ClassName;
import java.util.Collections;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class GetStaticFieldInst implements TACInst {

    private final LocalVariable result;

    private final ClassName scope;

    private final String fieldName;

    public GetStaticFieldInst(LocalVariable result, ClassName scope, String fieldName) {
        this.result = result;
        this.scope = scope;
        this.fieldName = fieldName;
    }

    public LocalVariable getResult() {
        return result;
    }

    public ClassName getScope() {
        return scope;
    }

    public String getFieldName() {
        return fieldName;
    }

    public LocalVariable getDef() {
        return result;
    }

    public Set<LocalVariable> getUses() {
        return Collections.emptySet();
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
