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

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.type.TypeKind;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NewObjectInst extends DefInst {

    private final JavaType type;

    private List<Variable> arguments;

    NewObjectInst(int defID, Variable result, JavaType type) {
        super(defID, result);
        if (type.getKind() == TypeKind.PRIMITIVE) {
            throw new ABCDException("type.getKind() == TypeKind.PRIMITIVE");
        }
        this.type = type;
        this.arguments = Collections.emptyList();
    }

    public JavaType getType() {
        return type;
    }

    public List<Variable> getArguments() {
        return arguments;
    }

    public int getArgumentCount() {
        return arguments.size();
    }

    @Override
    public Set<Variable> getUses() {
        return new HashSet<Variable>(arguments);
    }

    @Override
    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
