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

import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CallMethodInst extends DefInst {

    private final Variable object;

    private final MethodeSignature signature;

    private final List<Variable> arguments;

    public CallMethodInst(int defID, Variable result, Variable object,
                          MethodeSignature signature, List<Variable> arguments) {
        super(defID, result);
        this.object = object;
        this.signature = signature;
        this.arguments = arguments;
    }

    public Variable getObject() {
        return object;
    }

    public MethodeSignature getSignature() {
        return signature;
    }

    public List<Variable> getArguments() {
        return arguments;
    }

    public Set<Variable> getUses() {
        Set<Variable> uses = new HashSet<Variable>(arguments.size()+1);
        uses.add(object);
        uses.addAll(arguments);
        return uses;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
