/*
 * Copyright (C) 2010 geo
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

package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.expr.Expression;

/**
 *
 * @author geo
 */
public class ThrowStatement extends AbstractStatement {

    private final Expression objectRef;

    public ThrowStatement(Expression objectRef) {
        if (objectRef == null){
            throw new IllegalArgumentException("objectRef == null");
        }
        this.objectRef = objectRef;
    }

    public Expression getObjectRef() {
        return objectRef;
    }

    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
