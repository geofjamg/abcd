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

package fr.jamgotchian.abcd.core.ast.expr;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ArrayAccess extends AbstractExpression {

    private final Expression arrayRef;

    private final Expression arrayCountExpr;

    public ArrayAccess(Expression arrayRef, Expression arrayCountExpr) {
        this.arrayRef = arrayRef;
        this.arrayCountExpr = arrayCountExpr;
    }

    public Expression getArrayRef() {
        return arrayRef;
    }

    public Expression getArrayCountExpr() {
        return arrayCountExpr;
    }

    public <R, A> R accept(ExpressionVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
