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
public class AssignExpression extends AbstractExpression {

    private final Expression target;

    private final Expression value;

    private final AssignOperator operator;

    AssignExpression(Expression target, Expression value, AssignOperator operator) {
        this.target = target;
        this.value = value;
        this.operator = operator;
        target.setParent(this);
        value.setParent(this);
    }

    public Expression getTarget() {
        return target;
    }

    public Expression getValue() {
        return value;
    }

    public AssignOperator getOperator() {
        return operator;
    }

    @Override
    public <R, A> R accept(ExpressionVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
 }
