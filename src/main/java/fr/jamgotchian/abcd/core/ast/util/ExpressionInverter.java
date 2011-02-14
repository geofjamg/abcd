/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

package fr.jamgotchian.abcd.core.ast.util;

import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.UnaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.ExpressionModifierVisitor;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ExpressionInverter extends ExpressionModifierVisitor {

    private static final ExpressionInverter instance = new ExpressionInverter();

    public static Expression invert(Expression expr) {
        return expr.accept(instance, null);
    }

    @Override
    public Expression visit(UnaryExpression expr, Void arg) {
        Expression newExpr = expr.getExpr().accept(this, arg);
        return new UnaryExpression(newExpr != null ? newExpr : expr.getExpr(), 
                                     expr.getOperator().getInverse());
    }

    @Override
    public Expression visit(BinaryExpression expr, Void arg) {
        Expression newLeft = expr.getLeft().accept(this, arg);
        Expression newRight = expr.getRight().accept(this, arg);
        return new BinaryExpression(newLeft != null ? newLeft : expr.getLeft(), 
                                      newRight != null ? newRight : expr.getRight(), 
                                      expr.getOperator().getInverse());
    }
}
