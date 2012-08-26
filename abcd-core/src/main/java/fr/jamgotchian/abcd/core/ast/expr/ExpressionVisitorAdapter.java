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
package fr.jamgotchian.abcd.core.ast.expr;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ExpressionVisitorAdapter<R, A> implements ExpressionVisitor<R, A> {

    @Override
    public R visit(IntegerLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(LongLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(ByteLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(ShortLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(BooleanLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(FloatLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(DoubleLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(StringLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(NullLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(ClassLiteralExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(VariableExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(UnaryExpression expr, A arg) {
        expr.getExpr().accept(this, arg);
        return null;
    }

    @Override
    public R visit(BinaryExpression expr, A arg) {
        expr.getLeft().accept(this, arg);
        expr.getRight().accept(this, arg);
        return null;
    }

    @Override
    public R visit(AssignExpression expr, A arg) {
        expr.getTarget().accept(this, arg);
        expr.getValue().accept(this, arg);
        return null;
    }

    @Override
    public R visit(TypeExpression expr, A arg) {
        return null;
    }

    @Override
    public R visit(FieldAccess expr, A arg) {
        expr.getScope().accept(this, arg);
        return null;
    }

    @Override
    public R visit(MethodCall expr, A arg) {
        expr.getScope().accept(this, arg);
        for (Expression expr2 : expr.getArguments()) {
            expr2.accept(this, arg);
        }
        return null;
    }

    @Override
    public R visit(ConditionalExpression expr, A arg) {
        expr.getCondition().accept(this, arg);
        expr.getThen().accept(this, arg);
        expr.getElse().accept(this, arg);
        return null;
    }

    @Override
    public R visit(ObjectCreationExpression expr, A arg) {
        for (Expression expr2 : expr.getArguments()) {
            expr2.accept(this, arg);
        }
        return null;
    }

    @Override
    public R visit(ArrayCreationExpression expr, A arg) {
        for (Expression expr2 : expr.getArrayLengthExprs()) {
            expr2.accept(this, arg);
        }
        for (Expression expr2 : expr.getInitValues()) {
            expr2.accept(this, arg);
        }
        return null;
    }

    @Override
    public R visit(ArrayLength expr, A arg) {
        expr.getArrayRef().accept(this, arg);
        return null;
    }

    @Override
    public R visit(CastExpression expr, A arg) {
        expr.getExpr().accept(this, arg);
        return null;
    }

    @Override
    public R visit(ArrayAccess expr, A arg) {
        expr.getArrayRef().accept(this, arg);
        expr.getArrayIndexExpr().accept(this, arg);
        return null;
    }

    @Override
    public R visit(ChoiceExpression expr, A arg) {
        for (Expression expr2 : expr.getChoices()) {
            expr2.accept(this, arg);
        }
        return null;
    }

}
