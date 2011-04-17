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
public interface ExpressionVisitor<R, A> {

    R visit(IntegerLiteralExpression expr, A arg);

    R visit(LongLiteralExpression expr, A arg);

    R visit(ByteLiteralExpression expr, A arg);

    R visit(ShortLiteralExpression expr, A arg);
    
    R visit(BooleanLiteralExpression expr, A arg);

    R visit(FloatLiteralExpression expr, A arg);

    R visit(DoubleLiteralExpression expr, A arg);

    R visit(StringLiteralExpression expr, A arg);

    R visit(NullLiteralExpression expr, A arg);

    R visit(ClassLiteralExpression expr, A arg);

    R visit(LocalVariable expr, A arg);

    R visit(UnaryExpression expr, A arg);

    R visit(BinaryExpression expr, A arg);

    R visit(AssignExpression expr, A arg);

    R visit(ClassExpression expr, A arg);

    R visit(FieldAccess expr, A arg);

    R visit(MethodCall expr, A arg);

    R visit(ConditionalExpression expr, A arg);

    R visit(ObjectCreationExpression expr, A arg);

    R visit(ArrayCreationExpression expr, A arg);

    R visit(ArrayLength expr, A arg);

    R visit(CastExpression expr, A arg);

    R visit(ArrayAccess expr, A arg);

    R visit(ChoiceExpression expr, A arg);

}
