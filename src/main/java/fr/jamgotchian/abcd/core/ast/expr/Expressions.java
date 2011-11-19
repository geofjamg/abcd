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

import fr.jamgotchian.abcd.core.ir.Variable;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Expressions {

    private Expressions() {
    }

    public static TypeExpression newTypeExpr(JavaType type) {
        TypeExpression expr = new TypeExpression(type);
        return expr;
    }

    public static FieldAccess newFieldAccesExpr(Expression scope, String fieldName) {
        return new FieldAccess(scope, fieldName);
    }

    public static AssignExpression newAssignExpr(Expression target, Expression value,
                                                 AssignOperator operator) {
        return new AssignExpression(target, value, operator);
    }

    public static UnaryExpression newUnaryExpr(Expression expr, ASTUnaryOperator operator) {
        return new UnaryExpression(expr, operator);
    }

    public static IntegerLiteralExpression newIntExpr(int value) {
        return new IntegerLiteralExpression(value);
    }

    public static LongLiteralExpression newLongExpr(long value) {
        return new LongLiteralExpression(value);
    }

    public static FloatLiteralExpression newFloatExpr(float value) {
        return new FloatLiteralExpression(value);
    }

    public static DoubleLiteralExpression newDoubleExpr(double value) {
        return new DoubleLiteralExpression(value);
    }

    public static BooleanLiteralExpression newBooleanExpr(boolean value) {
        return new BooleanLiteralExpression(value);
    }

    public static ByteLiteralExpression newByteExpr(byte value) {
        return new ByteLiteralExpression(value);
    }

    public static ShortLiteralExpression newShortExpr(short value) {
        return new ShortLiteralExpression(value);
    }

    public static StringLiteralExpression newStringExpr(String value) {
        return new StringLiteralExpression(value);
    }

    public static NullLiteralExpression newNullExpr() {
        return new NullLiteralExpression();
    }

    public static ClassLiteralExpression newClsExpr(ClassName className) {
        return new ClassLiteralExpression(className);
    }

    public static BinaryExpression newBinExpr(Expression left, Expression right,
                                              ASTBinaryOperator operator) {
        return new BinaryExpression(left, right, operator);
    }

    public static ArrayAccess newArrayAccess(Expression arrayRef, Expression arrayIndexExpr) {
        return new ArrayAccess(arrayRef, arrayIndexExpr);
    }

    public static ArrayCreationExpression newArrayCreatExpr(JavaType type,
                                                            Expression arrayLengthExpr) {
        return new ArrayCreationExpression(type, arrayLengthExpr);
    }

    public static ArrayCreationExpression newArrayCreatExpr(JavaType type,
                                                            List<Expression> arrayLengthExprs) {
        return new ArrayCreationExpression(type, arrayLengthExprs);
    }

    public static ArrayLength newArrayLength(Expression arrayRef) {
        return new ArrayLength(arrayRef);
    }

    public static CastExpression newCastExpr(JavaType type, Expression expr) {
        return new CastExpression(type, expr);
    }

    public static ChoiceExpression newChoiceExpr(Set<Expression> choices) {
        return new ChoiceExpression(choices);
    }

    public static ChoiceExpression newChoiceExpr() {
        return  new ChoiceExpression();
    }

    public static ConditionalExpression newCondExpr(Expression condition, Expression then,
                                                    Expression _else) {
        return new ConditionalExpression(condition, then, _else);
    }

    public static VariableExpression newVarExpr(Variable var) {
        return new VariableExpression(var);
    }

    public static MethodCall newMethodExpr(Expression scope, String methodName,
                                           List<Expression> arguments) {
        return new MethodCall(scope, methodName, arguments);
    }

    public static ObjectCreationExpression newObjCreatExpr(JavaType type, List<Expression> arguments) {
        return new ObjectCreationExpression(type, arguments);
    }

    public static ObjectCreationExpression newObjCreatExpr(JavaType type) {
        return new ObjectCreationExpression(type);
    }
}
