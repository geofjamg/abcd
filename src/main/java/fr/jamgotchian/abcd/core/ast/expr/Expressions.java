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

import fr.jamgotchian.abcd.core.ast.type.ClassName;
import fr.jamgotchian.abcd.core.ast.type.JavaType;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Expressions {

    private Expressions() {
    }

    public static TypeExpression newTypeExpr(JavaType type, BasicBlock block) {
        TypeExpression expr = new TypeExpression(type);
        expr.setBasicBlock(block);
        return expr;
    }

    public static FieldAccess newFieldAccesExpr(Expression scope, String fieldName, BasicBlock block) {
        FieldAccess expr = new FieldAccess(scope, fieldName);
        expr.setBasicBlock(block);
        return expr;
    }

    public static AssignExpression newAssignExpr(Expression target, Expression value,
                                                 AssignOperator operator, BasicBlock block) {
        AssignExpression expr = new AssignExpression(target, value, operator);
        expr.setBasicBlock(block);
        return expr;
    }

    public static UnaryExpression newUnaryExpr(Expression expr, UnaryOperator operator, BasicBlock block) {
        UnaryExpression expr2 = new UnaryExpression(expr, operator);
        expr2.setBasicBlock(block);
        return expr2;
    }

    public static IntegerLiteralExpression newIntExpr(int value, BasicBlock block) {
        IntegerLiteralExpression expr = new IntegerLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static LongLiteralExpression newLongExpr(long value, BasicBlock block) {
        LongLiteralExpression expr = new LongLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static FloatLiteralExpression newFloatExpr(float value, BasicBlock block) {
        FloatLiteralExpression expr = new FloatLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static DoubleLiteralExpression newDoubleExpr(double value, BasicBlock block) {
        DoubleLiteralExpression expr = new DoubleLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static BooleanLiteralExpression newBooleanExpr(boolean value, BasicBlock block) {
        BooleanLiteralExpression expr = new BooleanLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ByteLiteralExpression newByteExpr(byte value, BasicBlock block) {
        ByteLiteralExpression expr = new ByteLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ShortLiteralExpression newShortExpr(short value, BasicBlock block) {
        ShortLiteralExpression expr = new ShortLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static StringLiteralExpression newStringExpr(String value, BasicBlock block) {
        StringLiteralExpression expr = new StringLiteralExpression(value);
        expr.setBasicBlock(block);
        return expr;
    }

    public static NullLiteralExpression newNullExpr(BasicBlock block) {
        NullLiteralExpression expr = new NullLiteralExpression();
        expr.setBasicBlock(block);
        return expr;
    }

    public static ClassLiteralExpression newClsExpr(ClassName className, BasicBlock block) {
        ClassLiteralExpression expr = new ClassLiteralExpression(className);
        expr.setBasicBlock(block);
        return expr;
    }

    public static BinaryExpression newBinExpr(Expression left, Expression right,
                                              BinaryOperator operator, BasicBlock block) {
        BinaryExpression expr = new BinaryExpression(left, right, operator);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ArrayAccess newArrayAcces(Expression arrayRef, Expression arrayIndexExpr,
                                            BasicBlock block) {
        ArrayAccess expr = new ArrayAccess(arrayRef, arrayIndexExpr);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ArrayCreationExpression newArrayCreatExpr(JavaType type,
                                                            Expression arrayLengthExpr,
                                                            BasicBlock block) {
        ArrayCreationExpression expr = new ArrayCreationExpression(type, arrayLengthExpr);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ArrayCreationExpression newArrayCreatExpr(JavaType type,
                                                            List<Expression> arrayLengthExprs,
                                                            BasicBlock block) {
        ArrayCreationExpression expr = new ArrayCreationExpression(type, arrayLengthExprs);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ArrayLength newArrayLength(Expression arrayRef, BasicBlock block) {
        ArrayLength expr = new ArrayLength(arrayRef);
        expr.setBasicBlock(block);
        return expr;
    }

    public static CastExpression newCastExpr(JavaType type, Expression expr,
                                             BasicBlock block) {
        CastExpression expr2 = new CastExpression(type, expr);
        expr2.setBasicBlock(block);
        return expr2;
    }

    public static ChoiceExpression newChoiceExpr(Set<Expression> choices, BasicBlock block) {
        ChoiceExpression expr = new ChoiceExpression(choices);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ChoiceExpression newChoiceExpr(BasicBlock block) {
        ChoiceExpression expr = new ChoiceExpression();
        expr.setBasicBlock(block);
        return expr;
    }

    public static ConditionalExpression newCondExpr(Expression condition, Expression then,
                                                    Expression _else, BasicBlock block) {
        ConditionalExpression expr = new ConditionalExpression(condition, then, _else);
        expr.setBasicBlock(block);
        return expr;
    }

    public static LocalVariable newVarExpr(int index, BasicBlock block) {
        LocalVariable expr = new LocalVariable(index);
        expr.setBasicBlock(block);
        return expr;
    }

    public static MethodCall newMethodExpr(Expression scope, String methodName,
                                           List<Expression> arguments, BasicBlock block) {
        MethodCall expr = new MethodCall(scope, methodName, arguments);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ObjectCreationExpression newObjCreatExpr(ClassName className, List<Expression> arguments,
                                                           BasicBlock block) {
        ObjectCreationExpression expr = new ObjectCreationExpression(className, arguments);
        expr.setBasicBlock(block);
        return expr;
    }

    public static ObjectCreationExpression newObjCreatExpr(ClassName className, BasicBlock block) {
        ObjectCreationExpression expr = new ObjectCreationExpression(className);
        expr.setBasicBlock(block);
        return expr;
    }
}
