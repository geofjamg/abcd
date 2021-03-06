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
import fr.jamgotchian.abcd.core.ast.expr.ASTBinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import org.junit.Assert;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ExpressionInverterTest {

    @Test
    public void test1() {
        BinaryExpression expr1 = Expressions.newBinExpr(Expressions.newIntExpr(1),
                                                        Expressions.newIntExpr(2),
                                                        ASTBinaryOperator.GT);
        BinaryExpression expr2 = Expressions.newBinExpr(Expressions.newIntExpr(2),
                                                        Expressions.newIntExpr(3),
                                                        ASTBinaryOperator.NE);
        BinaryExpression expr3 = Expressions.newBinExpr(expr1,
                                                        expr2,
                                                        ASTBinaryOperator.AND);

        Expression expr3Inv = ExpressionInverter.invert(expr3);
        Expression expr1Inv = ((BinaryExpression)expr3Inv).getLeft();
        Expression expr2Inv = ((BinaryExpression)expr3Inv).getRight();
        Assert.assertTrue(((BinaryExpression)expr3Inv).getOperator() == ASTBinaryOperator.OR);
        Assert.assertTrue(((BinaryExpression)expr1Inv).getOperator() == ASTBinaryOperator.LE);
        Assert.assertTrue(((BinaryExpression)expr2Inv).getOperator() == ASTBinaryOperator.EQ);
    }
}