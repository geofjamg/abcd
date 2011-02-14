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

import fr.jamgotchian.abcd.core.ast.util.ExpressionInverter;
import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import org.junit.Assert;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ExpressionInverterTest {

    @Test
    public void test1() {
        BinaryExpression expr1 = new BinaryExpression(new Constant(1),
                                                          new Constant(2),
                                                          BinaryOperator.GT);
        BinaryExpression expr2 = new BinaryExpression(new Constant(2),
                                                          new Constant(3),
                                                          BinaryOperator.NE);
        BinaryExpression expr3 = new BinaryExpression(expr1,
                                                          expr2,
                                                          BinaryOperator.AND);

        Expression expr3Inv = ExpressionInverter.invert(expr3);
        Expression expr1Inv = ((BinaryExpression)expr3Inv).getLeft();
        Expression expr2Inv = ((BinaryExpression)expr3Inv).getRight();
        Assert.assertTrue(((BinaryExpression)expr3Inv).getOperator() == BinaryOperator.OR);
        Assert.assertTrue(((BinaryExpression)expr1Inv).getOperator() == BinaryOperator.LE);
        Assert.assertTrue(((BinaryExpression)expr2Inv).getOperator() == BinaryOperator.EQ);
    }
}