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

package fr.jamgotchian.abcd.core.ast.util;

import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.StringLiteralExpression;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author geo
 */
public class ExpressionEqualityCheckerTest {

    public ExpressionEqualityCheckerTest() {
    }

    @Test
    public void test1() {
        StringLiteralExpression expr1 = Expressions.newStringExpr("a");
        StringLiteralExpression expr2 = Expressions.newStringExpr("a");
        assertTrue(ExpressionEqualityChecker.equal(expr1, expr2));
    }

    @Test
    public void test2() {
        BinaryExpression expr1 = Expressions.newBinExpr(Expressions.newStringExpr("a"),
                                                        Expressions.newStringExpr("b"),
                                                        BinaryOperator.EQ);
        BinaryExpression expr2 = Expressions.newBinExpr(Expressions.newStringExpr("a"),
                                                        Expressions.newStringExpr("b"),
                                                        BinaryOperator.EQ);
        assertTrue(ExpressionEqualityChecker.equal(expr1, expr2));
    }

    @Test
    public void test3() {
        BinaryExpression expr1 = Expressions.newBinExpr(Expressions.newStringExpr("a"),
                                                        Expressions.newStringExpr("b"),
                                                        BinaryOperator.EQ);
        BinaryExpression expr2 = Expressions.newBinExpr(Expressions.newStringExpr("a"),
                                                        Expressions.newStringExpr("c"),
                                                        BinaryOperator.EQ);
        assertFalse(ExpressionEqualityChecker.equal(expr1, expr2));
    }

}