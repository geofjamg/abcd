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

package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.AssignOperator;
import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.ObjectCreationExpression;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Statements {

    private Statements() {
    }

    public static void move(Statement startStmt, Statement endStmt, BlockStatement targetBlock) {
        if (startStmt == null) {
            // empty source block, do nothing
            return;
        } else {
            if (startStmt.getBlock() == null) {
                throw new IllegalArgumentException("startStmt.getBlock() == null");
            } else {
                if (targetBlock.getBlock() == startStmt.getBlock()) {
                    throw new IllegalArgumentException("toBlock.getBlock() == startStmt.getBlock()");
                }
            }
        }

        if (endStmt != null ) {
            if (endStmt.getBlock() == null) {
                throw new IllegalArgumentException("endStmt.getBlock() == null");
            }
        }

        List<Statement> stmts = new ArrayList<Statement>();
        Statement stmt;
        for (stmt = startStmt; stmt != null && stmt != endStmt; stmt = stmt.getNext()) {
            stmts.add(stmt);
        }
        if (endStmt == null || stmt != null) {
            targetBlock.addAll(stmts);
        }
    }
    
    public static <E extends Throwable> Statement createThrowStmt(Class<E> excCls, String msg) {
        return new ThrowStatement(
                    new ObjectCreationExpression(excCls.getName(),
                                                   Arrays.<Expression>asList(new Constant(msg))));
    }
    
    public static IfStatement createIfThenBreakStmt(Expression condition) {
        IfStatement ifStmt = new IfStatement(condition, new BlockStatement());
        ifStmt.getThen().add(new BreakStatement());
        return ifStmt;
    }
    
    public static boolean isAnAssignment(Statement stmt) {
        if (!(stmt instanceof ExpressionStatement)) {
            return false;
        }
        
        ExpressionStatement exprStmt = (ExpressionStatement) stmt;
        if (!(exprStmt.getExpression() instanceof AssignExpression)) {
            return false;
        }
        
        AssignExpression assignExpr = (AssignExpression) exprStmt.getExpression();
        
        return assignExpr.getOperator() == AssignOperator.ASSIGN;
    }
    
    public static boolean isAnIncrement(Statement stmt) {
        if (!(stmt instanceof ExpressionStatement)) {
            return false;
        }
        
        ExpressionStatement exprStmt = (ExpressionStatement) stmt;
        if (!(exprStmt.getExpression() instanceof AssignExpression)) {
            return false;
        }
        
        AssignExpression assignExpr = (AssignExpression) exprStmt.getExpression();
        
        return assignExpr.getOperator() == AssignOperator.PLUS ||
               assignExpr.getOperator() == AssignOperator.MINUS;
    }
}
