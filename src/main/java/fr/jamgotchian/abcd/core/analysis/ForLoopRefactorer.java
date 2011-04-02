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

package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ForStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.Statements;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.StatementModifierVisitor;
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ForLoopRefactorer extends StatementModifierVisitor implements Refactorer {

    public void refactor(BlockStatement blockStmt) {
        blockStmt.accept(this, null);
    }
    
    @Override
    public Collection<Statement> visit(ExpressionStatement stmt, Void arg) {
        if (Statements.isAnAssignment(stmt)) {
            AssignExpression initExpr = ((AssignExpression) stmt.getExpression());
            if (!stmt.isLast() && stmt.getNext() instanceof WhileStatement) {
                WhileStatement whileStmt = (WhileStatement) stmt.getNext();
                Expression conditionExpr = whileStmt.getCondition();
                if (!whileStmt.getBody().isEmpty() && Statements.isAnIncrement(whileStmt.getBody().getLast())) {
                    ExpressionStatement updateStmt = (ExpressionStatement) whileStmt.getBody().getLast();
                    Expression updateExpr = (Expression) updateStmt.getExpression();
                    updateStmt.remove();
                    ForStatement forStmt = new ForStatement(initExpr, conditionExpr, updateExpr,
                                                            whileStmt.getBody());
                    whileStmt.remove();
                    return Collections.<Statement>singleton(forStmt);
                }
            }
        }

        return super.visit(stmt, arg);
    }
}
