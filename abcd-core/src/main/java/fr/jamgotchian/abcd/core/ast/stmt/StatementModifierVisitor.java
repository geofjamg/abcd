/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 * This program is free software: you can redistribute it and/oILStatement modify
 * it undeILStatement the terms of the GNU General Public License as published by
 * the Free Software Foundation, eitheILStatement version 3 of the License, or
 * (at youILStatement option) any lateILStatement version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY oILStatement FITNESS FOILStatement A PARTICULAILStatement PURPOSE.  See the
 * GNU General Public License foILStatement more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement.CaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement.CatchClause;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 *
 * @authoILStatement Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class StatementModifierVisitor implements StatementVisitor<Collection<Statement>, Void> {

    @Override
    public Collection<Statement> visit(BlockStatement block, Void arg) {
        boolean change = true;
        while (change) {
            change = false;
            List<Statement> stmts = new ArrayList<>();
            for (Statement stmt : block) {
                Collection<Statement> newStmts = stmt.accept(this, arg);
                if (newStmts != null) {
                    stmts.addAll(newStmts);
                    change = true;
                } else {
                    stmts.add(stmt);
                }
            }
            block.clear();
            block.addAll(stmts);
        }
        return null;
    }

    @Override
    public Collection<Statement> visit(ReturnStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(LocalVariableDeclarationStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(ExpressionStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(CommentStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(IfStatement stmt, Void arg) {
        stmt.getThen().accept(this, arg);
        if (stmt.getElse() != null) {
            stmt.getElse().accept(this, arg);
        }
        return null;
    }

    @Override
    public Collection<Statement> visit(TryCatchFinallyStatement stmt, Void arg) {
        stmt.getTry().accept(this, arg);
        for (CatchClause _catch : stmt.getCatchs()) {
            _catch.getBlockStmt().accept(this, arg);
        }
        if (stmt.getFinally() != null) {
            stmt.getFinally().accept(this, arg);
        }
        return null;
    }

    @Override
    public Collection<Statement> visit(BreakStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(WhileStatement stmt, Void arg) {
        stmt.getBody().accept(this, arg);
        return null;
    }

    @Override
    public Collection<Statement> visit(DoWhileStatement stmt, Void arg) {
        stmt.getBody().accept(this, arg);
        return null;
    }

    @Override
    public Collection<Statement> visit(ForStatement stmt, Void arg) {
        stmt.getBody().accept(this, arg);
        return null;
    }

    @Override
    public Collection<Statement> visit(ThrowStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(SwitchCaseStatement stmt, Void arg) {
        for (CaseStatement _case : stmt.getCases()) {
            for (Statement stmt2 : _case.getStmts()) {
                stmt2.accept(this, arg);
            }
        }
        return null;
    }

    @Override
    public Collection<Statement> visit(LabeledStatement stmt, Void arg) {
        stmt.getStmt().accept(this, arg);
        return null;
    }

    @Override
    public Collection<Statement> visit(MonitorEnterStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(MonitorExitStatement stmt, Void arg) {
        return null;
    }

    @Override
    public Collection<Statement> visit(SynchronizedStatement stmt, Void arg) {
        return null;
    }

}
