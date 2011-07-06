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
package fr.jamgotchian.abcd.core.ast.stmt;

import fr.jamgotchian.abcd.core.ast.expr.ExpressionVisitor;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement.CaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement.CatchClause;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class StatementVisitorAdapter<R, A> implements StatementVisitor<R, A> {

    private final ExpressionVisitor<?, ?> exprVisitor;

    public StatementVisitorAdapter(ExpressionVisitor<?, ?> exprVisitor) {
        this.exprVisitor = exprVisitor;
    }

    public R visit(BlockStatement block, A arg) {
        for (Statement stmt : block) {
            stmt.accept(this, arg);
        }
        return null;
    }

    public R visit(ReturnStatement stmt, A arg) {
        if (exprVisitor != null) {
            if (stmt.getExpression() != null) {
                stmt.getExpression().accept(exprVisitor, null);
            }
        }
        return null;
    }

    public R visit(LocalVariableDeclarationStatement stmt, A arg) {
        if (exprVisitor != null) {
            if (stmt.getInitExpr() != null) {
                stmt.getInitExpr().accept(exprVisitor, null);
            }
        }
        return null;
    }

    public R visit(ExpressionStatement stmt, A arg) {
        if (exprVisitor != null) {
            if (stmt.getExpression() != null) {
                stmt.getExpression().accept(exprVisitor, null);
            }
        }
        return null;
    }

    public R visit(CommentStatement stmt, A arg) {
        return null;
    }

    public R visit(IfStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getCondition().accept(exprVisitor, null);
        }
        stmt.getThen().accept(this, arg);
        if (stmt.getElse() != null) {
            stmt.getElse().accept(this, arg);
        }
        return null;
    }

    public R visit(TryCatchFinallyStatement stmt, A arg) {
        stmt.getTry().accept(this, arg);
        for (CatchClause _catch : stmt.getCatchs()) {
            _catch.getBlockStmt().accept(this, arg);
        }
        if (stmt.getFinally() != null) {
            stmt.getFinally().accept(this, arg);
        }
        return null;
    }

    public R visit(BreakStatement stmt, A arg) {
        return null;
    }

    public R visit(WhileStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getCondition().accept(exprVisitor, null);
        }
        stmt.getBody().accept(this, arg);
        return null;
    }

    public R visit(DoWhileStatement stmt, A arg) {
        stmt.getBody().accept(this, arg);
        if (exprVisitor != null) {
            stmt.getCondition().accept(exprVisitor, null);
        }
        return null;
    }

    public R visit(ForStatement stmt, A arg) {
        if (exprVisitor != null) {
            if (stmt.getInit() != null) {
                stmt.getInit().accept(exprVisitor, null);
            }
            if (stmt.getCondition() != null) {
                stmt.getCondition().accept(exprVisitor, null);
            }
            if (stmt.getUpdate() != null) {
                stmt.getUpdate().accept(exprVisitor, null);
            }
            stmt.getBody().accept(this, arg);
        }
        return null;
    }

    public R visit(ThrowStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getObjectRef().accept(exprVisitor, null);
        }
        return null;
    }

    public R visit(SwitchCaseStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getCondition().accept(exprVisitor, null);
        }
        for (CaseStatement _case : stmt.getCases()) {
            for (Statement stmt2 : _case.getStmts()) {
                stmt2.accept(this, arg);
            }
        }
        return null;
    }

    public R visit(LabeledStatement stmt, A arg) {
        stmt.getStmt().accept(this, arg);
        return null;
    }

    public R visit(MonitorEnterStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getObjectRef().accept(exprVisitor, null);
        }
        return null;
    }

    public R visit(MonitorExitStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getObjectRef().accept(exprVisitor, null);
        }
        return null;
    }

    public R visit(SynchronizedStatement stmt, A arg) {
        if (exprVisitor != null) {
            stmt.getExpression().accept(exprVisitor, null);
        }
        stmt.getBody().accept(this, arg);
        return null;
    }

}
