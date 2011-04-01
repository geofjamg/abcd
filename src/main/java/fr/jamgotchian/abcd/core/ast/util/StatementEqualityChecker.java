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

package fr.jamgotchian.abcd.core.ast.util;

import com.google.common.base.Objects;
import com.google.common.collect.Iterables;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.BreakStatement;
import fr.jamgotchian.abcd.core.ast.stmt.CommentStatement;
import fr.jamgotchian.abcd.core.ast.stmt.DoWhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ForStatement;
import fr.jamgotchian.abcd.core.ast.stmt.GotoStatement;
import fr.jamgotchian.abcd.core.ast.stmt.IfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.JumpIfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabelStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabeledStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclarationStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LookupOrTableSwitchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ReturnStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.StatementVisitor;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement.CaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ThrowStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement.CatchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class StatementEqualityChecker implements StatementVisitor<Boolean, Statement> {

    private static final StatementEqualityChecker INSTANCE = new StatementEqualityChecker();

    public static boolean equal(Iterable<Statement> stmts1, Iterable<Statement> stmts2) {
        List<Statement> filteredStmts1 = filter(stmts1);
        List<Statement> filteredStmts2 = filter(stmts2);
        if (filteredStmts1.size() == filteredStmts2.size()) {
            for (int i = 0; i < filteredStmts1.size(); i++) {
                Statement filteredStmt1 = filteredStmts1.get(i);
                Statement filteredStmt2 = filteredStmts2.get(i);
                if (Boolean.FALSE.equals(filteredStmt1.accept(INSTANCE, filteredStmt2))) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    private static List<Statement> filter(Iterable<Statement> stmts) {
        List<Statement> filteredStmts = new ArrayList<Statement>();
        for (Statement stmt : stmts) {
            if (stmt instanceof CommentStatement
                    || stmt instanceof JumpIfStatement
                    || stmt instanceof GotoStatement
                    || stmt instanceof LabelStatement
                    || stmt instanceof LookupOrTableSwitchStatement) {
                continue;
            }
            filteredStmts.add(stmt);
        }
        return filteredStmts;
    }

    private static boolean nullableStmtEqual(StatementVisitor<Boolean, Statement> visitor,
                                             Statement stmt1, Statement stmt2) {
        if (stmt1 == null && stmt2 == null) {
            return true;
        } else if (stmt1 != null && stmt2 != null) {
            return Boolean.TRUE.equals(stmt1.accept(visitor, stmt2));
        } else {
            return false;
        }
    }

    public Boolean visit(BlockStatement block, Statement arg) {
        if (arg instanceof BlockStatement) {
            BlockStatement block2 = (BlockStatement) arg;
            int size = Iterables.size(block);
            int size2 = Iterables.size(block2);
            if (size == size2) {
                Iterator<Statement> it = block.iterator();
                Iterator<Statement> it2 = block2.iterator();
                while (it.hasNext() && it2.hasNext()) {
                    Statement stmt = it.next();
                    Statement stmt2 = it.next();
                    if (Boolean.FALSE.equals(stmt.accept(this, stmt2))) {
                        return Boolean.FALSE;
                    }
                }
                return Boolean.TRUE;
            }
        }
        return Boolean.FALSE;
    }

    public Boolean visit(ReturnStatement stmt, Statement arg) {
        if (arg instanceof ReturnStatement) {
            ReturnStatement stmt2 = (ReturnStatement) arg;
            return ExpressionEqualityChecker.equal(stmt.getExpression(), stmt2.getExpression());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(LocalVariableDeclarationStatement stmt, Statement arg) {
        if (arg instanceof LocalVariableDeclarationStatement) {
            LocalVariableDeclarationStatement stmt2 = (LocalVariableDeclarationStatement) stmt;
            if (!stmt.getLocalVarDecl().equals(stmt2.getLocalVarDecl())) {
                return Boolean.FALSE;
            }
            return ExpressionEqualityChecker.equal(stmt.getInitExpr(), stmt2.getInitExpr());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(ExpressionStatement stmt, Statement arg) {
        if (arg instanceof ExpressionStatement) {
            ExpressionStatement stmt2 = (ExpressionStatement) arg;
            return ExpressionEqualityChecker.equal(stmt.getExpression(), stmt2.getExpression());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(CommentStatement stmt, Statement arg) {
        throw new IllegalStateException();
    }

    public Boolean visit(IfStatement stmt, Statement arg) {
        if (arg instanceof IfStatement) {
            IfStatement stmt2 = (IfStatement) arg;
            if (!ExpressionEqualityChecker.equal(stmt.getCondition(), stmt2.getCondition())) {
                return Boolean.FALSE;
            }
            if (Boolean.FALSE.equals(stmt.getThen().accept(this, stmt2.getThen()))) {
                return Boolean.FALSE;
            }
            return nullableStmtEqual(this, stmt.getElse(), stmt2.getElse());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(TryCatchFinallyStatement stmt, Statement arg) {
        if (arg instanceof TryCatchFinallyStatement) {
            TryCatchFinallyStatement stmt2 = (TryCatchFinallyStatement) arg;
            if (Boolean.FALSE.equals(stmt.getTry().accept(this, stmt2.getTry()))) {
                return Boolean.FALSE;
            }
            Collection<CatchStatement> catchs1 = stmt.getCatchs();
            Collection<CatchStatement> catchs2 = stmt2.getCatchs();
            if (catchs1.size() != catchs2.size()) {
                return Boolean.FALSE;
            }
            Iterator<CatchStatement> it1 = catchs1.iterator();
            Iterator<CatchStatement> it2 = catchs2.iterator();
            while (it1.hasNext() && it2.hasNext()) {
                CatchStatement catch1 = it1.next();
                CatchStatement catch2 = it2.next();
                if (!catch1.getExceptionVarDecl().equals(catch2.getExceptionVarDecl())) {
                    return Boolean.FALSE;
                }
                if (Boolean.FALSE.equals(catch1.getBlockStmt().accept(this, catch2.getBlockStmt()))) {
                    return Boolean.FALSE;
                }
            }
            return nullableStmtEqual(this, stmt.getFinally(), stmt2.getFinally());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(BreakStatement stmt, Statement arg) {
        return arg instanceof BreakStatement ? Boolean.TRUE :  Boolean.FALSE;
    }

    public Boolean visit(WhileStatement stmt, Statement arg) {
        if (arg instanceof WhileStatement) {
            WhileStatement stmt2 = (WhileStatement) arg;
            if (!ExpressionEqualityChecker.equal(stmt.getCondition(), stmt2.getCondition())) {
                return Boolean.FALSE;
            }
            return stmt.getBody().accept(this, stmt2.getBody());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(DoWhileStatement stmt, Statement arg) {
        if (arg instanceof DoWhileStatement) {
            DoWhileStatement stmt2 = (DoWhileStatement) arg;
            if (!ExpressionEqualityChecker.equal(stmt.getCondition(), stmt2.getCondition())) {
                return Boolean.FALSE;
            }
            return stmt.getBody().accept(this, stmt2.getBody());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(ForStatement stmt, Statement arg) {
        if (arg instanceof ForStatement) {
            ForStatement stmt2 = (ForStatement) arg;
            if (!ExpressionEqualityChecker.equal(stmt.getInit(), stmt2.getInit())) {
                return Boolean.FALSE;
            }
            if (!ExpressionEqualityChecker.equal(stmt.getCondition(), stmt2.getCondition())) {
                return Boolean.FALSE;
            }
            if (!ExpressionEqualityChecker.equal(stmt.getUpdate(), stmt2.getUpdate())) {
                return Boolean.FALSE;
            }
            return stmt.getBody().accept(this, stmt2.getBody());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(ThrowStatement stmt, Statement arg) {
        if (arg instanceof ThrowStatement) {
            ThrowStatement stmt2 = (ThrowStatement) arg;
            return ExpressionEqualityChecker.equal(stmt.getObjectRef(), stmt2.getObjectRef());
        }
        return Boolean.FALSE;
    }

    public Boolean visit(JumpIfStatement stmt, Statement arg) {
        throw new IllegalStateException();
    }

    public Boolean visit(GotoStatement stmt, Statement arg) {
        throw new IllegalStateException();
    }

    public Boolean visit(LabelStatement stmt, Statement arg) {
        throw new IllegalStateException();
    }

    public Boolean visit(LookupOrTableSwitchStatement stmt, Statement arg) {
        throw new IllegalStateException();
    }

    public Boolean visit(SwitchCaseStatement stmt, Statement arg) {
        if (arg instanceof SwitchCaseStatement) {
            SwitchCaseStatement stmt2 = (SwitchCaseStatement) arg;
            if (!ExpressionEqualityChecker.equal(stmt.getCondition(), stmt2.getCondition())) {
                return Boolean.FALSE;
            }
            Collection<CaseStatement> cases1 = stmt.getCases();
            Collection<CaseStatement> cases2 = stmt2.getCases();
            if (cases1.size() != cases2.size()) {
                return Boolean.FALSE;
            }
            Iterator<CaseStatement> it1 = cases1.iterator();
            Iterator<CaseStatement> it2 = cases2.iterator();
            while (it1.hasNext() && it2.hasNext()) {
                CaseStatement case1 = it1.next();
                CaseStatement case2 = it2.next();
                if (!Objects.equal(case1.getValue(), case2.getValue())) {
                    return Boolean.FALSE;
                }
                if (Boolean.FALSE.equals(case1.getBlockStmt().accept(this, case2.getBlockStmt()))) {
                    return Boolean.FALSE;
                }
            }
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    public Boolean visit(LabeledStatement stmt, Statement arg) {
        if (arg instanceof LabeledStatement) {
            LabeledStatement stmt2 = (LabeledStatement) arg;
            if (!stmt.getLabel().equals(stmt2.getLabel())) {
                return Boolean.FALSE;
            }
            return stmt.getStmt().accept(this, stmt2.getStmt());
        }
        return Boolean.FALSE;
    }

}
