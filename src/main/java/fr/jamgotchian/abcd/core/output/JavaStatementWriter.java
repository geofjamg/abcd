/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHwriter ANY WARRANTY; withwriter even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.output;

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
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclarationStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ReturnStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.StatementVisitor;
import fr.jamgotchian.abcd.core.ast.stmt.ThrowStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchStatement.CatchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import java.util.Iterator;

public class JavaStatementWriter implements StatementVisitor<Void, Void> {

    private final CodeWriter writer;

    private final JavaExpressionWriter exprVisitor;

    private final boolean debug;

    public JavaStatementWriter(CodeWriter writer) {
        this(writer, false);
    }

    public JavaStatementWriter(CodeWriter writer, boolean debug) {
        this.writer = writer;
        this.debug = debug;
        this.exprVisitor = new JavaExpressionWriter(writer, debug);
    }

    public Void visit(BlockStatement blockStmt, Void arg) {
        writer.write("{").newLine();
        writer.incrIndent();
        Iterator<Statement> it = blockStmt.iterator();
        while (it.hasNext()) {
            Statement stmt = it.next();
            stmt.accept(this, null);
            if (it.hasNext()) {
                writer.newLine();
            }
        }
        writer.newLine();
        writer.decrIndent();
        if (debug) {
            writer.write("/*").newLine();
            for (String info : blockStmt.getDebugInfos()) {
                writer.write("  ").write(info).newLine();
            }
            writer.write("*/").newLine();
        }
        writer.write("}");
        return null;
    }

    public Void visit(ReturnStatement stmt, Void arg) {
        writer.writeKeyword("return");
        if (stmt.getExpression() != null) {
            writer.writeSpace();
            stmt.getExpression().accept(exprVisitor, stmt.getBlock());
        }
        writer.write(";");
        return null;
    }

    public Void visit(LocalVariableDeclarationStatement stmt, Void arg) {
        writer.write(stmt.getTypeName()).write(" v").write(stmt.getIndex());
        if (stmt.getInitExpr() != null) {
            writer.writeSpace().write("=").writeSpace();
            stmt.getInitExpr().accept(exprVisitor, stmt.getBlock());
        }
        writer.write(";");
        return null;
    }

    public Void visit(ExpressionStatement stmt, Void arg) {
        stmt.getExpression().accept(exprVisitor, stmt.getBlock());
        writer.write(";");
        return null;
    }

    public Void visit(CommentStatement stmt, Void arg) {
        writer.write("/*").write(stmt.getComment()).write("*/").newLine();
        return null;
    }

    public Void visit(IfStatement stmt, Void arg) {
        writer.writeKeyword("if").writeSpace().write("(");
        stmt.getCondition().accept(exprVisitor, stmt.getBlock());
        writer.write(")").writeSpace();
        stmt.getThen().accept(this, null);
        if (stmt.getElse() != null) {
            writer.writeSpace().writeKeyword("else").writeSpace();
            stmt.getElse().accept(this, null);
        }
        return null;
    }

    public Void visit(TryCatchStatement stmt, Void arg) {
        writer.writeKeyword("try").writeSpace();
        stmt.getTry().accept(this, null);
        for (CatchStatement _catch : stmt.getCatchs()) {
            if (_catch.getExceptionVarDecl() != null) {
                writer.writeSpace().writeKeyword("catch").writeSpace().write("(")
                      .write(_catch.getExceptionVarDecl().getTypeName())
                      .writeSpace().write(stmt.getBlock().getVariable(_catch.getExceptionVarDecl().getIndex()))
                      .write(")").writeSpace();
            } else {
                writer.writeSpace().writeKeyword("finally").writeSpace();
            }
            _catch.getBlockStmt().accept(this, null);            
        }
        return null;
    }

    public Void visit(BreakStatement stmt, Void arg) {
        writer.writeKeyword("break").write(";");
        return null;
    }

    public Void visit(DoWhileStatement stmt, Void arg) {
        writer.writeKeyword("do").writeSpace();
        stmt.getBody().accept(this, null);
        writer.write(" ").writeKeyword("while").writeSpace().write("(");
        stmt.getCondition().accept(exprVisitor, stmt.getBlock());
        writer.write(") ");
        return null;
    }

    public Void visit(WhileStatement stmt, Void arg) {
        writer.writeKeyword("while").writeSpace().write("(");
        stmt.getCondition().accept(exprVisitor, stmt.getBlock());
        writer.write(")").writeSpace();
        stmt.getBody().accept(this, null);
        return null;
    }

    public Void visit(ForStatement stmt, Void arg) {
        writer.writeKeyword("for").writeSpace().write("(");
        stmt.getInit().accept(exprVisitor, stmt.getBlock());
        writer.write(";").writeSpace();
        stmt.getCondition().accept(exprVisitor, stmt.getBlock());
        writer.write(";").writeSpace();
        stmt.getUpdate().accept(exprVisitor, stmt.getBlock());
        writer.write(")").writeSpace();
        stmt.getBody().accept(this, null);
        return null;
    }

    public Void visit(ThrowStatement stmt, Void arg) {
        writer.writeKeyword("throw").writeSpace();
        stmt.getObjectRef().accept(exprVisitor, stmt.getBlock());
        writer.write(";");
        return null;
    }

    public Void visit(JumpIfStatement stmt, Void arg) {
        writer.writeKeyword("jumpif").writeSpace();
        stmt.getCondition().accept(exprVisitor, stmt.getBlock());
        writer.writeSpace();
        writer.writeLabel(stmt.getLabel());
        writer.write(";");
        return null;
    }

    public Void visit(GotoStatement stmt, Void arg) {
        writer.writeKeyword("goto").writeSpace();
        writer.writeLabel(stmt.getLabel());
        writer.write(";");
        return null;
    }

    public Void visit(LabelStatement stmt, Void arg) {
        writer.writeLabel(stmt.getLabel());
        return null;
    }

}
