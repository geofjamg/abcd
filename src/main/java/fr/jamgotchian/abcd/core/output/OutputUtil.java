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

package fr.jamgotchian.abcd.core.output;

import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraphImpl;
import java.io.IOException;
import java.io.StringWriter;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.objectweb.asm.tree.InsnList;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class OutputUtil {

    private static final Logger logger = Logger.getLogger(OutputUtil.class.getName());

    private OutputUtil() {
    }

    private static class InstnPrintVisitor extends BytecodeWriter {

        private InstnPrintVisitor(InstnWriter writer) {
            super(writer);
        }

        @Override
        public void before(BasicBlock block) {
            try {
                writer.begin();
            } catch(IOException exc) {
                throw new ABCDException(exc);
            }
        }
    }

    public static String toText(BasicBlock block) {
        StringWriter writer = new StringWriter();
        block.visit(new BytecodeWriter(new TextInstnWriter(writer)));
        return writer.toString();
    }

    public static String toText(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph graph = new ControlFlowGraphImpl("", instructions);
        BasicBlock block = graph.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        block.visit(new InstnPrintVisitor(new TextInstnWriter(writer)));
        return writer.toString();
    }

    public static String toHTML(BasicBlock block) {
        StringWriter writer = new StringWriter();
        block.visit(new BytecodeWriter(new HTMLInstnWriter(writer)));
        return writer.toString();
    }

    public static String toHTML(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph graph = new ControlFlowGraphImpl("", instructions);
        BasicBlock block = graph.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        block.visit(new InstnPrintVisitor(new HTMLInstnWriter(writer)));
        return writer.toString();
    }

    public static String toText(Expression expr) {
        StringWriter writer = new StringWriter();
        try {
            JavaExpressionWriter exprWriter = new JavaExpressionWriter(new TextCodeWriter(writer));
            expr.accept(exprWriter, null);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }

    public static String toHTML(Expression expr) {
        StringWriter writer = new StringWriter();
        try {
            JavaExpressionWriter exprWriter = new JavaExpressionWriter(new HTMLCodeWriter(writer));
            expr.accept(exprWriter, null);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }

    public static String toText(Statement stmt) {
        StringWriter writer = new StringWriter();
        try {
            JavaStatementWriter stmtWriter = new JavaStatementWriter(new TextCodeWriter(writer));
            stmt.accept(stmtWriter, null);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }

    public static String toHTML(Statement stmt) {
        StringWriter writer = new StringWriter();
        try {
            JavaStatementWriter stmtWriter = new JavaStatementWriter(new HTMLCodeWriter(writer));
            stmt.accept(stmtWriter, null);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }

    public static String toHTML(Iterable<Statement> stmts) {
        StringWriter writer = new StringWriter();
        try {
            JavaStatementWriter stmtWriter = new JavaStatementWriter(new HTMLCodeWriter(writer));
            BlockStatement blockStmt = new BlockStatement(stmts);
            blockStmt.accept(stmtWriter, null);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }
    
    public static String toText(Iterable<Statement> stmts) {
        StringWriter writer = new StringWriter();
        try {
            JavaStatementWriter stmtWriter = new JavaStatementWriter(new TextCodeWriter(writer));
            BlockStatement blockStmt = new BlockStatement(stmts);
            blockStmt.accept(stmtWriter, null);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }
}
