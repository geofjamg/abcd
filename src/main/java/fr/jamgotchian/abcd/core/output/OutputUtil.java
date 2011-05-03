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
import fr.jamgotchian.abcd.core.ast.util.ExpressionStack;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraphImpl;
import fr.jamgotchian.abcd.core.tac.TACInst;
import fr.jamgotchian.abcd.core.tac.TACInstWriter;
import fr.jamgotchian.abcd.core.tac.TemporaryVariable;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Iterator;
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

    public static String toDOTHTMLLike(BasicBlock block) {
        StringWriter writer = new StringWriter();
        block.visit(new BytecodeWriter(new DOTHTMLLikeInstnWriter(writer)));
        return writer.toString();
    }

    public static String toHTML(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph graph = new ControlFlowGraphImpl("", instructions);
        BasicBlock block = graph.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        block.visit(new InstnPrintVisitor(new HTMLInstnWriter(writer)));
        return writer.toString();
    }

    public static String toString(Expression expr, CodeWriterFactory factory) {
        StringWriter writer = new StringWriter();
        try {
            JavaExpressionWriter exprWriter = new JavaExpressionWriter(factory.create(writer));
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

    public static String toText(Expression expr) {
        return toString(expr, new TextCodeWriterFactory());
    }

    public static String toHTML(Expression expr) {
        return toString(expr, new HTMLCodeWriterFactory());
    }

    public static String toString(Iterable<Expression> exprs, CodeWriterFactory factory) {
        StringBuilder builder = new StringBuilder("[");
        Iterator<Expression> it = exprs.iterator();
        while (it.hasNext()) {
            Expression expr = it.next();
            builder.append(OutputUtil.toString(expr, factory));
            if (it.hasNext()) {
                builder.append(", ");
            }
        }
        builder.append("]");
        return builder.toString();
    }

    public static String toText(Iterable<Expression> exprs) {
        return toString(exprs, new TextCodeWriterFactory());
    }

    public static String toHTML(Iterable<Expression> exprs) {
        return toString(exprs, new HTMLCodeWriterFactory());
    }

    public static String toString(Statement stmt, CodeWriterFactory factory) {
        StringWriter writer = new StringWriter();
        try {
            JavaStatementWriter stmtWriter = new JavaStatementWriter(factory.create(writer));
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

    public static String toText(Statement stmt) {
        return toString(stmt, new TextCodeWriterFactory());
    }

    public static String toHTML(Statement stmt) {
        return toString(stmt, new HTMLCodeWriterFactory());
    }

    public static String toString(Iterable<Statement> stmts,
                                  ExpressionStack inputStack,
                                  ExpressionStack outputStack,
                                  CodeWriterFactory factory) {
        StringWriter writer = new StringWriter();
        try {
            CodeWriter codeWriter = factory.create(writer);
            JavaStatementWriter stmtWriter = new JavaStatementWriter(codeWriter);
            String infoBefore = null;
            if (inputStack != null && inputStack.size() > 0) {
                infoBefore = toString(inputStack.toIterable(), factory);
            }
            codeWriter.before(infoBefore);
            BlockStatement blockStmt = new BlockStatement(stmts);
            blockStmt.accept(stmtWriter, null);
            String infoAfter = null;
            if (outputStack != null && outputStack.size() > 0) {
                infoAfter = toString(outputStack.toIterable(), factory);
            }
            codeWriter.after(infoAfter);
        } finally {
            try {
                writer.close();
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
        return writer.toString();
    }

    public static String toText(Iterable<Statement> stmts,
                                ExpressionStack inputStack,
                                ExpressionStack outputStack) {
        return toString(stmts, inputStack, outputStack, new TextCodeWriterFactory());
    }

    public static String toHTML(Iterable<Statement> stmts,
                                ExpressionStack inputStack,
                                ExpressionStack outputStack) {
        return toString(stmts, inputStack, outputStack, new HTMLCodeWriterFactory());
    }

    public static String toDOTHTMLLike(Iterable<Statement> stmts,
                                       ExpressionStack inputStack,
                                       ExpressionStack outputStack) {
        return toString(stmts, inputStack, outputStack, new DOTHTMLLikeCodeWriterFactory());
    }

    public static String toString(TACInst inst, CodeWriterFactory factory) {
        Writer writer = new StringWriter();
        try {
            inst.accept(new TACInstWriter(factory.create(writer)), null);
        } finally {
            try {
                writer.close();
            } catch (IOException e) {
                logger.log(Level.SEVERE, e.toString(), e);
            }
        }
        return writer.toString();
    }

    public static String toText(TACInst inst) {
        return toString(inst, new TextCodeWriterFactory());
    }

    public static String toHTML(TACInst inst) {
        return toString(inst, new HTMLCodeWriterFactory());
    }

    public static String toString2(Iterable<TemporaryVariable> vars, CodeWriterFactory factory) {
        StringBuilder builder = new StringBuilder("[");
        Iterator<TemporaryVariable> it =  vars.iterator();
        while (it.hasNext()) {
            TemporaryVariable var = it.next();
            Writer writer = new StringWriter();
            try {
                var.accept(new TACInstWriter(factory.create(writer)), null);
            } finally {
                try {
                    writer.close();
                } catch (IOException e) {
                    logger.log(Level.SEVERE, e.toString(), e);
                }
            }
            builder.append(writer.toString());
            if (it.hasNext()) {
                builder.append(", ");
            }
        }
        builder.append("]");
        return builder.toString();
    }

    public static String toText2(Iterable<TemporaryVariable> exprs) {
        return toString2(exprs, new TextCodeWriterFactory());
    }

    public static String toHTML2(Iterable<TemporaryVariable> exprs) {
        return toString2(exprs, new HTMLCodeWriterFactory());
    }
}
