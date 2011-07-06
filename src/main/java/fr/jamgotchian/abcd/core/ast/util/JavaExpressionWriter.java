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

package fr.jamgotchian.abcd.core.ast.util;

import fr.jamgotchian.abcd.core.ast.expr.ArrayAccess;
import fr.jamgotchian.abcd.core.ast.expr.ArrayCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.ArrayLength;
import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.BooleanLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.ByteLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.CastExpression;
import fr.jamgotchian.abcd.core.ast.expr.TypeExpression;
import fr.jamgotchian.abcd.core.ast.expr.ConditionalExpression;
import fr.jamgotchian.abcd.core.ast.expr.DoubleLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.ChoiceExpression;
import fr.jamgotchian.abcd.core.ast.expr.ClassLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.ExpressionVisitor;
import fr.jamgotchian.abcd.core.ast.expr.FieldAccess;
import fr.jamgotchian.abcd.core.ast.expr.FloatLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.IntegerLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.expr.LongLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.MethodCall;
import fr.jamgotchian.abcd.core.ast.expr.NullLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.ObjectCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.ShortLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.StringLiteralExpression;
import fr.jamgotchian.abcd.core.ast.expr.UnaryExpression;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.output.CodeWriter;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaExpressionWriter implements ExpressionVisitor<Void, BlockStatement> {

    private final CodeWriter writer;

    private final boolean debug;

    public JavaExpressionWriter(CodeWriter writer) {
        this(writer, false);
    }

    public JavaExpressionWriter(CodeWriter writer, boolean debug) {
        this.writer = writer;
        this.debug = debug;
    }

    public Void visit(AssignExpression expr, BlockStatement blockStmt) {
        expr.getTarget().accept(this, blockStmt);
        writer.writeSpace();
        switch (expr.getOperator()) {
            case ASSIGN:
                writer.write("=");
                break;

            case PLUS:
                writer.write("+=");
                break;

            case MINUS:
                writer.write("-=");
                break;

            case MUL:
                writer.write("*=");
                break;

            case DIV:
                writer.write("/=");
                break;

            default:
                throw new AssertionError();
        }
        writer.writeSpace();
        expr.getValue().accept(this, blockStmt);
        return null;
    }

    public Void visit(UnaryExpression expr, BlockStatement blockStmt) {
        switch (expr.getOperator()) {
            case MINUS:
                writer.write("-");
                expr.getExpr().accept(this, blockStmt);
                break;

            case POST_DECREMENT:
                expr.getExpr().accept(this, blockStmt);
                writer.write("--");
                break;

            case POST_INCREMENT:
                expr.getExpr().accept(this, blockStmt);
                writer.write("++");
                break;

            case PRE_DECREMENT:
                writer.write("--");
                expr.getExpr().accept(this, blockStmt);
                break;

            case PRE_INCREMENT:
                writer.write("++");
                expr.getExpr().accept(this, blockStmt);
                break;

            default:
                throw new AssertionError();
        }
        return null;
    }

    public Void visit(BinaryExpression expr, BlockStatement blockStmt) {
        int depth = expr.getDepth();
        if (depth > 0) {
            writer.write("(");
        }
        expr.getLeft().accept(this, blockStmt);
        writer.writeSpace();
        switch (expr.getOperator()) {
            case PLUS:
                writer.write("+");
                break;

            case MINUS:
                writer.write("-");
                break;

            case MUL:
                writer.write("*");
                break;

            case DIV:
                writer.write("/");
                break;

            case REMAINDER:
                writer.write("%");
                break;

            case EQ:
                writer.write("==");
                break;

            case NE:
                writer.write("!=");
                break;

            case GE:
                writer.writeGt();
                writer.write("=");
                break;

            case GT:
                writer.writeGt();
                break;

            case LT:
                writer.writeLt();
                break;

            case LE:
                writer.writeLt();
                writer.write("=");
                break;

            case INSTANCE_OF:
                writer.write("instanceof");
                break;

            case AND:
                writer.writeAmpersand();
                writer.writeAmpersand();
                break;

            case OR:
                writer.write("||");
                break;

            case XOR:
                writer.write("^");
                break;

            case SHIFT_LEFT:
                writer.write("<<");
                break;

            case SHIFT_RIGHT:
                writer.writeGt();
                writer.writeGt();
                break;

            case LOGICAL_SHIFT_RIGHT:
                writer.writeGt();
                writer.writeGt();
                writer.writeGt();
                break;

            default:
                throw new AssertionError();
        }
        writer.writeSpace();
        expr.getRight().accept(this, blockStmt);
        if (depth > 0) {
            writer.write(")");
        }
        return null;
    }

    public Void visit(LocalVariable expr, BlockStatement blockStmt) {
        writer.write(expr.getName());
        return null;
    }

    public Void visit(TypeExpression expr, BlockStatement blockStmt) {
        writer.write(expr.getType());
        return null;
    }

    public Void visit(FieldAccess expr, BlockStatement blockStmt) {
        expr.getScope().accept(this, blockStmt);
        writer.write(".").write(expr.getFieldName());
        return null;
    }

    public Void visit(MethodCall expr, BlockStatement blockStmt) {
        if (expr.getScope() != null) {
            expr.getScope().accept(this, blockStmt);
            writer.write(".");
        }
        writer.write(expr.getMethodName()).write("(");
        for (Iterator<Expression> it = expr.getArguments().iterator(); it.hasNext();) {
            Expression argument = it.next();
            argument.accept(this, blockStmt);
            if (it.hasNext()) {
                writer.write(",").writeSpace();
            }
        }
        writer.write(")");
        return null;
    }

    public Void visit(ConditionalExpression expr, BlockStatement blockStmt) {
        writer.write("(");
        expr.getCondition().accept(this, blockStmt);
        writer.writeSpace().write("?").writeSpace();
        expr.getThen().accept(this, blockStmt);
        writer.writeSpace().write(":").writeSpace();
        expr.getElse().accept(this, blockStmt);
        writer.write(")");
        return null;
    }

    public Void visit(ObjectCreationExpression expr, BlockStatement blockStmt) {
        writer.writeKeyword("new").writeSpace().write(expr.getType()).write("(");
        if (expr.getArguments() != null) {
            for (Iterator<Expression> it = expr.getArguments().iterator(); it.hasNext();) {
                Expression argument = it.next();
                argument.accept(this, blockStmt);
                if (it.hasNext()) {
                    writer.write(",").writeSpace();
                }
            }
        }
        writer.write(")");
        return null;
    }

    public Void visit(ArrayCreationExpression expr, BlockStatement blockStmt) {
        if (expr.getInitValues() != null) {
            writer.write("{");
            for (Iterator<Expression> it = expr.getInitValues().iterator(); it.hasNext();) {
                it.next().accept(this, blockStmt);
                if (it.hasNext()) {
                    writer.write(",").writeSpace();
                }
            }
            writer.write("}");
        } else {
            writer.writeKeyword("new").writeSpace().write(expr.getType());
            for (Expression arrayLengthExpr : expr.getArrayLengthExprs()) {
                writer.write("[");
                arrayLengthExpr.accept(this, blockStmt);
                writer.write("]");
            }
        }
        return null;
    }

    public Void visit(ArrayLength expr, BlockStatement blockStmt) {
        expr.getArrayRef().accept(this, blockStmt);
        writer.write(".length");
        return null;
    }

    public Void visit(CastExpression expr, BlockStatement blockStmt) {
        writer.write("((").write(expr.getType()).write(")").writeSpace();
        expr.getExpr().accept(this, blockStmt);
        writer.write(")");
        return null;
    }

    public Void visit(ArrayAccess expr, BlockStatement blockStmt) {
        expr.getArrayRef().accept(this, blockStmt);
        writer.write("[");
        expr.getArrayIndexExpr().accept(this, blockStmt);
        writer.write("]");
        return null;
    }

    public Void visit(ChoiceExpression choiceExpr, BlockStatement blockStmt) {
        if (choiceExpr.getChoices().size() == 1) {
            choiceExpr.getChoices().iterator().next().accept(this, blockStmt);
        } else {
            writer.write("?(");
            for (Iterator<Expression> it = choiceExpr.getChoices().iterator(); it.hasNext();) {
                Expression expr = it.next();
                expr.accept(this, blockStmt);
                if (it.hasNext()) {
                    writer.write(", ");
                }
            }
            writer.write(")");
        }
        return null;
    }

    public Void visit(IntegerLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(LongLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(ByteLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(ShortLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(BooleanLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(FloatLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(DoubleLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getValue());
        return null;
    }

    public Void visit(StringLiteralExpression expr, BlockStatement arg) {
        writer.writeQuotedString(expr.getValue().toString());
        return null;
    }

    public Void visit(NullLiteralExpression expr, BlockStatement arg) {
        writer.write("null");
        return null;
    }

    public Void visit(ClassLiteralExpression expr, BlockStatement arg) {
        writer.write(expr.getClassName()).write(".class");
        return null;
    }
}
