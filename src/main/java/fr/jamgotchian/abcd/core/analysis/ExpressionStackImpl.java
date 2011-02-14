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

package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.output.JavaExpressionWriter;
import fr.jamgotchian.abcd.core.output.TextCodeWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class ExpressionStackImpl implements ExpressionStack {

    private static final Logger logger = Logger.getLogger(ExpressionStackImpl.class.getName());

    static {
        logger.setLevel(Level.FINER);
    }

    private final ArrayDeque<Expression> stack;

    ExpressionStackImpl() {
        stack = new ArrayDeque<Expression>();
    }

    ExpressionStackImpl(ExpressionStackImpl other) {
        if (other == null) {
            throw new IllegalArgumentException("other == null");
        }
        stack = other.stack.clone();
    }

    public void push(Expression expr) {
        if (expr == null) {
            throw new IllegalArgumentException("expr == null");
        }
        stack.push(expr);
        logger.log(Level.FINEST, "Push {0} -> {1}", new Object[] {toString(expr), toString(stack)});
    }

    public Expression pop() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("operand stack is empty");
        }
        Expression expr = stack.pop();
        logger.log(Level.FINEST, "Pop {0} -> {1}", new Object[] {toString(expr), toString(stack)});
        return expr;
    }

    public Expression peek() {
        if (stack.isEmpty()) {
            throw new IllegalStateException("stack is empty");
        }
        return stack.peek();
    }

    public int size() {
        return stack.size();
    }

    public List<Expression> toList() {
        return new ArrayList<Expression>(stack);
    }

    @Override
    public ExpressionStack clone() {
        return new ExpressionStackImpl(this);
    }

    public static String toString(Expression expr) {
        return toString(Collections.singleton(expr));
    }

    public static String toString(Collection<Expression> exprs) {
        StringBuilder builder = new StringBuilder("[");
        Iterator<Expression> it = exprs.iterator();
        while (it.hasNext()) {
            Expression expr = it.next();

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
            builder.append(writer);
            if (it.hasNext()) {
                builder.append(", ");
            }
        }
        builder.append("]");
        return builder.toString();
    }

    @Override
    public String toString() {
        return toString(stack);
    }

}
