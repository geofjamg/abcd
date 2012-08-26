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

import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.VariableExpression;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariableDeclarationStatement extends AbstractStatement {

    private final VariableExpression varExpr;

    private final Expression initExpr;

    public LocalVariableDeclarationStatement(VariableExpression varExpr) {
        this(varExpr, null);
    }

    public LocalVariableDeclarationStatement(VariableExpression varExpr, Expression initExpr) {
        if (varExpr == null) {
            throw new IllegalArgumentException("varExpr == null");
        }
        this.varExpr = varExpr;
        this.initExpr = initExpr;
    }

    public VariableExpression getVarExpr() {
        return varExpr;
    }

    public Expression getInitExpr() {
        return initExpr;
    }

    @Override
    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
