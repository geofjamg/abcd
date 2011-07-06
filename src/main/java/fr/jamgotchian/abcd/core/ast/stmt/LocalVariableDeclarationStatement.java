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
import fr.jamgotchian.abcd.core.type.JavaType;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariableDeclarationStatement extends AbstractStatement {

    private final LocalVariableDeclaration localVarDecl;

    private final Expression initExpr;

    public LocalVariableDeclarationStatement(LocalVariableDeclaration localVarDecl) {
        this(localVarDecl, null);
    }

    public LocalVariableDeclarationStatement(LocalVariableDeclaration localVarDecl, Expression initExpr) {
        if (localVarDecl == null) {
            throw new IllegalArgumentException("localVarDecl == null");
        }
        this.localVarDecl = localVarDecl;
        this.initExpr = initExpr;
    }

    public LocalVariableDeclaration getLocalVarDecl() {
        return localVarDecl;
    }

    public Expression getInitExpr() {
        return initExpr;
    }

    public <R, A> R accept(StatementVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

}
