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

package fr.jamgotchian.abcd.core.ast.expr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ArrayCreationExpression extends AbstractExpression {

    private final String typeName;

    private final List<Expression> arrayLengthExprs;

    private List<Expression> initValues;

    ArrayCreationExpression(String typeName, Expression arrayLengthExpr) {
        this.typeName = typeName;
        this.arrayLengthExprs = Collections.singletonList(arrayLengthExpr);
    }

    ArrayCreationExpression(String typeName, List<Expression> arrayLengthExprs) {
        this.typeName = typeName;
        this.arrayLengthExprs = arrayLengthExprs;
    }

    public String getTypeName() {
        return typeName;
    }

    public List<Expression> getArrayLengthExprs() {
        return arrayLengthExprs;
    }

    public void addInitValue(Expression value) {
        if (initValues == null) {
            initValues = new ArrayList<Expression>();
        }
        initValues.add(value);
    }

    public List<Expression> getInitValues() {
        return initValues;
    }

    public <R, A> R accept(ExpressionVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
