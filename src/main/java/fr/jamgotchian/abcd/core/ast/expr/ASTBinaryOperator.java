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

import fr.jamgotchian.abcd.core.common.ABCDException;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public enum ASTBinaryOperator {
    PLUS,
    MINUS,
    MUL,
    DIV,
    LE,
    LT,
    GE,
    GT,
    EQ,
    NE,
    REMAINDER,
    SHIFT_LEFT,
    SHIFT_RIGHT,
    LOGICAL_SHIFT_RIGHT,
    AND,
    OR,
    XOR,
    INSTANCE_OF;

    public static ASTBinaryOperator getInverse(ASTBinaryOperator op) {
        switch (op) {
            case PLUS:
            case MINUS:
            case MUL:
            case DIV:
            case REMAINDER:
            case SHIFT_LEFT:
            case SHIFT_RIGHT:
            case INSTANCE_OF:
                return op;
            case LE:
                return GT;
            case LT:
                return GE;
            case GE:
                return LT;
            case GT:
                return LE;
            case EQ:
                return NE;
            case NE:
                return EQ;
            case AND:
                return OR;
            case OR:
                return AND;
            default:
                throw new ABCDException("Impossible to invert " + op);
        }
    }

    public ASTBinaryOperator getInverse() {
        return getInverse(this);
    }
}
