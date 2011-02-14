/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

import fr.jamgotchian.abcd.core.ast.expr.ArrayAccess;
import fr.jamgotchian.abcd.core.ast.expr.ArrayCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.ArrayLength;
import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.CastExpression;
import fr.jamgotchian.abcd.core.ast.expr.ClassExpression;
import fr.jamgotchian.abcd.core.ast.expr.ConditionalExpression;
import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.ChoiceExpression;
import fr.jamgotchian.abcd.core.ast.expr.FieldAccess;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.expr.MethodCall;
import fr.jamgotchian.abcd.core.ast.expr.ObjectCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.UnaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.ExpressionVisitor;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ExpressionEqualityChecker implements ExpressionVisitor<Boolean, Expression> {

    private static final ExpressionEqualityChecker instance = new ExpressionEqualityChecker();

    public static boolean equal(Expression expr1, Expression expr2) {
        return expr1.accept(instance, expr2);
    }

    public Boolean visit(Constant expr1, Expression expr2) {
        if (expr2 instanceof Constant) {
            return ((Constant) expr2).getValue().equals(expr1.getValue());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(LocalVariable expr1, Expression expr2) {
        if (expr2 instanceof LocalVariable) {
            return ((LocalVariable) expr2).getIndex() == expr1.getIndex();
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(UnaryExpression expr1, Expression expr2) {
        if (expr2 instanceof UnaryExpression) {
            return expr1.getOperator() == ((UnaryExpression) expr2).getOperator() &&
                   expr1.getExpr().accept(this, ((UnaryExpression) expr2).getExpr());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(BinaryExpression expr1, Expression expr2) {
        if (expr2 instanceof BinaryExpression) {
            return expr1.getOperator() == ((BinaryExpression) expr2).getOperator() &&
                   expr1.getLeft().accept(this, ((BinaryExpression) expr2).getLeft()) &&
                   expr1.getRight().accept(this, ((BinaryExpression) expr2).getRight());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(AssignExpression expr1, Expression expr2) {
        if (expr2 instanceof AssignExpression) {
            return expr1.getOperator() == ((AssignExpression) expr2).getOperator() &&
                   expr1.getTarget().accept(this, ((AssignExpression) expr2).getTarget()) &&
                   expr1.getValue().accept(this, ((AssignExpression) expr2).getValue());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ClassExpression expr1, Expression expr2) {
        if (expr2 instanceof ClassExpression) {
            return ((ClassExpression) expr2).getClassName().equals(expr1.getClassName());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(FieldAccess expr1, Expression expr2) {
        if (expr2 instanceof FieldAccess) {
            return expr1.getScope().accept(this, ((FieldAccess) expr2).getScope()) &&
                   expr1.getFieldName().equals(((FieldAccess) expr2).getFieldName());
        } else {
            return Boolean.FALSE;
        }
    }

    private static boolean nullableListEquals(List<?> l1, List<?> l2) {
        return (l1 == null ? -1 : l1.size()) == (l2 == null ? -1 : l2.size());
    }

    public Boolean visit(MethodCall expr1, Expression expr2) {
        if (expr2 instanceof MethodCall) {
            if (!expr1.getMethodName().equals(((MethodCall) expr2).getMethodName()) ||
                expr1.getScope().accept(this, ((MethodCall) expr2).getScope()).equals(Boolean.FALSE) ||
                !nullableListEquals(expr1.getArguments(), ((MethodCall) expr2).getArguments())) {
                return Boolean.FALSE;
            } else {
                if (expr1.getArguments() != null) {
                    for (int i = 0; i < expr1.getArguments().size(); i++) {
                        if (expr1.getArguments().get(i).accept(this, ((MethodCall) expr2).getArguments().get(i)).equals(Boolean.FALSE)) {
                            return Boolean.FALSE;
                        }
                    }
                }
                return Boolean.TRUE;
            }
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ConditionalExpression expr1, Expression expr2) {
        if (expr2 instanceof ConditionalExpression) {
            return expr1.getCondition().accept(this, ((ConditionalExpression) expr2).getCondition()) &&
                   expr1.getThen().accept(this, ((ConditionalExpression) expr2).getThen()) &&
                   expr1.getElse().accept(this, ((ConditionalExpression) expr2).getElse());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ObjectCreationExpression expr1, Expression expr2) {
        if (expr2 instanceof ObjectCreationExpression) {
            if (!expr1.getClassName().equals(((ObjectCreationExpression) expr2).getClassName()) ||
                !nullableListEquals(expr1.getArguments(), ((ObjectCreationExpression) expr2).getArguments())) {
                return Boolean.FALSE;
            } else {
                if (expr1.getArguments() != null) {
                    for (int i = 0; i < expr1.getArguments().size(); i++) {
                        if (expr1.getArguments().get(i).accept(this, ((ObjectCreationExpression) expr2).getArguments().get(i)).equals(Boolean.FALSE)) {
                            return Boolean.FALSE;
                        }
                    }
                }
                return Boolean.TRUE;
            }
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ArrayCreationExpression expr1, Expression expr2) {
        if (expr2 instanceof ArrayCreationExpression) {
            if (!expr1.getTypeName().equals(((ArrayCreationExpression) expr2).getTypeName()) ||
                expr1.getArrayCountExpr().accept(this, ((ArrayCreationExpression) expr2).getArrayCountExpr()).equals(Boolean.FALSE) ||
                !nullableListEquals(expr1.getInitValues(), ((ArrayCreationExpression) expr2).getInitValues())) {
                return Boolean.FALSE;
            } else {
                if (expr1.getInitValues() != null) {
                    for (int i = 0; i < expr1.getInitValues().size(); i++) {
                        if (expr1.getInitValues().get(i).accept(this, ((ArrayCreationExpression) expr2).getInitValues().get(i)).equals(Boolean.FALSE)) {
                            return Boolean.FALSE;
                        }
                    }
                }
                return Boolean.TRUE;
            }
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ArrayLength expr1, Expression expr2) {
        if (expr2 instanceof ArrayLength) {
            return expr1.getArrayRef().accept(this, ((ArrayLength) expr2).getArrayRef());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(CastExpression expr1, Expression expr2) {
        if (expr2 instanceof CastExpression) {
            return expr1.getExpr().accept(this, ((CastExpression) expr2).getExpr()) &&
                   expr1.getClassName().equals(((CastExpression) expr2).getClassName());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ArrayAccess expr1, Expression expr2) {
        if (expr2 instanceof ArrayAccess) {
            return expr1.getArrayRef().accept(this, ((ArrayAccess) expr2).getArrayRef()) &&
                   expr1.getArrayCountExpr().accept(this, ((ArrayAccess) expr2).getArrayCountExpr());
        } else {
            return Boolean.FALSE;
        }
    }

    public Boolean visit(ChoiceExpression exprChoice1, Expression otherExpr) {
        if (otherExpr instanceof ChoiceExpression) {
            ChoiceExpression exprChoice2 = (ChoiceExpression) otherExpr;
            if (exprChoice1.getChoices().size() != exprChoice2.getChoices().size()) {
                return Boolean.FALSE;
            } else {
                Set<Expression> exprs1 = exprChoice1.getChoices();
                Set<Expression> exprs2 = exprChoice2.getChoices();
                for (Iterator<Expression> it1 = exprs1.iterator(); it1.hasNext();) {
                    Expression expr1 = it1.next();
                    for (Iterator<Expression> it2 = exprs2.iterator(); it2.hasNext();) {
                        Expression expr2 = it2.next();
                        if (Boolean.TRUE.equals(expr1.accept(this, expr2))) {
                            it1.remove();
                            it2.remove();
                            break;
                        }
                    }
                }
                return (exprs1.isEmpty() && exprs2.isEmpty());
            }
        } else {
            return Boolean.FALSE;            
        }
    }

}
