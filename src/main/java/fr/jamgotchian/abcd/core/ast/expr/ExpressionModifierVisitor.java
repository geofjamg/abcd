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

package fr.jamgotchian.abcd.core.ast.expr;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ExpressionModifierVisitor implements ExpressionVisitor<Expression, Void> {

    public Expression visit(Constant cst, Void arg) {
        return null;
    }

    public Expression visit(LocalVariable var, Void arg) {
        return null;
    }

    public Expression visit(UnaryExpression expr, Void arg) {
        Expression newExpr = expr.getExpr().accept(this, arg);
        if (newExpr == null) {
            return null;
        } else {
            return new UnaryExpression(newExpr, expr.getOperator());
        }
    }

    public Expression visit(BinaryExpression expr, Void arg) {
        Expression newLeft = expr.getLeft().accept(this, arg);
        Expression newRight = expr.getRight().accept(this, arg);
        if (newLeft == null && newRight == null) {
            return null;
        } else {
            return new BinaryExpression(newLeft != null ? newLeft : expr.getLeft(), 
                                        newRight != null ? newRight : expr.getRight(), 
                                        expr.getOperator());
        }
    }

    public Expression visit(AssignExpression expr, Void arg) {
        Expression newtarget = expr.getTarget().accept(this, arg);
        Expression newValue = expr.getValue().accept(this, arg);
        if (newtarget == null && newValue == null) {
            return null;
        } else {
            return new AssignExpression(newtarget != null ? newtarget : expr.getTarget(), 
                                        newValue != null ? newValue : expr.getValue(), expr.getOperator());
        }
    }

    public Expression visit(ClassExpression expr, Void arg) {
        return null;
    }

    public Expression visit(FieldAccess expr, Void arg) {
        Expression newScope = expr.getScope().accept(this, arg);
        if (newScope == null) {
            return null;
        } else {
            return new FieldAccess(newScope, expr.getFieldName());
        }
    }

    public Expression visit(MethodCall expr, Void arg) {
        Expression newScope = expr.getScope().accept(this, arg);
        List<Expression> newArguments = new ArrayList<Expression>(expr.getArguments());
        boolean argumentsModified = false;
        for (int i = 0; i < newArguments.size(); i++) {
            Expression argument = newArguments.get(i).accept(this, arg);
            if (argument != null) {
                newArguments.set(i, argument);
                argumentsModified = true;
            }
        }
        if (newScope != null || argumentsModified) {
            return new MethodCall(newScope != null ? newScope : expr.getScope(), 
                                  expr.getMethodName(), 
                                  argumentsModified ? newArguments : expr.getArguments());
        } else {
            return null;
        }
    }

    public Expression visit(ConditionalExpression expr, Void arg) {
        Expression newCond = expr.getCondition().accept(this, arg);
        Expression newThen = expr.getThen().accept(this, arg);
        Expression newElse = expr.getElse().accept(this, arg);
        if (newCond == null && newThen == null && newElse == null) {
            return null;
        } else {
            return new ConditionalExpression(newCond != null ? newCond : expr.getCondition(), 
                                             newThen != null ? newThen : expr.getThen(), 
                                             newElse != null ? newElse : expr.getElse());
        }
    }

    public Expression visit(ObjectCreationExpression expr, Void arg) {
        List<Expression> newArguments = new ArrayList<Expression>(expr.getArguments());
        boolean argumentsModified = false;
        for (int i = 0; i < newArguments.size(); i++) {
            Expression argument = newArguments.get(i).accept(this, arg);
            if (argument != null) {
                newArguments.set(i, argument);
                argumentsModified = true;
            }
        }
        if (argumentsModified) {
            return new ObjectCreationExpression(expr.getClassName(), newArguments);
        } else {
            return null;
        }
    }

    public Expression visit(ArrayCreationExpression expr, Void arg) {
        Expression newArrayCountExpr = expr.getArrayCountExpr().accept(this, arg);
        if (newArrayCountExpr == null) {
            return null;
        } else {
            return new ArrayCreationExpression(expr.getTypeName(), newArrayCountExpr);
        }
    }

    public Expression visit(ArrayLength expr, Void arg) {
        Expression newArrayRef = expr.getArrayRef().accept(this, arg);
        if (newArrayRef == null) {
            return null;
        } else {
            return new ArrayLength(newArrayRef);
        }
    }

    public Expression visit(CastExpression expr, Void arg) {
        Expression newExpr = expr.getExpr().accept(this, arg);
        if (newExpr == null) {
            return null;
        } else {
            return new CastExpression(expr.getClassName(), newExpr);
        }
    }

    public Expression visit(ArrayAccess expr, Void arg) {
        Expression newArrayRef = expr.getArrayRef().accept(this, arg);
        Expression newArrayCount = expr.getArrayCountExpr().accept(this, arg);
        if (newArrayRef == null && newArrayCount == null) {
            return null;
        } else {
            return new ArrayAccess(newArrayRef != null ? newArrayRef : expr.getArrayRef(), 
                                   newArrayCount != null ? newArrayCount : expr.getArrayCountExpr());
        }
    }

    public Expression visit(ChoiceExpression choiceExpr, Void arg) {
        Set<Expression> newChoices = new HashSet<Expression>();
        boolean modified = false;
        for (Expression expr : choiceExpr.getChoices()) {
            Expression newExpr = expr.accept(this, arg);
            if (newExpr == null) {
                newChoices.add(expr);
            } else {
                newChoices.add(newExpr);
                modified = true;
            }
        }
        if (modified) {
            return new ChoiceExpression(newChoices);
        } else {
            return null;
        }
    }
}
