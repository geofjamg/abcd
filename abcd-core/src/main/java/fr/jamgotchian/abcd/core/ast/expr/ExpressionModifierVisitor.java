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

    @Override
    public Expression visit(IntegerLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(LongLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(ByteLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(ShortLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(BooleanLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(FloatLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(DoubleLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(StringLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(NullLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(ClassLiteralExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(VariableExpression var, Void arg) {
        return null;
    }

    @Override
    public Expression visit(UnaryExpression expr, Void arg) {
        Expression newExpr = expr.getExpr().accept(this, arg);
        if (newExpr == null) {
            return null;
        } else {
            return new UnaryExpression(newExpr, expr.getOperator());
        }
    }

    @Override
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

    @Override
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

    @Override
    public Expression visit(TypeExpression expr, Void arg) {
        return null;
    }

    @Override
    public Expression visit(FieldAccess expr, Void arg) {
        Expression newScope = expr.getScope().accept(this, arg);
        if (newScope == null) {
            return null;
        } else {
            return new FieldAccess(newScope, expr.getFieldName());
        }
    }

    @Override
    public Expression visit(MethodCall expr, Void arg) {
        Expression newScope = null;
        if (expr.getScope() != null) {
            newScope = expr.getScope().accept(this, arg);
        }
        List<Expression> newArguments = new ArrayList<>(expr.getArguments());
        boolean argumentsModified = false;
        for (int i = 0; i < newArguments.size(); i++) {
            Expression newArgument = newArguments.get(i).accept(this, arg);
            if (newArgument != null) {
                newArguments.set(i, newArgument);
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

    @Override
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

    @Override
    public Expression visit(ObjectCreationExpression expr, Void arg) {
        List<Expression> newArguments = new ArrayList<>(expr.getArguments());
        boolean argumentsModified = false;
        for (int i = 0; i < newArguments.size(); i++) {
            Expression newArgument = newArguments.get(i).accept(this, arg);
            if (newArgument != null) {
                newArguments.set(i, newArgument);
                argumentsModified = true;
            }
        }
        if (argumentsModified) {
            return new ObjectCreationExpression(expr.getType(), newArguments);
        } else {
            return null;
        }
    }

    @Override
    public Expression visit(ArrayCreationExpression expr, Void arg) {
        List<Expression> newArrayLengthExprs = new ArrayList<>(expr.getArrayLengthExprs());
        boolean lengthModified = false;
        for (int i = 0; i < newArrayLengthExprs.size(); i++) {
            Expression newArrayLengthExpr = newArrayLengthExprs.get(i).accept(this, arg);
            if (newArrayLengthExpr != null) {
                newArrayLengthExprs.set(i, newArrayLengthExpr);
                lengthModified = true;
            }
        }
        if (lengthModified) {
            return new ArrayCreationExpression(expr.getType(), newArrayLengthExprs);
        } else {
            return null;
        }
    }

    @Override
    public Expression visit(ArrayLength expr, Void arg) {
        Expression newArrayRef = expr.getArrayRef().accept(this, arg);
        if (newArrayRef == null) {
            return null;
        } else {
            return new ArrayLength(newArrayRef);
        }
    }

    @Override
    public Expression visit(CastExpression expr, Void arg) {
        Expression newExpr = expr.getExpr().accept(this, arg);
        if (newExpr == null) {
            return null;
        } else {
            return new CastExpression(expr.getType(), newExpr);
        }
    }

    @Override
    public Expression visit(ArrayAccess expr, Void arg) {
        Expression newArrayRef = expr.getArrayRef().accept(this, arg);
        Expression newArrayIndex = expr.getArrayIndexExpr().accept(this, arg);
        if (newArrayRef == null && newArrayIndex == null) {
            return null;
        } else {
            return new ArrayAccess(newArrayRef != null ? newArrayRef : expr.getArrayRef(),
                                   newArrayIndex != null ? newArrayIndex : expr.getArrayIndexExpr());
        }
    }

    @Override
    public Expression visit(ChoiceExpression choiceExpr, Void arg) {
        Set<Expression> newChoices = new HashSet<>();
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
