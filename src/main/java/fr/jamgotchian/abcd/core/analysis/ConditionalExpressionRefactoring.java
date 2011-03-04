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

package fr.jamgotchian.abcd.core.analysis;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.ast.expr.ChoiceExpression;
import fr.jamgotchian.abcd.core.ast.expr.ConditionalExpression;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.ExpressionModifierVisitor;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.BreakStatement;
import fr.jamgotchian.abcd.core.ast.stmt.CommentStatement;
import fr.jamgotchian.abcd.core.ast.stmt.DoWhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ForStatement;
import fr.jamgotchian.abcd.core.ast.stmt.GotoStatement;
import fr.jamgotchian.abcd.core.ast.stmt.IfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.JumpIfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabelStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclarationStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LookupOrTableSwitchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ReturnStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.StatementVisitor;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ThrowStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement.CatchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import fr.jamgotchian.abcd.core.ast.util.ExpressionInverter;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockType;
import fr.jamgotchian.abcd.core.controlflow.DominatorInfo;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ConditionalExpressionRefactoring implements StatementVisitor<Object, Object> {
    
    private static final Logger logger = Logger.getLogger(ConditionalExpressionRefactoring.class.getName());

    private static class ChoiceExpressionRemover extends ExpressionModifierVisitor {

        @Override
        public Expression visit(ChoiceExpression choiceExpr, Void arg) {
            
            ChoiceExpression oldChoiceExpr = new ChoiceExpression(new HashSet<Expression>(choiceExpr.getChoices()));
            
            boolean change = true;
            while (change) {
                change = false;

                Multimap<BasicBlock, Expression> forkBlocks = HashMultimap.create();
                for (Expression expr : choiceExpr.getChoices()) {
                    BasicBlock block = expr.getBasicBlock();
                    DominatorInfo dominatorInfo = block.getGraph().getDominatorInfo();
                    BasicBlock forkBlock = dominatorInfo.getDominatorsTree().getParent(block);
                    forkBlocks.put(forkBlock, expr);
                }

                for (Map.Entry<BasicBlock, Collection<Expression>> entry : forkBlocks.asMap().entrySet()) {
                    BasicBlock forkBlock = entry.getKey();
                    Collection<Expression> exprs = entry.getValue();
                    if (forkBlock.getType() == BasicBlockType.JUMP_IF && exprs.size() == 2) {
                        Iterator<Expression> it = exprs.iterator();
                        Expression expr1 = it.next();
                        Expression expr2 = it.next();

                        BasicBlock block1 = expr1.getBasicBlock();
                        BasicBlock block2 = expr2.getBasicBlock();
                        DominatorInfo dominatorInfo = forkBlock.getGraph().getDominatorInfo();
                        Edge forkEdge1 = dominatorInfo.getPostDominanceFrontierOf(block1).iterator().next();
                        Edge forkEdge2 = dominatorInfo.getPostDominanceFrontierOf(block2).iterator().next();
                        Expression thenExpr = null; 
                        Expression elseExpr = null;
                        if (Boolean.TRUE.equals(forkEdge1.getValue()) && Boolean.FALSE.equals(forkEdge2.getValue())) {
                            thenExpr = expr1; 
                            elseExpr = expr2; 
                        } else if (Boolean.FALSE.equals(forkEdge1.getValue()) && Boolean.TRUE.equals(forkEdge2.getValue())) {
                            thenExpr = expr2; 
                            elseExpr = expr1; 
                        }
                        if (thenExpr != null && elseExpr != null) {
                            BasicBlockAnalysisDataImpl forkData = (BasicBlockAnalysisDataImpl) forkBlock.getData();
                            JumpIfStatement jumpIfStmt = (JumpIfStatement) forkData.getLastStatement();
                            Expression condition = ExpressionInverter.invert(jumpIfStmt.getCondition());
                            Expression condExpr = new ConditionalExpression(condition, elseExpr, thenExpr);
                            condExpr.setBasicBlock(forkBlock);
                            choiceExpr.getChoices().remove(expr1);
                            choiceExpr.getChoices().remove(expr2);
                            choiceExpr.getChoices().add(condExpr);

                            change = true;
                        } else {
                            logger.log(Level.SEVERE, "Conditional expression refactoring error");
                        }
                    }
                }
            }

            if (choiceExpr.getChoices().size() == 1) {
                Expression condExpr = choiceExpr.getChoices().iterator().next();
                logger.log(Level.FINEST, "Refactor choice : {0} -> {1}", 
                        new Object[] {ExpressionStackImpl.toString(oldChoiceExpr), 
                                      ExpressionStackImpl.toString(condExpr)}); 
                return condExpr;
            } else {
                logger.log(Level.SEVERE, "Conditional expression refactoring error");
                return null;
            }
        }
    }

    private final ChoiceExpressionRemover choiceExprRemover = new ChoiceExpressionRemover();

    public Object visit(BlockStatement block, Object arg) {
        for (Statement stmt : block) {
            stmt.accept(this, arg);
        }
        return null;
    }

    public Object visit(ReturnStatement stmt, Object arg) {
        if (stmt.getExpression() != null) {
            stmt.getExpression().accept(choiceExprRemover, null);
        }
        return null;
    }

    public Object visit(LocalVariableDeclarationStatement stmt, Object arg) {
        return null;
    }

    public Object visit(ExpressionStatement stmt, Object arg) {
        if (stmt.getExpression() != null) {
            stmt.getExpression().accept(choiceExprRemover, null);
        }
        return null;
    }

    public Object visit(CommentStatement stmt, Object arg) {
        return null;
    }

    public Object visit(IfStatement stmt, Object arg) {
        stmt.getCondition().accept(choiceExprRemover, null);
        stmt.getThen().accept(this, arg);
        if (stmt.getElse() != null) {
            stmt.getElse().accept(this, arg);
        }
        return null;
    }

    public Object visit(TryCatchFinallyStatement stmt, Object arg) {
        stmt.getTry().accept(this, arg);
        for (CatchStatement _catch : stmt.getCatchs()) {
            _catch.getBlockStmt().accept(this, arg);
        }
        if (stmt.getFinally() != null) {
            stmt.getFinally().accept(this, arg);
        }
        return null;
    }

    public Object visit(BreakStatement stmt, Object arg) {
        return null;
    }

    public Object visit(WhileStatement stmt, Object arg) {
        stmt.getCondition().accept(choiceExprRemover, null);
        stmt.getBody().accept(this, arg);
        return null;
    }

    public Object visit(DoWhileStatement stmt, Object arg) {
        stmt.getCondition().accept(choiceExprRemover, null);
        stmt.getBody().accept(this, arg);
        return null;
    }

    public Object visit(ForStatement stmt, Object arg) {
        if (stmt.getInit() != null) {
            stmt.getInit().accept(choiceExprRemover, null);
        }
        if (stmt.getCondition() != null) {
            stmt.getCondition().accept(choiceExprRemover, null);
        }
        if (stmt.getUpdate() != null) {
            stmt.getUpdate().accept(choiceExprRemover, null);
        }
        stmt.getBody().accept(this, arg);
        return null;
    }

    public Object visit(ThrowStatement stmt, Object arg) {
        stmt.getObjectRef().accept(choiceExprRemover, null);
        return null;
    }

    public Object visit(JumpIfStatement stmt, Object arg) {
        return null;
    }

    public Object visit(GotoStatement stmt, Object arg) {
        return null;
    }

    public Object visit(LabelStatement stmt, Object arg) {
        return null;
    }

    public Object visit(LookupOrTableSwitchStatement stmt, Object arg) {
        return null;
    }

    public Object visit(SwitchCaseStatement stmt, Object arg) {
        return null;
    }
}
