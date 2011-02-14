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
package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.DoWhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.GotoStatement;
import fr.jamgotchian.abcd.core.ast.stmt.IfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.JumpIfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabelStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.Statements;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import fr.jamgotchian.abcd.core.ast.util.ExpressionInverter;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.region.BlockRegion;
import fr.jamgotchian.abcd.core.region.IfThenElseRegion;
import fr.jamgotchian.abcd.core.region.IfThenRegion;
import fr.jamgotchian.abcd.core.region.LeafRegion;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.LoopRegion;
import fr.jamgotchian.abcd.core.region.LoopSubRegion;
import fr.jamgotchian.abcd.core.region.LoopType;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.region.LogicalRegion;
import fr.jamgotchian.abcd.core.region.LogicalType;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class AbstractSyntaxTreeBuilder {

    private static final Logger logger = Logger.getLogger(AbstractSyntaxTreeBuilder.class.getName());

    public AbstractSyntaxTreeBuilder() {
    }

    public void build(Tree<Region, Edge> controlTree, BlockStatement methodBody) {
        buildAST(controlTree.getRoot(), controlTree, methodBody);
    }

    private boolean isStmtBlockEmpty(BlockStatement blockStmt) {
        for (Statement stmt : blockStmt) {
            if (!(stmt instanceof LabelStatement
                    && stmt instanceof GotoStatement)) {
                return false;
            }
        }
        return true;
    }

    private void buildAST(Region region, Tree<Region, Edge> controlTree, BlockStatement blockStmt) {
        switch (region.getType()) {
            case LEAF: {
                LeafRegion leaf = (LeafRegion) region;
                for (Statement stmt : ((BasicBlockAnalysisDataImpl) leaf.getBlock().getData()).getUsefullStatements()) {
                    blockStmt.add(stmt);
                }
                break;
            }

            case BLOCK: {
                for (Region child : ((BlockRegion) region).getRegions()) {
                    buildAST(child, controlTree, blockStmt);
                }
                break;
            }

            case LOGICAL: {
                LogicalRegion logical = (LogicalRegion) region;
                
                buildAST(logical.getRegionA(), controlTree, blockStmt);
                JumpIfStatement jumpIfStmtA = (JumpIfStatement) blockStmt.getLast();
                
                BlockStatement tmpBlockStmt = new BlockStatement();
                buildAST(logical.getRegionB(), controlTree, tmpBlockStmt);
                JumpIfStatement jumpIfStmtB = (JumpIfStatement) tmpBlockStmt.getLast();
                
                BinaryOperator operator = null;
                switch (logical.getLogicalType()) {
                    case AND:
                    case AND_INVERT_B:
                        operator = BinaryOperator.AND;
                        break;
                
                    case OR:
                    case OR_INVERT_B:
                        operator = BinaryOperator.OR;
                        break;

                    default:
                        throw new AssertionError();
                }
                
                Expression conditionA = jumpIfStmtA.getCondition();
                Expression conditionB = jumpIfStmtB.getCondition();
                if (logical.getLogicalType() == LogicalType.AND_INVERT_B 
                        || logical.getLogicalType() == LogicalType.OR_INVERT_B) {
                    conditionB = ExpressionInverter.invert(conditionB);
                }
                jumpIfStmtA.setCondition(new BinaryExpression(conditionA, conditionB, 
                                                              operator));
                break;
            }

            case IF_THEN_ELSE: {
                IfThenElseRegion ifThenElse = (IfThenElseRegion) region;
                buildAST(ifThenElse.getIfRegion(), controlTree, blockStmt);
                JumpIfStatement jumpIfStmt = (JumpIfStatement) blockStmt.getLast();
                jumpIfStmt.remove();
                BlockStatement thenBlockStmt = new BlockStatement();
                BlockStatement elseBlockStmt = new BlockStatement();
                buildAST(ifThenElse.getThenRegion(), controlTree, thenBlockStmt);
                buildAST(ifThenElse.getElseRegion(), controlTree, elseBlockStmt);
                // don't build if statement if then and else blocks are empty
                if (!isStmtBlockEmpty(thenBlockStmt) || !isStmtBlockEmpty(elseBlockStmt)) {
                    IfStatement ifStmt = new IfStatement(jumpIfStmt.getCondition(), 
                                                         thenBlockStmt, elseBlockStmt);
                    ifStmt.invert();
                    blockStmt.add(ifStmt);
                }
                break;
            }

            case IF_THEN: {
                IfThenRegion ifThen = (IfThenRegion) region;
                buildAST(ifThen.getIfRegion(), controlTree, blockStmt);
                JumpIfStatement jumpIfStmt = (JumpIfStatement) blockStmt.getLast();
                jumpIfStmt.remove();
                BlockStatement thenBlockStmt = new BlockStatement();
                buildAST(ifThen.getThenRegion(), controlTree, thenBlockStmt);
                // don't build if statement if then block is empty
                if (!isStmtBlockEmpty(thenBlockStmt)) {
                    Expression condition = ifThen.isInvertCondition() 
                            ? ExpressionInverter.invert(jumpIfStmt.getCondition())
                            : jumpIfStmt.getCondition();
                    IfStatement ifStmt = new IfStatement(condition, thenBlockStmt);
                    blockStmt.add(ifStmt);
                }
                break;
            }

            case LOOP: {
                LoopRegion loopRegion = (LoopRegion) region;
                BlockStatement bodyBlockStmt = new BlockStatement();

                List<LoopSubRegion> subRegions = new ArrayList<LoopSubRegion>(loopRegion.getSubRegions());

                Expression condition = null;

                if (loopRegion.getLoopType() == LoopType.WHILE) {
                    if (subRegions.size() > 0) {
                        buildAST(subRegions.get(0).getLoopRegion(), controlTree, bodyBlockStmt);
                        subRegions.remove(0);
                        JumpIfStatement jumpIfStmt = (JumpIfStatement) bodyBlockStmt.getLast();
                        jumpIfStmt.remove();
                        condition = ExpressionInverter.invert(jumpIfStmt.getCondition());
                    } else { // while(true) ...
                        condition = new Constant(Boolean.TRUE);
                    }
                }

                for (LoopSubRegion subRegion : subRegions) {
                    buildAST(subRegion.getLoopRegion(), controlTree, bodyBlockStmt);
                    JumpIfStatement jumpIfStmt = (JumpIfStatement) bodyBlockStmt.getLast();
                    jumpIfStmt.remove();
                    bodyBlockStmt.add(Statements.createIfThenBreakStmt(ExpressionInverter.invert(jumpIfStmt.getCondition())));
                }

                buildAST(loopRegion.getLoopTailRegion(), controlTree, bodyBlockStmt);

                switch (loopRegion.getLoopType()) {
                    case WHILE:
                        blockStmt.add(new WhileStatement(condition, bodyBlockStmt));
                        break;

                    case DO_WHILE: {
                        JumpIfStatement jumpIfStmt = (JumpIfStatement) bodyBlockStmt.getLast();
                        jumpIfStmt.remove();
                        DoWhileStatement doWhileStmt = new DoWhileStatement(bodyBlockStmt, jumpIfStmt.getCondition());
                        blockStmt.add(doWhileStmt);
                        break;
                    }

                    default:
                        throw new AssertionError();
                }

                break;
            }
        }
    }
}
