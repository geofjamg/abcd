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

import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.BreakStatement;
import fr.jamgotchian.abcd.core.ast.stmt.DoWhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.GotoStatement;
import fr.jamgotchian.abcd.core.ast.stmt.IfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.JumpIfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabelStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.stmt.LookupOrTableSwitchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ReturnStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement.CaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ThrowStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement.CatchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import fr.jamgotchian.abcd.core.ast.type.ClassName;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.type.JavaType;
import fr.jamgotchian.abcd.core.ast.util.ExpressionInverter;
import fr.jamgotchian.abcd.core.region.BlockRegion;
import fr.jamgotchian.abcd.core.region.CaseRegion;
import fr.jamgotchian.abcd.core.region.CatchRegion;
import fr.jamgotchian.abcd.core.region.IfThenElseRegion;
import fr.jamgotchian.abcd.core.region.IfThenRegion;
import fr.jamgotchian.abcd.core.region.BasicBlockRegion;
import fr.jamgotchian.abcd.core.region.IfThenBreakRegion;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.LoopRegion;
import fr.jamgotchian.abcd.core.region.LogicalRegion;
import fr.jamgotchian.abcd.core.region.LogicalType;
import fr.jamgotchian.abcd.core.region.SwitchCaseRegion;
import fr.jamgotchian.abcd.core.region.TryCatchRegion;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class AbstractSyntaxTreeBuilder {

    private static final Logger logger = Logger.getLogger(AbstractSyntaxTreeBuilder.class.getName());

    static {
        logger.setLevel(Level.FINER);
    }

    private final ImportManager importManager;

    public AbstractSyntaxTreeBuilder(ImportManager importManager) {
        this.importManager = importManager;
    }

    public void build(Region rootRegion, BlockStatement methodBody) {
        buildAST(rootRegion, methodBody);
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

    private void buildAST(Region region, BlockStatement blockStmt) {
        logger.log(Level.FINEST, "Build AST from region {0} {1}",
                new Object[] {region, region.getTypeName()});

        switch (region.getType()) {
            case BASIC_BLOCK: {
                BasicBlockRegion basicBlockRegion = (BasicBlockRegion) region;
                BasicBlockAnalysisDataImpl data = (BasicBlockAnalysisDataImpl) basicBlockRegion.getBasicBlock().getData();
                for (Statement stmt : data.getUsefullStatements()) {
                    blockStmt.add(stmt);
                }
                break;
            }

            case BLOCK: {
                for (Region child : ((BlockRegion) region).getRegions()) {
                    buildAST(child, blockStmt);
                }
                break;
            }

            case LOGICAL: {
                LogicalRegion logical = (LogicalRegion) region;

                buildAST(logical.getRegionA(), blockStmt);
                JumpIfStatement jumpIfStmtA = (JumpIfStatement) blockStmt.getLast();

                BlockStatement tmpBlockStmt = new BlockStatement();
                buildAST(logical.getRegionB(), tmpBlockStmt);
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
                jumpIfStmtA.setCondition(Expressions.newBinExpr(conditionA, conditionB,
                                                                operator, conditionA.getBasicBlock()));
                break;
            }

            case IF_THEN_ELSE: {
                IfThenElseRegion ifThenElse = (IfThenElseRegion) region;
                buildAST(ifThenElse.getIfRegion(), blockStmt);
                JumpIfStatement jumpIfStmt = (JumpIfStatement) blockStmt.getLast();
                jumpIfStmt.remove();
                BlockStatement thenBlockStmt = new BlockStatement();
                BlockStatement elseBlockStmt = new BlockStatement();
                buildAST(ifThenElse.getThenRegion(), thenBlockStmt);
                buildAST(ifThenElse.getElseRegion(), elseBlockStmt);
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
                buildAST(ifThen.getIfRegion(), blockStmt);
                JumpIfStatement jumpIfStmt = (JumpIfStatement) blockStmt.getLast();
                jumpIfStmt.remove();
                BlockStatement thenBlockStmt = new BlockStatement();
                buildAST(ifThen.getThenRegion(), thenBlockStmt);
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

            case IF_THEN_BREAK: {
                IfThenBreakRegion ifBreak = (IfThenBreakRegion) region;
                buildAST(ifBreak.getIfRegion(), blockStmt);
                JumpIfStatement jumpIfStmt = (JumpIfStatement) blockStmt.getLast();
                jumpIfStmt.remove();
                BlockStatement thenBlockStmt = new BlockStatement();
                if (ifBreak.getBeforeThenRegion() != null) {
                    buildAST(ifBreak.getBeforeThenRegion(), thenBlockStmt);
                }
                if (ifBreak.getAfterThenRegion() != null) {
                    buildAST(ifBreak.getAfterThenRegion(), thenBlockStmt);
                }
                Statement lastStmt = thenBlockStmt.getLast();
                if (!(lastStmt instanceof ReturnStatement)
                        && !(lastStmt instanceof ThrowStatement)) {
//                    thenBlockStmt.add(new CommentStatement("Loop ID : "
//                            + ifBreak.getBreakTargetRegion().getBreakLoopID()));
                    thenBlockStmt.add(new BreakStatement());
                }
                Expression condition = jumpIfStmt.getCondition();
                if (ifBreak.isInvertCond()) {
                    condition = ExpressionInverter.invert(condition);
                }
                IfStatement ifStmt = new IfStatement(condition, thenBlockStmt);
                blockStmt.add(ifStmt);
                break;
            }

            case LOOP: {
                LoopRegion loopRegion = (LoopRegion) region;
                BlockStatement bodyBlockStmt = new BlockStatement();

                buildAST(loopRegion.getLoopRegion(), bodyBlockStmt);

//                blockStmt.add(new CommentStatement("Loop ID : " + loopRegion.getLoopID()));
                switch (loopRegion.getLoopType()) {
                    case WHILE: {
                        IfStatement ifStmt = (IfStatement) bodyBlockStmt.getFirst();
                        ifStmt.remove();
                        Expression condition = ExpressionInverter.invert(ifStmt.getCondition());
                        blockStmt.add(new WhileStatement(condition, bodyBlockStmt));
                        break;
                    }

                    case DO_WHILE:
                        IfStatement ifStmt = (IfStatement) bodyBlockStmt.getLast();
                        ifStmt.remove();
                        Expression condition = ExpressionInverter.invert(ifStmt.getCondition());
                        blockStmt.add(new DoWhileStatement(bodyBlockStmt, condition));
                        break;

                    case INFINITE:
                        blockStmt.add(new WhileStatement(Expressions.newBooleanExpr(true, null), bodyBlockStmt));
                        break;

                    default:
                        throw new AssertionError();
                }
                break;
            }

            case SWITCH_CASE: {
                SwitchCaseRegion switchCase = (SwitchCaseRegion) region;

                buildAST(switchCase.getSwitchRegion(), blockStmt);
                LookupOrTableSwitchStatement lookupOrTableSwitchStmt
                        = (LookupOrTableSwitchStatement) blockStmt.getLast();
                lookupOrTableSwitchStmt.remove();

                List<CaseStatement> cases = new ArrayList<CaseStatement>();
                for (CaseRegion caseRegion : switchCase.getCaseRegions()) {
                    List<Statement> caseStmts = new ArrayList<Statement>();

                    if (caseRegion.getRegion() != null) {
                        BlockStatement caseCompoundStmt = new BlockStatement();
                        buildAST(caseRegion.getRegion(), caseCompoundStmt);
                        for (Statement stmt : caseCompoundStmt) {
                            caseStmts.add(stmt);
                        }
                        caseCompoundStmt.clear();
                        if (!(caseStmts.get(caseStmts.size()-1) instanceof ReturnStatement)) {
                            caseStmts.add(new BreakStatement());
                        }
                    } else {
                        caseStmts.add(new BreakStatement());
                    }

                    cases.add(new CaseStatement(caseRegion.getValues(), caseStmts));
                }
                blockStmt.add(new SwitchCaseStatement(lookupOrTableSwitchStmt.getCondition(), cases));

                break;
            }

            case TRY_CATCH: {
                TryCatchRegion tryCatchRegion = (TryCatchRegion) region;

                BlockStatement tryBlockStmt = new BlockStatement();
                buildAST(tryCatchRegion.getTryRegion1(), tryBlockStmt);

                if (tryCatchRegion.getTryRegion2() != null) {
                    buildAST(tryCatchRegion.getTryRegion2(), tryBlockStmt);
                }

                List<CatchStatement> catchStmts = new ArrayList<CatchStatement>();
                for (CatchRegion catchRegion : tryCatchRegion.getCatchRegions()) {

                    BlockStatement catchBlockStmt = new BlockStatement();
                    buildAST(catchRegion.getRegion(), catchBlockStmt);

                    ExpressionStatement exprStmt = (ExpressionStatement) catchBlockStmt.getFirst();
                    exprStmt.remove();
                    LocalVariable excVar = (LocalVariable) ((AssignExpression) exprStmt.getExpression()).getTarget();

                    ClassName exceptionClassName = importManager.newClassName(catchRegion.getExceptionClassName());
                    JavaType exceptionType = JavaType.newRefType(exceptionClassName);
                    LocalVariableDeclaration varDecl
                            = new LocalVariableDeclaration(excVar.getIndex(), exceptionType);
                    catchStmts.add(new CatchStatement(catchBlockStmt, varDecl));
                }
                TryCatchFinallyStatement tryCatchStmt
                        = new TryCatchFinallyStatement(tryBlockStmt, catchStmts);
                blockStmt.add(tryCatchStmt);

                break;
            }
        }
    }
}
