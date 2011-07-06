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

import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.AssignOperator;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.expr.TypeExpression;
import fr.jamgotchian.abcd.core.ast.expr.UnaryOperator;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.BreakStatement;
import fr.jamgotchian.abcd.core.ast.stmt.DoWhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.IfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.stmt.MonitorEnterStatement;
import fr.jamgotchian.abcd.core.ast.stmt.MonitorExitStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ReturnStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.SwitchCaseStatement.CaseStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ThrowStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement;
import fr.jamgotchian.abcd.core.ast.stmt.TryCatchFinallyStatement.CatchClause;
import fr.jamgotchian.abcd.core.ast.stmt.WhileStatement;
import fr.jamgotchian.abcd.core.ast.util.ExpressionInverter;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.region.BasicBlockRegion;
import fr.jamgotchian.abcd.core.region.BlockRegion;
import fr.jamgotchian.abcd.core.region.CaseRegion;
import fr.jamgotchian.abcd.core.region.CatchRegion;
import fr.jamgotchian.abcd.core.region.IfThenBreakRegion;
import fr.jamgotchian.abcd.core.region.IfThenElseRegion;
import fr.jamgotchian.abcd.core.region.IfThenRegion;
import fr.jamgotchian.abcd.core.region.LogicalRegion;
import fr.jamgotchian.abcd.core.region.LogicalType;
import fr.jamgotchian.abcd.core.region.LoopRegion;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.RegionType;
import fr.jamgotchian.abcd.core.region.SwitchCaseRegion;
import fr.jamgotchian.abcd.core.region.TryCatchRegion;
import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.AssignConstInst;
import fr.jamgotchian.abcd.core.tac.model.AssignVarInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryInst;
import fr.jamgotchian.abcd.core.tac.model.ByteConst;
import fr.jamgotchian.abcd.core.tac.model.CallMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.ClassConst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.Const;
import fr.jamgotchian.abcd.core.tac.model.DoubleConst;
import fr.jamgotchian.abcd.core.tac.model.FloatConst;
import fr.jamgotchian.abcd.core.tac.model.GetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.GetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GotoInst;
import fr.jamgotchian.abcd.core.tac.model.InstanceOfInst;
import fr.jamgotchian.abcd.core.tac.model.IntConst;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.LabelInst;
import fr.jamgotchian.abcd.core.tac.model.LongConst;
import fr.jamgotchian.abcd.core.tac.model.MonitorEnterInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorExitInst;
import fr.jamgotchian.abcd.core.tac.model.NewArrayInst;
import fr.jamgotchian.abcd.core.tac.model.NewObjectInst;
import fr.jamgotchian.abcd.core.tac.model.NullConst;
import fr.jamgotchian.abcd.core.tac.model.PhiInst;
import fr.jamgotchian.abcd.core.tac.model.ReturnInst;
import fr.jamgotchian.abcd.core.tac.model.SetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.SetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.ShortConst;
import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.SwitchInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.tac.model.ThrowInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.model.VariableID;
import fr.jamgotchian.abcd.core.tac.util.EmptyTACInstVisitor;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

    private Map<BasicBlock, Set<Variable>> liveVariables;

    private final Map<VariableID, Expression> expressions;

    private class RegionTACInstVisitor extends EmptyTACInstVisitor<Void, BlockStatement> {

        private final Region region;

        private RegionTACInstVisitor(Region region) {
            this.region = region;
        }

        private Expression getVarExpr(Variable var) {
            if (var.isTemporary()) {
                return expressions.get(var.getID());
            } else {
                return Expressions.newVarExpr(var.getIndex(),
                                              var.getVersion(),
                                              var.getName(),
                                              var.getBasicBlock());
            }
        }

        private void addVarAssignExpr(Variable leftVar, Expression rightExpr,
                                      BlockStatement blockStmt) {
            if (leftVar.isTemporary()) {
                expressions.put(leftVar.getID(), rightExpr);
            } else {
                BasicBlock bb = leftVar.getBasicBlock();
                Expression varExpr
                        = Expressions.newVarExpr(leftVar.getIndex(),
                                                 leftVar.getVersion(),
                                                 leftVar.getName(),
                                                 bb);
                Expression assignExpr
                        = Expressions.newAssignExpr(varExpr, rightExpr,
                                                    AssignOperator.ASSIGN, bb);
                blockStmt.add(new ExpressionStatement(assignExpr));
            }
        }

        @Override
        public Void visit(ArrayLengthInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable arrayVar = inst.getArray();
            BasicBlock bb = resultVar.getBasicBlock();
            Expression arrayExpr = getVarExpr(arrayVar);
            Expression lengthExpr = Expressions.newArrayLength(arrayExpr, bb);
            addVarAssignExpr(resultVar, lengthExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(AssignConstInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Const _const = inst.getConst();
            Expression constExpr = null;
            if (_const instanceof IntConst) {
                constExpr = Expressions.newIntExpr(((IntConst) _const).getValue(), bb);
            } else if (_const instanceof LongConst) {
                constExpr = Expressions.newLongExpr(((LongConst) _const).getValue(), bb);
            } else if (_const instanceof ByteConst) {
                constExpr = Expressions.newByteExpr(((ByteConst) _const).getValue(), bb);
            } else if (_const instanceof FloatConst) {
                constExpr = Expressions.newFloatExpr(((FloatConst) _const).getValue(), bb);
            } else if (_const instanceof DoubleConst) {
                constExpr = Expressions.newDoubleExpr(((DoubleConst) _const).getValue(), bb);
            } else if (_const instanceof ShortConst) {
                constExpr = Expressions.newShortExpr(((ShortConst) _const).getValue(), bb);
            } else if (_const instanceof StringConst) {
                constExpr = Expressions.newStringExpr(((StringConst) _const).getValue(), bb);
            } else if (_const instanceof ClassConst) {
                constExpr = Expressions.newClsExpr(((ClassConst) _const).getClassName(), bb);
            } else if (_const instanceof NullConst) {
                constExpr = Expressions.newNullExpr(bb);
            } else {
                throw new InternalError();
            }
            addVarAssignExpr(resultVar, constExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(AssignVarInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable valueVar = inst.getValue();
            Expression valueExpr = getVarExpr(valueVar);
            addVarAssignExpr(resultVar, valueExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(BinaryInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable leftVar = inst.getLeft();
            Variable rightVar = inst.getRight();
            BasicBlock bb = resultVar.getBasicBlock();
            Expression leftExpr = getVarExpr(leftVar);
            Expression rightExpr = getVarExpr(rightVar);
            BinaryOperator op;
            switch (inst.getOperator()) {
                case AND: op = BinaryOperator.AND; break;
                case DIV: op = BinaryOperator.DIV; break;
                case EQ: op = BinaryOperator.EQ; break;
                case GE: op = BinaryOperator.GE; break;
                case GT: op = BinaryOperator.GT; break;
                case LE: op = BinaryOperator.LE; break;
                case LOGICAL_SHIFT_RIGHT: op = BinaryOperator.LOGICAL_SHIFT_RIGHT; break;
                case LT: op = BinaryOperator.LT; break;
                case MINUS: op = BinaryOperator.MINUS; break;
                case MUL: op = BinaryOperator.MUL; break;
                case NE: op = BinaryOperator.NE; break;
                case OR: op = BinaryOperator.OR; break;
                case PLUS: op = BinaryOperator.PLUS; break;
                case REMAINDER: op = BinaryOperator.REMAINDER; break;
                case SHIFT_LEFT: op = BinaryOperator.SHIFT_LEFT; break;
                case SHIFT_RIGHT: op = BinaryOperator.SHIFT_RIGHT; break;
                case XOR: op = BinaryOperator.XOR; break;
                default: throw new InternalError();
            }
            Expression binExpr = Expressions.newBinExpr(leftExpr, rightExpr, op, bb);
            addVarAssignExpr(resultVar, binExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(CallMethodInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable objVar = inst.getObject();
            Expression objExpr = getVarExpr(objVar);
            List<Expression> argsExpr = new ArrayList<Expression>(inst.getArgumentCount());
            for (Variable argVar : inst.getArguments()) {
                argsExpr.add(getVarExpr(argVar));
            }
            Expression callExpr
                    = Expressions.newMethodExpr(objExpr, inst.getSignature().getMethodName(),
                                                argsExpr, bb);
            if (liveVariables.get(bb).contains(resultVar)) {
                expressions.put(resultVar.getID(), callExpr);
            } else {
                blockStmt.add(new ExpressionStatement(callExpr));
            }
            return null;
        }

        @Override
        public Void visit(CallStaticMethodInst inst, BlockStatement blockStmt) {
            return super.visit(inst, blockStmt);
        }

        @Override
        public Void visit(CastInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable var = inst.getVar();
            Expression expr = getVarExpr(var);
            Expression castExpr = Expressions.newCastExpr(inst.getCastType(), expr, bb);
            addVarAssignExpr(resultVar, castExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(ConditionalInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable condVar = inst.getCond();
            Variable thenVar = inst.getThen();
            Variable elseVar = inst.getElse();
            Expression condExpr = ExpressionInverter.invert(getVarExpr(condVar));
            Expression thenExpr = getVarExpr(thenVar);
            Expression elseExpr = getVarExpr(elseVar);
            Expression ternExpr = Expressions.newCondExpr(condExpr, elseExpr, thenExpr, bb);
            addVarAssignExpr(resultVar, ternExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(GetArrayInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable arrayVar = inst.getArray();
            Variable indexVar = inst.getIndex();
            Expression arrayExpr = getVarExpr(arrayVar);
            Expression indexExpr = getVarExpr(indexVar);
            Expression accessExpr = Expressions.newArrayAccess(arrayExpr, indexExpr, bb);
            addVarAssignExpr(resultVar, accessExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(SetArrayInst inst, BlockStatement blockStmt) {
            Variable arrayVar = inst.getArray();
            Variable indexVar = inst.getIndex();
            Variable valueVar = inst.getValue();
            BasicBlock bb = valueVar.getBasicBlock();
            Expression arrayExpr = getVarExpr(arrayVar);
            Expression indexExpr = getVarExpr(indexVar);
            Expression valueExpr = getVarExpr(valueVar);
            Expression accessExpr = Expressions.newArrayAccess(arrayExpr, indexExpr, bb);
            Expression assignExpr
                        = Expressions.newAssignExpr(accessExpr, valueExpr,
                                                    AssignOperator.ASSIGN, bb);
            blockStmt.add(new ExpressionStatement(assignExpr));
            return null;
        }

        @Override
        public Void visit(GetFieldInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable objVar = inst.getObject();
            Expression objExpr = getVarExpr(objVar);
            Expression fieldExpr = Expressions.newFieldAccesExpr(objExpr, inst.getFieldName(), bb);
            addVarAssignExpr(resultVar, fieldExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(SetFieldInst inst, BlockStatement blockStmt) {
            Variable valueVar = inst.getValue();
            BasicBlock bb = valueVar.getBasicBlock();
            Expression valueExpr = getVarExpr(valueVar);
            Variable objVar = inst.getObject();
            Expression objExpr = getVarExpr(objVar);
            Expression fieldExpr = Expressions.newFieldAccesExpr(objExpr, inst.getFieldName(), bb);
            Expression assignExpr
                    = Expressions.newAssignExpr(fieldExpr, valueExpr,
                                                AssignOperator.ASSIGN, bb);
            blockStmt.add(new ExpressionStatement(assignExpr));
            return null;
        }

        @Override
        public Void visit(GotoInst inst, BlockStatement blockStmt) {
            // nothing to do
            return null;
        }

        @Override
        public Void visit(JumpIfInst inst, BlockStatement blockStmt) {
            Variable condVar = inst.getCond();
            Expression condExpr = getVarExpr(condVar);
            IfStatement ifStmt;
            Region parent = region.getAncestor(EnumSet.of(RegionType.IF_THEN,
                                                          RegionType.IF_THEN_ELSE,
                                                          RegionType.IF_THEN_BREAK));
            if (parent.getType() == RegionType.IF_THEN) {
                Expression condExpr2
                        = ((IfThenRegion) parent).shouldInvertCondition()
                        ? ExpressionInverter.invert(condExpr)
                        : condExpr;
                ifStmt = new IfStatement(condExpr2, new BlockStatement());
            } else if (parent.getType() == RegionType.IF_THEN_BREAK) {
                ifStmt = new IfStatement(condExpr, new BlockStatement());
            } else if (parent.getType() == RegionType.IF_THEN_ELSE) {
                ifStmt = new IfStatement(condExpr, new BlockStatement(), new BlockStatement());
            } else {
                throw new InternalError();
            }
            blockStmt.add(ifStmt);
            return null;
        }

        @Override
        public Void visit(LabelInst inst, BlockStatement blockStmt) {
            // nothing to do
            return null;
        }

        @Override
        public Void visit(InstanceOfInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable var = inst.getVar();
            Expression expr = getVarExpr(var);
            Expression typeExpr = Expressions.newTypeExpr(inst.getType(), bb);
            Expression instOfExpr = Expressions.newBinExpr(expr, typeExpr, BinaryOperator.INSTANCE_OF, bb);
            addVarAssignExpr(resultVar, instOfExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(MonitorEnterInst inst, BlockStatement blockStmt) {
            Variable objVar = inst.getObj();
            BasicBlock bb = objVar.getBasicBlock();
            Expression objExpr = getVarExpr(objVar);
            blockStmt.add(new MonitorEnterStatement(objExpr));
            return null;
        }

        @Override
        public Void visit(MonitorExitInst inst, BlockStatement blockStmt) {
            Variable objVar = inst.getObj();
            BasicBlock bb = objVar.getBasicBlock();
            Expression objExpr = getVarExpr(objVar);
            blockStmt.add(new MonitorExitStatement(objExpr));
            return null;
        }

        @Override
        public Void visit(NewArrayInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            List<Expression> dimExprs = new ArrayList<Expression>(inst.getDimensionCount());
            for (Variable dimVar : inst.getDimensions()) {
                dimExprs.add(getVarExpr(dimVar));
            }
            Expression newArrExpr = Expressions.newArrayCreatExpr(inst.getType(), dimExprs, bb);
            addVarAssignExpr(resultVar, newArrExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(NewObjectInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            List<Expression> argExprs = new ArrayList<Expression>(inst.getArgumentCount());
            for (Variable argVar : inst.getArguments()) {
                argExprs.add(getVarExpr(argVar));
            }
            Expression newObjExpr = Expressions.newObjCreatExpr(inst.getType(), argExprs, bb);
            addVarAssignExpr(resultVar, newObjExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(ReturnInst inst, BlockStatement blockStmt) {
            Variable var = inst.getVar();
            Statement retStmt;
            if (var == null) {
                retStmt = new ReturnStatement();
            } else {
                retStmt = new ReturnStatement(getVarExpr(var));
            }
            blockStmt.add(retStmt);
            return null;
        }

        @Override
        public Void visit(SwitchInst inst, BlockStatement blockStmt) {
            Variable condVar = inst.getCond();
            Expression condExpr = getVarExpr(condVar);
            SwitchCaseStatement switchStmt = new SwitchCaseStatement(condExpr);
            blockStmt.add(switchStmt);
            return null;
        }

        @Override
        public Void visit(ThrowInst inst, BlockStatement blockStmt) {
            Variable var = inst.getVar();
            blockStmt.add(new ThrowStatement(getVarExpr(var)));
            return null;
        }

        @Override
        public Void visit(UnaryInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            Variable var = inst.getVar();
            Expression expr = getVarExpr(var);
            UnaryOperator op;
            switch (inst.getOperator()) {
                case MINUS:
                    op = UnaryOperator.MINUS;
                    break;
                default:
                    throw new InternalError();
            }
            Expression unaryExpr = Expressions.newUnaryExpr(expr, op, bb);
            addVarAssignExpr(resultVar, unaryExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(ChoiceInst inst, BlockStatement blockStmt) {
            // TODO
            return null;
        }

        @Override
        public Void visit(PhiInst inst, BlockStatement blockStmt) {
            throw new InternalError();
        }

        @Override
        public Void visit(GetStaticFieldInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            JavaType type = JavaType.newRefType(inst.getScope());
            TypeExpression typeExpr = Expressions.newTypeExpr(type, bb);
            Expression fieldExpr = Expressions.newFieldAccesExpr(typeExpr, inst.getFieldName(), bb);
            addVarAssignExpr(resultVar, fieldExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(SetStaticFieldInst inst, BlockStatement blockStmt) {
            Variable valueVar = inst.getValue();
            BasicBlock bb = valueVar.getBasicBlock();
            Expression valueExpr = getVarExpr(valueVar);
            JavaType type = JavaType.newRefType(inst.getScope());
            TypeExpression typeExpr = Expressions.newTypeExpr(type, bb);
            Expression fieldExpr = Expressions.newFieldAccesExpr(typeExpr, inst.getFieldName(), bb);
            Expression assignExpr
                    = Expressions.newAssignExpr(fieldExpr, valueExpr,
                                                AssignOperator.ASSIGN, bb);
            blockStmt.add(new ExpressionStatement(assignExpr));
            return null;
        }
    }

    public AbstractSyntaxTreeBuilder(ImportManager importManager) {
        this.importManager = importManager;
        expressions = new HashMap<VariableID, Expression>();
    }

    public void build(Region rootRegion, BlockStatement methodBody,
                      Map<BasicBlock, Set<Variable>> liveVariables) {
        this.liveVariables = liveVariables;
        buildAST(rootRegion, null, methodBody);
    }

    private void buildAST(Region region, Region parent, BlockStatement blockStmt) {
        logger.log(Level.FINEST, "Build AST from region {0} {1}",
                new Object[] {region, region.getTypeName()});

        switch (region.getType()) {
            case BASIC_BLOCK: {
                BasicBlockRegion basicBlockRegion = (BasicBlockRegion) region;
                AnalysisData data = (AnalysisData) basicBlockRegion.getBasicBlock().getData();
                RegionTACInstVisitor visitor = new RegionTACInstVisitor(region);
                new TACInstSeq(data.getInstructions()).accept(visitor, blockStmt);
                break;
            }

            case BLOCK: {
                for (Region child : ((BlockRegion) region).getRegions()) {
                    buildAST(child, region, blockStmt);
                }
                break;
            }

            case LOGICAL: {
                LogicalRegion logical = (LogicalRegion) region;

                buildAST(logical.getRegionA(), logical, blockStmt);
                IfStatement ifStmtA = (IfStatement) blockStmt.getLast();
                ifStmtA.remove();

                BlockStatement tmpBlockStmt = new BlockStatement();
                buildAST(logical.getRegionB(), logical, tmpBlockStmt);
                IfStatement ifStmtB = (IfStatement) tmpBlockStmt.getLast();

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

                Expression conditionA = ifStmtA.getCondition();
                Expression conditionB = ifStmtB.getCondition();
                if (logical.getLogicalType() == LogicalType.AND_INVERT_B
                        || logical.getLogicalType() == LogicalType.OR_INVERT_B) {
                    conditionB = ExpressionInverter.invert(conditionB);
                }
                Expression condition
                        = Expressions.newBinExpr(conditionA, conditionB,
                                                 operator, conditionA.getBasicBlock());
                blockStmt.add(new IfStatement(condition, ifStmtA.getThen(), ifStmtA.getElse()));
                break;
            }

            case IF_THEN_ELSE: {
                IfThenElseRegion ifThenElse = (IfThenElseRegion) region;
                buildAST(ifThenElse.getIfRegion(), region, blockStmt);
                IfStatement ifStmt = (IfStatement) blockStmt.getLast();
                buildAST(ifThenElse.getThenRegion(), region, ifStmt.getThen());
                buildAST(ifThenElse.getElseRegion(), region, ifStmt.getElse());
                if (ifStmt.getThen().isEmpty() && ifStmt.getElse().isEmpty()) {
                    ifStmt.remove();
                } else {
                    ifStmt.invert();
                }
                break;
            }

            case IF_THEN: {
                IfThenRegion ifThen = (IfThenRegion) region;
                buildAST(ifThen.getIfRegion(), region, blockStmt);
                IfStatement ifStmt = (IfStatement) blockStmt.getLast();
                buildAST(ifThen.getThenRegion(), region, ifStmt.getThen());
                if (ifStmt.getThen().isEmpty()) {
                    ifStmt.remove();
                }
                break;
            }

            case IF_THEN_BREAK: {
                IfThenBreakRegion ifBreak = (IfThenBreakRegion) region;
                buildAST(ifBreak.getIfRegion(), ifBreak, blockStmt);
                IfStatement ifStmt = (IfStatement) blockStmt.getLast();
                ifStmt.remove();
                BlockStatement thenBlockStmt = new BlockStatement();
                if (ifBreak.getBeforeThenRegion() != null) {
                    buildAST(ifBreak.getBeforeThenRegion(), ifBreak, thenBlockStmt);
                }
                if (ifBreak.getAfterThenRegion() != null) {
                    buildAST(ifBreak.getAfterThenRegion(), ifBreak, thenBlockStmt);
                }
                Statement lastStmt = thenBlockStmt.getLast();
                if (!(lastStmt instanceof ReturnStatement)
                        && !(lastStmt instanceof ThrowStatement)) {
//                    thenBlockStmt.add(new CommentStatement("Loop ID : "
//                            + ifBreak.getBreakTargetRegion().getBreakLoopID()));
                    thenBlockStmt.add(new BreakStatement());
                }
                Expression condition = ifStmt.getCondition();
                if (ifBreak.isInvertCond()) {
                    condition = ExpressionInverter.invert(condition);
                }
                blockStmt.add(new IfStatement(condition, thenBlockStmt));
                break;
            }

            case LOOP: {
                LoopRegion loopRegion = (LoopRegion) region;
                BlockStatement bodyBlockStmt = new BlockStatement();

                buildAST(loopRegion.getLoopRegion(), loopRegion, bodyBlockStmt);

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

                buildAST(switchCase.getSwitchRegion(), switchCase, blockStmt);
                SwitchCaseStatement switchStmt
                        = (SwitchCaseStatement) blockStmt.getLast();
                switchStmt.remove();

                List<CaseStatement> cases = new ArrayList<CaseStatement>();
                for (CaseRegion caseRegion : switchCase.getCaseRegions()) {
                    List<Statement> caseStmts = new ArrayList<Statement>();

                    if (caseRegion.getRegion() != null) {
                        BlockStatement caseCompoundStmt = new BlockStatement();
                        buildAST(caseRegion.getRegion(), switchCase, caseCompoundStmt);
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
                blockStmt.add(new SwitchCaseStatement(switchStmt.getCondition(), cases));

                break;
            }

            case TRY_CATCH: {
                TryCatchRegion tryCatchRegion = (TryCatchRegion) region;

                BlockStatement tryBlockStmt = new BlockStatement();
                buildAST(tryCatchRegion.getTryRegion1(), tryCatchRegion, tryBlockStmt);

                if (tryCatchRegion.getTryRegion2() != null) {
                    buildAST(tryCatchRegion.getTryRegion2(), tryCatchRegion, tryBlockStmt);
                }

                List<CatchClause> catchs = new ArrayList<CatchClause>();
                for (CatchRegion catchRegion : tryCatchRegion.getCatchRegions()) {

                    BlockStatement catchBlockStmt = new BlockStatement();
                    buildAST(catchRegion.getRegion(), tryCatchRegion, catchBlockStmt);

                    ExpressionStatement exprStmt = (ExpressionStatement) catchBlockStmt.getFirst();
                    exprStmt.remove();
                    LocalVariable excVar = (LocalVariable) ((AssignExpression) exprStmt.getExpression()).getTarget();

                    ClassName exceptionClassName = importManager.newClassName(catchRegion.getExceptionClassName());
                    JavaType exceptionType = JavaType.newRefType(exceptionClassName);
                    LocalVariableDeclaration varDecl
                            = new LocalVariableDeclaration(excVar, exceptionType);
                    catchs.add(new CatchClause(catchBlockStmt, varDecl));
                }
                TryCatchFinallyStatement tryCatchStmt
                        = new TryCatchFinallyStatement(tryBlockStmt, catchs);
                blockStmt.add(tryCatchStmt);

                break;
            }
        }
    }
}
