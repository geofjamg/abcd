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
import fr.jamgotchian.abcd.core.ast.expr.ArrayCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.AssignOperator;
import fr.jamgotchian.abcd.core.ast.expr.ASTBinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.ObjectCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.TypeExpression;
import fr.jamgotchian.abcd.core.ast.expr.ASTUnaryOperator;
import fr.jamgotchian.abcd.core.ast.stmt.BlockStatement;
import fr.jamgotchian.abcd.core.ast.stmt.BreakStatement;
import fr.jamgotchian.abcd.core.ast.stmt.DoWhileStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.IfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabeledStatement;
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
import fr.jamgotchian.abcd.core.controlflow.TACInstSeq;
import fr.jamgotchian.abcd.core.region.BasicBlockRegion;
import fr.jamgotchian.abcd.core.region.BlockRegion;
import fr.jamgotchian.abcd.core.region.CaseRegion;
import fr.jamgotchian.abcd.core.region.CatchRegion;
import fr.jamgotchian.abcd.core.region.IfThenRegion;
import fr.jamgotchian.abcd.core.region.InfiniteLoopRegion;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.SwitchCaseRegion;
import fr.jamgotchian.abcd.core.region.TryCatchFinallyRegion;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.ArrayLengthInst;
import fr.jamgotchian.abcd.core.controlflow.AssignConstInst;
import fr.jamgotchian.abcd.core.controlflow.AssignVarInst;
import fr.jamgotchian.abcd.core.controlflow.BinaryInst;
import fr.jamgotchian.abcd.core.controlflow.ByteConst;
import fr.jamgotchian.abcd.core.controlflow.CallMethodInst;
import fr.jamgotchian.abcd.core.controlflow.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.controlflow.CastInst;
import fr.jamgotchian.abcd.core.controlflow.ChoiceInst;
import fr.jamgotchian.abcd.core.controlflow.ClassConst;
import fr.jamgotchian.abcd.core.controlflow.ConditionalInst;
import fr.jamgotchian.abcd.core.controlflow.Const;
import fr.jamgotchian.abcd.core.controlflow.DoubleConst;
import fr.jamgotchian.abcd.core.controlflow.ExceptionHandlerInfo;
import fr.jamgotchian.abcd.core.controlflow.FloatConst;
import fr.jamgotchian.abcd.core.controlflow.GetArrayInst;
import fr.jamgotchian.abcd.core.controlflow.GetFieldInst;
import fr.jamgotchian.abcd.core.controlflow.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.controlflow.InstanceOfInst;
import fr.jamgotchian.abcd.core.controlflow.IntConst;
import fr.jamgotchian.abcd.core.controlflow.JumpIfInst;
import fr.jamgotchian.abcd.core.controlflow.LongConst;
import fr.jamgotchian.abcd.core.controlflow.MonitorEnterInst;
import fr.jamgotchian.abcd.core.controlflow.MonitorExitInst;
import fr.jamgotchian.abcd.core.controlflow.NewArrayInst;
import fr.jamgotchian.abcd.core.controlflow.NewObjectInst;
import fr.jamgotchian.abcd.core.controlflow.NullConst;
import fr.jamgotchian.abcd.core.controlflow.PhiInst;
import fr.jamgotchian.abcd.core.controlflow.ReturnInst;
import fr.jamgotchian.abcd.core.controlflow.SetArrayInst;
import fr.jamgotchian.abcd.core.controlflow.SetFieldInst;
import fr.jamgotchian.abcd.core.controlflow.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.controlflow.ShortConst;
import fr.jamgotchian.abcd.core.controlflow.StringConst;
import fr.jamgotchian.abcd.core.controlflow.SwitchInst;
import fr.jamgotchian.abcd.core.controlflow.TACInst;
import fr.jamgotchian.abcd.core.controlflow.ThrowInst;
import fr.jamgotchian.abcd.core.controlflow.UnaryInst;
import fr.jamgotchian.abcd.core.controlflow.Variable;
import fr.jamgotchian.abcd.core.controlflow.VariableID;
import fr.jamgotchian.abcd.core.controlflow.util.EmptyTACInstVisitor;
import fr.jamgotchian.abcd.core.region.DoWhileLoopRegion;
import fr.jamgotchian.abcd.core.region.IfThenElseRegion;
import fr.jamgotchian.abcd.core.region.InlinedFinallyBreakRegion;
import fr.jamgotchian.abcd.core.region.LabeledRegion;
import fr.jamgotchian.abcd.core.region.RegionType;
import fr.jamgotchian.abcd.core.region.WhileLoopRegion;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.ArrayList;
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

    private final ControlFlowGraph CFG;

    private final ImportManager importManager;

    private final Region rootRegion;

    private final BlockStatement methodBody;

    private Map<BasicBlock, Set<Variable>> liveVariables;

    private final Map<VariableID, Expression> expressions;

    private class RegionTACInstVisitor extends EmptyTACInstVisitor<Void, BlockStatement> {

        private RegionTACInstVisitor() {
        }

        private Expression getVarExpr(Variable var) {
            if (var.isTemporary()) {
                return expressions.get(var.getID());
            } else {
                return Expressions.newVarExpr(var.getID(),
                                              var.getName());
            }
        }

        private void addVarAssignExpr(Variable leftVar, Expression rightExpr,
                                      BlockStatement blockStmt) {
            if (leftVar.isTemporary()) {
                expressions.put(leftVar.getID(), rightExpr);
            } else {
                Expression varExpr
                        = Expressions.newVarExpr(leftVar.getID(),
                                                 leftVar.getName());
                Expression assignExpr
                        = Expressions.newAssignExpr(varExpr, rightExpr,
                                                    AssignOperator.ASSIGN);
                blockStmt.add(new ExpressionStatement(assignExpr));
            }
        }

        @Override
        public Void visit(TACInstSeq seq, BlockStatement arg) {
            for (TACInst inst : seq) {
                if (!inst.isIgnored()) {
                    inst.accept(this, arg);
                }
            }
            return null;
        }

        @Override
        public Void visit(ArrayLengthInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable arrayVar = inst.getArray();
            Expression arrayExpr = getVarExpr(arrayVar);
            Expression lengthExpr = Expressions.newArrayLength(arrayExpr);
            addVarAssignExpr(resultVar, lengthExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(AssignConstInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Const _const = inst.getConst();
            Expression constExpr = null;
            if (_const instanceof IntConst) {
                constExpr = Expressions.newIntExpr(((IntConst) _const).getValue());
            } else if (_const instanceof LongConst) {
                constExpr = Expressions.newLongExpr(((LongConst) _const).getValue());
            } else if (_const instanceof ByteConst) {
                constExpr = Expressions.newByteExpr(((ByteConst) _const).getValue());
            } else if (_const instanceof FloatConst) {
                constExpr = Expressions.newFloatExpr(((FloatConst) _const).getValue());
            } else if (_const instanceof DoubleConst) {
                constExpr = Expressions.newDoubleExpr(((DoubleConst) _const).getValue());
            } else if (_const instanceof ShortConst) {
                constExpr = Expressions.newShortExpr(((ShortConst) _const).getValue());
            } else if (_const instanceof StringConst) {
                constExpr = Expressions.newStringExpr(((StringConst) _const).getValue());
            } else if (_const instanceof ClassConst) {
                constExpr = Expressions.newClsExpr(((ClassConst) _const).getClassName());
            } else if (_const instanceof NullConst) {
                constExpr = Expressions.newNullExpr();
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
            Expression leftExpr = getVarExpr(leftVar);
            Expression rightExpr = getVarExpr(rightVar);
            ASTBinaryOperator op;
            switch (inst.getOperator()) {
                case AND: op = ASTBinaryOperator.AND; break;
                case DIV: op = ASTBinaryOperator.DIV; break;
                case EQ: op = ASTBinaryOperator.EQ; break;
                case GE: op = ASTBinaryOperator.GE; break;
                case GT: op = ASTBinaryOperator.GT; break;
                case LE: op = ASTBinaryOperator.LE; break;
                case LOGICAL_SHIFT_RIGHT: op = ASTBinaryOperator.LOGICAL_SHIFT_RIGHT; break;
                case LT: op = ASTBinaryOperator.LT; break;
                case MINUS: op = ASTBinaryOperator.MINUS; break;
                case MUL: op = ASTBinaryOperator.MUL; break;
                case NE: op = ASTBinaryOperator.NE; break;
                case OR: op = ASTBinaryOperator.OR; break;
                case PLUS: op = ASTBinaryOperator.PLUS; break;
                case REMAINDER: op = ASTBinaryOperator.REMAINDER; break;
                case SHIFT_LEFT: op = ASTBinaryOperator.SHIFT_LEFT; break;
                case SHIFT_RIGHT: op = ASTBinaryOperator.SHIFT_RIGHT; break;
                case XOR: op = ASTBinaryOperator.XOR; break;
                default: throw new InternalError();
            }
            Expression binExpr = Expressions.newBinExpr(leftExpr, rightExpr, op);
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
            if (inst.getSignature().isConstructor()) {
                // contructor call
                if (objExpr instanceof ObjectCreationExpression) {
                    ((ObjectCreationExpression) objExpr).setArguments(argsExpr);
                } else {
                    // TODO
                }
            } else {
                Expression callExpr
                        = Expressions.newMethodExpr(objExpr, inst.getSignature().getMethodName(),
                                                    argsExpr);
                if (liveVariables.get(bb).contains(resultVar)) {
                    expressions.put(resultVar.getID(), callExpr);
                } else {
                    blockStmt.add(new ExpressionStatement(callExpr));
                }
            }
            return null;
        }

        @Override
        public Void visit(CallStaticMethodInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            BasicBlock bb = resultVar.getBasicBlock();
            List<Expression> argsExpr = new ArrayList<Expression>(inst.getArgumentCount());
            for (Variable argVar : inst.getArguments()) {
                argsExpr.add(getVarExpr(argVar));
            }
            Expression typeExpr
                    = Expressions.newTypeExpr(JavaType.newRefType(inst.getScope()));
            Expression callExpr
                    = Expressions.newMethodExpr(typeExpr, inst.getSignature().getMethodName(),
                                                argsExpr);
            if (liveVariables.get(bb).contains(resultVar)) {
                expressions.put(resultVar.getID(), callExpr);
            } else {
                blockStmt.add(new ExpressionStatement(callExpr));
            }
            return null;
        }

        @Override
        public Void visit(CastInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable var = inst.getVar();
            Expression expr = getVarExpr(var);
            Expression castExpr = Expressions.newCastExpr(inst.getCastType(), expr);
            addVarAssignExpr(resultVar, castExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(ConditionalInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable condVar = inst.getCond();
            Variable thenVar = inst.getThen();
            Variable elseVar = inst.getElse();
            Expression condExpr = ExpressionInverter.invert(getVarExpr(condVar));
            Expression thenExpr = getVarExpr(thenVar);
            Expression elseExpr = getVarExpr(elseVar);
            Expression ternExpr = Expressions.newCondExpr(condExpr, elseExpr, thenExpr);
            addVarAssignExpr(resultVar, ternExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(GetArrayInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable arrayVar = inst.getArray();
            Variable indexVar = inst.getIndex();
            Expression arrayExpr = getVarExpr(arrayVar);
            Expression indexExpr = getVarExpr(indexVar);
            Expression accessExpr = Expressions.newArrayAccess(arrayExpr, indexExpr);
            addVarAssignExpr(resultVar, accessExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(SetArrayInst inst, BlockStatement blockStmt) {
            Variable arrayVar = inst.getArray();
            Variable indexVar = inst.getIndex();
            Variable valueVar = inst.getValue();
            Expression arrayExpr = getVarExpr(arrayVar);
            Expression indexExpr = getVarExpr(indexVar);
            Expression valueExpr = getVarExpr(valueVar);
            if (arrayExpr instanceof ArrayCreationExpression) {
                ((ArrayCreationExpression) arrayExpr).addInitValue(valueExpr);
            } else {
                Expression accessExpr = Expressions.newArrayAccess(arrayExpr, indexExpr);
                Expression assignExpr
                            = Expressions.newAssignExpr(accessExpr, valueExpr,
                                                        AssignOperator.ASSIGN);
                blockStmt.add(new ExpressionStatement(assignExpr));
            }
            return null;
        }

        @Override
        public Void visit(GetFieldInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable objVar = inst.getObject();
            Expression objExpr = getVarExpr(objVar);
            Expression fieldExpr = Expressions.newFieldAccesExpr(objExpr, inst.getFieldName());
            addVarAssignExpr(resultVar, fieldExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(SetFieldInst inst, BlockStatement blockStmt) {
            Variable valueVar = inst.getValue();
            Expression valueExpr = getVarExpr(valueVar);
            Variable objVar = inst.getObject();
            Expression objExpr = getVarExpr(objVar);
            Expression fieldExpr = Expressions.newFieldAccesExpr(objExpr, inst.getFieldName());
            Expression assignExpr
                    = Expressions.newAssignExpr(fieldExpr, valueExpr,
                                                AssignOperator.ASSIGN);
            blockStmt.add(new ExpressionStatement(assignExpr));
            return null;
        }

        @Override
        public Void visit(JumpIfInst inst, BlockStatement blockStmt) {
            Variable condVar = inst.getCond();
            Expression condExpr = getVarExpr(condVar);
            blockStmt.add(new IfStatement(condExpr));
            return null;
        }

        @Override
        public Void visit(InstanceOfInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            Variable var = inst.getVar();
            Expression expr = getVarExpr(var);
            Expression typeExpr = Expressions.newTypeExpr(inst.getType());
            Expression instOfExpr = Expressions.newBinExpr(expr, typeExpr, ASTBinaryOperator.INSTANCE_OF);
            addVarAssignExpr(resultVar, instOfExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(MonitorEnterInst inst, BlockStatement blockStmt) {
            Variable objVar = inst.getObj();
            Expression objExpr = getVarExpr(objVar);
            blockStmt.add(new MonitorEnterStatement(objExpr));
            return null;
        }

        @Override
        public Void visit(MonitorExitInst inst, BlockStatement blockStmt) {
            Variable objVar = inst.getObj();
            Expression objExpr = getVarExpr(objVar);
            blockStmt.add(new MonitorExitStatement(objExpr));
            return null;
        }

        @Override
        public Void visit(NewArrayInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            List<Expression> dimExprs = new ArrayList<Expression>(inst.getDimensionCount());
            for (Variable dimVar : inst.getDimensions()) {
                dimExprs.add(getVarExpr(dimVar));
            }
            Expression newArrExpr = Expressions.newArrayCreatExpr(inst.getType(), dimExprs);
            addVarAssignExpr(resultVar, newArrExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(NewObjectInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            List<Expression> argExprs = new ArrayList<Expression>(inst.getArgumentCount());
            for (Variable argVar : inst.getArguments()) {
                argExprs.add(getVarExpr(argVar));
            }
            Expression newObjExpr = Expressions.newObjCreatExpr(inst.getType(), argExprs);
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
            Variable indexVar = inst.getIndex();
            Expression indexExpr = getVarExpr(indexVar);
            SwitchCaseStatement switchStmt = new SwitchCaseStatement(indexExpr);
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
            Variable var = inst.getVar();
            Expression expr = getVarExpr(var);
            ASTUnaryOperator op;
            switch (inst.getOperator()) {
                case MINUS:
                    op = ASTUnaryOperator.MINUS;
                    break;

                case NOT:
                    op = ASTUnaryOperator.NOT;
                    break;

                case NONE:
                    op = ASTUnaryOperator.NONE;
                    break;

                default:
                    throw new InternalError();
            }
            Expression unaryExpr = Expressions.newUnaryExpr(expr, op);
            addVarAssignExpr(resultVar, unaryExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(ChoiceInst inst, BlockStatement blockStmt) {
            throw new InternalError();
        }

        @Override
        public Void visit(PhiInst inst, BlockStatement blockStmt) {
            throw new InternalError();
        }

        @Override
        public Void visit(GetStaticFieldInst inst, BlockStatement blockStmt) {
            Variable resultVar = inst.getResult();
            JavaType type = JavaType.newRefType(inst.getScope());
            TypeExpression typeExpr = Expressions.newTypeExpr(type);
            Expression fieldExpr = Expressions.newFieldAccesExpr(typeExpr, inst.getFieldName());
            addVarAssignExpr(resultVar, fieldExpr, blockStmt);
            return null;
        }

        @Override
        public Void visit(SetStaticFieldInst inst, BlockStatement blockStmt) {
            Variable valueVar = inst.getValue();
            Expression valueExpr = getVarExpr(valueVar);
            JavaType type = JavaType.newRefType(inst.getScope());
            TypeExpression typeExpr = Expressions.newTypeExpr(type);
            Expression fieldExpr = Expressions.newFieldAccesExpr(typeExpr, inst.getFieldName());
            Expression assignExpr
                    = Expressions.newAssignExpr(fieldExpr, valueExpr,
                                                AssignOperator.ASSIGN);
            blockStmt.add(new ExpressionStatement(assignExpr));
            return null;
        }
    }

    public AbstractSyntaxTreeBuilder(ControlFlowGraph CFG, ImportManager importManager,
                                     Region rootRegion, BlockStatement methodBody) {
        this.CFG = CFG;
        this.importManager = importManager;
        this.rootRegion = rootRegion;
        this.methodBody = methodBody;
        expressions = new HashMap<VariableID, Expression>();
    }

    public void build() {
        this.liveVariables = new LiveVariablesAnalysis(CFG).analyse();
        buildAST(rootRegion, methodBody);
    }

    private void buildAST(Region region, BlockStatement blockStmt) {
        logger.log(Level.FINEST, "Build AST from region {0} {1}",
                new Object[] {region, region.getTypeName()});

        switch (region.getType()) {
            case EMPTY:
                break;

            case BASIC_BLOCK: {
                BasicBlockRegion region2 = (BasicBlockRegion) region;
                BasicBlock bb = region2.getBasicBlock();
                RegionTACInstVisitor visitor = new RegionTACInstVisitor();
                bb.getInstructions().accept(visitor, blockStmt);
                break;
            }

            case BLOCK: {
                BlockRegion region2 = (BlockRegion) region;
                for (Region child : region2.getRegions()) {
                    buildAST(child, blockStmt);
                }
                break;
            }

            case IF_THEN_ELSE_JOIN:
            case IF_THEN_BREAK_ELSE_BREAK: {
                IfThenElseRegion region2 = (IfThenElseRegion) region;

                buildAST(region2.getIfRegion(), blockStmt);
                IfStatement ifStmt = (IfStatement) blockStmt.getLast();
                ifStmt.invertCondition();
                BlockStatement thenBlockStmt = new BlockStatement();
                ifStmt.setThen(thenBlockStmt);
                BlockStatement elseBlockStmt = new BlockStatement();
                ifStmt.setElse(elseBlockStmt);
                buildAST(region2.getThenRegion(), elseBlockStmt);
                buildAST(region2.getElseRegion(), thenBlockStmt);
                if (ifStmt.getThen().isEmpty() && ifStmt.getElse().isEmpty()) {
                    ifStmt.remove();
                }
                break;
            }

            case IF_THEN_JOIN:
            case IF_THEN_BREAK: {
                IfThenRegion region2 = (IfThenRegion) region;

                buildAST(region2.getIfRegion(), blockStmt);
                IfStatement ifStmt = (IfStatement) blockStmt.getLast();
                if (region2.mustInvertCondition()) {
                    ifStmt.invertCondition();
                }
                BlockStatement thenBlockStmt = new BlockStatement();
                ifStmt.setThen(thenBlockStmt);
                buildAST(region2.getThenRegion(), thenBlockStmt);
                if (region.getType() == RegionType.IF_THEN_BREAK) {
                    thenBlockStmt.add(new BreakStatement());
                }
                if (ifStmt.getThen().isEmpty()) {
                    ifStmt.remove();
                }
                break;
            }

            case LABELED: {
                LabeledRegion region2 = (LabeledRegion) region;

                BlockStatement bodyBlockStmt = new BlockStatement();
                buildAST(region2.getBodyRegion(), bodyBlockStmt);

                blockStmt.add(new LabeledStatement("LABEL", bodyBlockStmt));

                break;
            }

            case INFINITE_LOOP: {
                InfiniteLoopRegion region2 = (InfiniteLoopRegion) region;

                BlockStatement bodyBlockStmt = new BlockStatement();
                buildAST(region2.getLoopRegion(), bodyBlockStmt);

                blockStmt.add(new WhileStatement(Expressions.newBooleanExpr(true), bodyBlockStmt));

                break;
            }

            case DO_WHILE_LOOP: {
                DoWhileLoopRegion region2 = (DoWhileLoopRegion) region;

                BlockStatement bodyBlockStmt = new BlockStatement();
                buildAST(region2.getLoopRegion(), bodyBlockStmt);

                IfStatement ifStmt = (IfStatement) bodyBlockStmt.getLast();
                ifStmt.remove();
                Expression condition = ExpressionInverter.invert(ifStmt.getCondition());
                blockStmt.add(new DoWhileStatement(bodyBlockStmt, condition));

                break;
            }

            case WHILE_LOOP: {
                WhileLoopRegion region2 = (WhileLoopRegion) region;

                BlockStatement bodyBlockStmt = new BlockStatement();
                buildAST(region2.getIfRegion(), bodyBlockStmt);
                IfStatement ifStmt = (IfStatement) bodyBlockStmt.getFirst();
                ifStmt.remove();
                Expression condition = ExpressionInverter.invert(ifStmt.getCondition());

                buildAST(region2.getLoopRegion(), bodyBlockStmt);

                blockStmt.add(new WhileStatement(condition, bodyBlockStmt));

                break;
            }

            case SWITCH_CASE: {
                SwitchCaseRegion region2 = (SwitchCaseRegion) region;

                buildAST(region2.getSwitchRegion(), blockStmt);
                SwitchCaseStatement switchStmt
                        = (SwitchCaseStatement) blockStmt.getLast();

                for (CaseRegion caseRegion : region2.getCaseRegions()) {
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

                    switchStmt.addCase(new CaseStatement(caseRegion.getValues(), caseStmts));
                }

                break;
            }

            case INLINED_FINALLY_BREAK: {
                InlinedFinallyBreakRegion region2 = (InlinedFinallyBreakRegion) region;

                buildAST(region2.getTryRegion(), blockStmt);

                break;
            }

            case TRY_CATCH_FINALLY: {
                TryCatchFinallyRegion region2 = (TryCatchFinallyRegion) region;

                BlockStatement tryBlockStmt = new BlockStatement();
                buildAST(region2.getTryRegion1(), tryBlockStmt);

                List<CatchClause> catchClauses = new ArrayList<CatchClause>();
                BlockStatement finallyBlockStmt = null;

                for (CatchRegion catchRegion : region2.getCatchRegions()) {
                    BlockStatement catchBlockStmt = new BlockStatement();
                    buildAST(catchRegion.getRegion(), catchBlockStmt);

                    ExceptionHandlerInfo info
                            = (ExceptionHandlerInfo) catchRegion.getIncomingEdge().getValue();
                    Variable excVar = info.getVariable();
                    LocalVariableDeclaration varDecl
                            = new LocalVariableDeclaration(Expressions.newVarExpr(excVar.getID(), excVar.getName()),
                                                           excVar.getType());
                    catchClauses.add(new CatchClause(catchBlockStmt, varDecl));
                }
                if (region2.getFinallyRegion() != null) {
                    finallyBlockStmt = new BlockStatement();
                    buildAST(region2.getFinallyRegion().getRegion(), finallyBlockStmt);
                }

                TryCatchFinallyStatement tryCatchStmt
                        = new TryCatchFinallyStatement(tryBlockStmt, catchClauses, finallyBlockStmt);
                blockStmt.add(tryCatchStmt);

                break;
            }
        }
    }
}
