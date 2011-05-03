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

import java.util.List;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;
import fr.jamgotchian.abcd.core.common.Label;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockVisitor;
import fr.jamgotchian.abcd.core.ast.expr.ArrayAccess;
import fr.jamgotchian.abcd.core.ast.expr.ArrayCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.AssignOperator;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.TypeExpression;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.FieldAccess;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.expr.MethodCall;
import fr.jamgotchian.abcd.core.ast.expr.ObjectCreationExpression;
import fr.jamgotchian.abcd.core.ast.expr.UnaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.UnaryOperator;
import fr.jamgotchian.abcd.core.ast.stmt.ExpressionStatement;
import fr.jamgotchian.abcd.core.ast.stmt.GotoStatement;
import fr.jamgotchian.abcd.core.ast.stmt.JumpIfStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LabelStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LookupOrTableSwitchStatement;
import fr.jamgotchian.abcd.core.ast.stmt.MonitorEnterStatement;
import fr.jamgotchian.abcd.core.ast.stmt.MonitorExitStatement;
import fr.jamgotchian.abcd.core.ast.stmt.ReturnStatement;
import fr.jamgotchian.abcd.core.ast.stmt.Statement;
import fr.jamgotchian.abcd.core.ast.stmt.ThrowStatement;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.ast.util.ExpressionStack;
import fr.jamgotchian.abcd.core.output.OutputUtil;
import org.objectweb.asm.Type;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.IincInsnNode;
import org.objectweb.asm.tree.InsnNode;
import org.objectweb.asm.tree.IntInsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LdcInsnNode;
import org.objectweb.asm.tree.LookupSwitchInsnNode;
import org.objectweb.asm.tree.MethodInsnNode;
import org.objectweb.asm.tree.MultiANewArrayInsnNode;
import org.objectweb.asm.tree.TableSwitchInsnNode;
import org.objectweb.asm.tree.TypeInsnNode;
import org.objectweb.asm.tree.VarInsnNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class BasicBlockStmtAnalysis implements BasicBlockVisitor {

    private static final Logger logger = Logger.getLogger(BasicBlockStmtAnalysis.class.getName());

    static {
        logger.setLevel(Level.FINEST);
    }

    public static final JavaType[] ATYPES = {
        null,
        null,
        null,
        null,
        JavaType.BOOLEAN,
        JavaType.CHAR,
        JavaType.FLOAT,
        JavaType.DOUBLE,
        JavaType.BYTE,
        JavaType.SHORT,
        JavaType.INT,
        JavaType.LONG
    };

    protected final ExpressionStack stack;

    protected final ImportManager importManager;

    BasicBlockStmtAnalysis(ExpressionStack stack, ImportManager importManager) {
        this.stack = stack;
        this.importManager = importManager;
    }

    private void addStmt(BasicBlock block, Expression expr) {
        addStmt(block, new ExpressionStatement(expr));
    }

    protected void addStmt(BasicBlock block, Statement stmt) {
        logger.log(Level.FINER, "Add stmt : {0}", OutputUtil.toText(stmt));

        ((AnalysisData) block.getData()).addStatement(stmt);
    }

    public void before(BasicBlock block) {
    }

    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {

        switch (node.getOpcode()) {
            case GETSTATIC: {
                ClassName clsName = importManager.newClassName(node.owner.replace('/', '.'));
                JavaType type = JavaType.newRefType(clsName);
                TypeExpression clsExpr = Expressions.newTypeExpr(type, block);
                FieldAccess fieldExpr = Expressions.newFieldAccesExpr(clsExpr, node.name, block);
                stack.push(fieldExpr);
                break;
            }

            case PUTSTATIC: {
                ClassName clsName = importManager.newClassName(node.owner.replace('/', '.'));
                JavaType type = JavaType.newRefType(clsName);
                TypeExpression clsExpr = Expressions.newTypeExpr(type, block);
                FieldAccess fieldExpr = Expressions.newFieldAccesExpr(clsExpr, node.name, block);
                AssignExpression assignExpr = Expressions.newAssignExpr(fieldExpr, stack.pop(), AssignOperator.ASSIGN, block);
                addStmt(block, assignExpr);
                break;
            }

            case GETFIELD: {
                FieldAccess fieldExpr = Expressions.newFieldAccesExpr(stack.pop(), node.name, block);
                stack.push(fieldExpr);
                break;
            }

            case PUTFIELD: {
                Expression value = stack.pop();
                Expression objectRef = stack.pop();
                FieldAccess fieldExpr = Expressions.newFieldAccesExpr(objectRef, node.name, block);
                AssignExpression assignExpr = Expressions.newAssignExpr(fieldExpr, value, AssignOperator.ASSIGN, block);
                addStmt(block, assignExpr);
                break;
            }
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        if (node.incr == 1) {
            UnaryExpression unaryExpr
                    = Expressions.newUnaryExpr(Expressions.newVarExpr(node.var, block),
                                               UnaryOperator.POST_INCREMENT,
                                               block);
            addStmt(block, unaryExpr);
        } else if (node.incr == -1) {
            UnaryExpression unaryExpr
                    = Expressions.newUnaryExpr(Expressions.newVarExpr(node.var, block),
                                               UnaryOperator.POST_DECREMENT,
                                               block);
            addStmt(block, unaryExpr);
        } else if (node.incr > 1) {
            AssignExpression assignExpr
                    = Expressions.newAssignExpr(Expressions.newVarExpr(node.var, block),
                                                Expressions.newIntExpr(node.incr, block),
                                                AssignOperator.PLUS,
                                                block);
            addStmt(block, assignExpr);
        } else if (node.incr < -1) {
            AssignExpression assignExpr
                    = Expressions.newAssignExpr(Expressions.newVarExpr(node.var, block),
                                                Expressions.newIntExpr(-node.incr, block),
                                                AssignOperator.MINUS,
                                                block);
            addStmt(block, assignExpr);
        }
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL:
                stack.push(Expressions.newNullExpr(block));
                break;

            case ICONST_M1:
                stack.push(Expressions.newIntExpr(1, block));
                break;

            case ICONST_0:
                stack.push(Expressions.newIntExpr(0, block));
                break;

            case ICONST_1:
                stack.push(Expressions.newIntExpr(1, block));
                break;

            case ICONST_2:
                stack.push(Expressions.newIntExpr(2, block));
                break;

            case ICONST_3:
                stack.push(Expressions.newIntExpr(3, block));
                break;

            case ICONST_4:
                stack.push(Expressions.newIntExpr(4, block));
                break;

            case ICONST_5:
                stack.push(Expressions.newIntExpr(5, block));
                break;

            case LCONST_0:
                stack.push(Expressions.newLongExpr(0, block));
                break;

            case LCONST_1:
                stack.push(Expressions.newLongExpr(1, block));
                break;

            case FCONST_0:
                stack.push(Expressions.newFloatExpr(0f, block));
                break;

            case FCONST_1:
                stack.push(Expressions.newFloatExpr(1f, block));
                break;

            case FCONST_2:
                stack.push(Expressions.newFloatExpr(2f, block));
                break;

            case DCONST_0:
                stack.push(Expressions.newDoubleExpr(0d, block));
                break;

            case DCONST_1:
                stack.push(Expressions.newDoubleExpr(1d, block));
                break;

            case IALOAD:
            case LALOAD:
            case FALOAD:
            case DALOAD:
            case AALOAD:
            case BALOAD:
            case CALOAD:
            case SALOAD: {
                Expression arrayIndexExpr = stack.pop();
                Expression arrayRef = stack.pop();
                stack.push(Expressions.newArrayAcces(arrayRef, arrayIndexExpr, block));
                break;
            }

            case IASTORE:
            case LASTORE:
            case FASTORE:
            case DASTORE:
            case AASTORE:
            case BASTORE:
            case CASTORE:
            case SASTORE: {
                Expression valueExpr = stack.pop();
                Expression arrayIndexExpr = stack.pop();
                Expression arrayRef = stack.pop();
                if (arrayRef instanceof ArrayCreationExpression) {
                    ((ArrayCreationExpression) arrayRef).addInitValue(valueExpr);
                } else {
                    ArrayAccess arrayAccesExpr
                            = Expressions.newArrayAcces(arrayRef, arrayIndexExpr, block);
                    AssignExpression assignExpr
                            = Expressions.newAssignExpr(arrayAccesExpr,
                                                        valueExpr,
                                                        AssignOperator.ASSIGN,
                                                        block);
                    addStmt(block, assignExpr);
                }
                break;
            }

            case POP: {
                Expression expr = stack.pop();
                if (expr instanceof MethodCall) {
                    addStmt(block, expr);
                }
                break;
            }

            case POP2:
                throw new ABCDException("TODO");

            case DUP:
                stack.push(stack.peek());
                break;

            case DUP_X1: {
                Expression expr1 = stack.pop();
                Expression expr2 = stack.pop();
                stack.push(expr1);
                stack.push(expr2);
                stack.push(expr1);
                break;
            }

            case DUP_X2:
            case DUP2_X1:
            case DUP2_X2:
                throw new ABCDException("TODO");

            case SWAP: {
                Expression expr1 = stack.pop();
                Expression expr2 = stack.pop();
                stack.push(expr1);
                stack.push(expr2);
                break;
            }

            case IADD:
            case LADD:
            case FADD:
            case DADD: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.PLUS, block));
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.MINUS, block));
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.MUL, block));
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.DIV, block));
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.REMAINDER, block));
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG: {
                stack.push(Expressions.newUnaryExpr(stack.pop(), UnaryOperator.MINUS, block));
                break;
            }

            case ISHL:
            case LSHL: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.SHIFT_LEFT, block));
                break;
            }

            case ISHR:
            case LSHR: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.SHIFT_RIGHT, block));
                break;
            }

            case IUSHR:
            case LUSHR: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.LOGICAL_SHIFT_RIGHT, block));
                break;
            }

            case IAND:
            case LAND: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.AND, block));
                break;
            }

            case IOR:
            case LOR: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.OR, block));
                break;
            }

            case IXOR:
            case LXOR: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                stack.push(Expressions.newBinExpr(left, right, BinaryOperator.XOR, block));
                break;
            }

            case I2L:
                stack.push(Expressions.newCastExpr(JavaType.LONG, stack.pop(), block));
                break;

            case I2F:
                stack.push(Expressions.newCastExpr(JavaType.FLOAT, stack.pop(), block));
                break;

            case I2D:
                stack.push(Expressions.newCastExpr(JavaType.DOUBLE, stack.pop(), block));
                break;

            case L2I:
                stack.push(Expressions.newCastExpr(JavaType.INT, stack.pop(), block));
                break;

            case L2F:
                stack.push(Expressions.newCastExpr(JavaType.FLOAT, stack.pop(), block));
                break;

            case L2D:
                stack.push(Expressions.newCastExpr(JavaType.DOUBLE, stack.pop(), block));
                break;

            case F2I:
                stack.push(Expressions.newCastExpr(JavaType.INT, stack.pop(), block));
                break;

            case F2L:
                stack.push(Expressions.newCastExpr(JavaType.LONG, stack.pop(), block));
                break;

            case F2D:
                stack.push(Expressions.newCastExpr(JavaType.DOUBLE, stack.pop(), block));
                break;

            case D2I:
                stack.push(Expressions.newCastExpr(JavaType.INT, stack.pop(), block));
                break;

            case D2L:
                stack.push(Expressions.newCastExpr(JavaType.LONG, stack.pop(), block));
                break;

            case D2F:
                stack.push(Expressions.newCastExpr(JavaType.FLOAT, stack.pop(), block));
                break;

            case I2B:
                stack.push(Expressions.newCastExpr(JavaType.BYTE, stack.pop(), block));
                break;

            case I2C:
                stack.push(Expressions.newCastExpr(JavaType.CHAR, stack.pop(), block));
                break;

            case I2S:
                stack.push(Expressions.newCastExpr(JavaType.SHORT, stack.pop(), block));
                break;

            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG: {
                Expression value2 = stack.pop();
                Expression value1 = stack.pop();
                stack.push(Expressions.newBinExpr(value1, value2, BinaryOperator.MINUS, block));
                break;
            }

            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                addStmt(block, new ReturnStatement(stack.pop()));
                break;

            case RETURN:
                addStmt(block, new ReturnStatement());
                break;

            case ARRAYLENGTH:
                stack.push(Expressions.newArrayLength(stack.pop(), block));
                break;

            case ATHROW:
                addStmt(block, new ThrowStatement(stack.pop()));
                break;

            case MONITORENTER:
                addStmt(block, new MonitorEnterStatement(stack.pop()));
                break;

            case MONITOREXIT:
                addStmt(block, new MonitorExitStatement(stack.pop()));
                break;
        }
    }

    public void visitIntInsn(BasicBlock block, int index, IntInsnNode node) {
        switch (node.getOpcode()) {
            case BIPUSH:
                stack.push(Expressions.newByteExpr((byte) node.operand, block));
                break;

            case SIPUSH:
                stack.push(Expressions.newShortExpr((short) node.operand, block));
                break;

            case NEWARRAY:
                stack.push(Expressions.newArrayCreatExpr(ATYPES[node.operand], stack.pop(), block));
                break;
        }
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {

        Expression expr = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newIntExpr(0, block),
                                              BinaryOperator.EQ,
                                              block);
                break;
            }

            case IFNE: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newIntExpr(0, block),
                                              BinaryOperator.NE,
                                              block);
                break;
            }

            case IFLT: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newIntExpr(0, block),
                                              BinaryOperator.LT,
                                              block);
                break;
            }

            case IFGE: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newIntExpr(0, block),
                                              BinaryOperator.GE,
                                              block);
                break;
            }

            case IFGT: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newIntExpr(0, block),
                                              BinaryOperator.GT,
                                              block);
                break;
            }

            case IFLE: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newIntExpr(0, block),
                                              BinaryOperator.LE,
                                              block);
                break;
            }

            case IF_ICMPEQ: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.EQ, block);
                break;
            }

            case IF_ICMPNE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.NE, block);
                break;
            }

            case IF_ICMPLT: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.LT, block);
                break;
            }

            case IF_ICMPGE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.GE, block);
                break;
            }

            case IF_ICMPGT: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.GT, block);
                break;
            }

            case IF_ICMPLE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.LE, block);
                break;
            }

            case IF_ACMPEQ: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.EQ, block);
                break;
            }

            case IF_ACMPNE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = Expressions.newBinExpr(left, right, BinaryOperator.NE, block);
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newNullExpr(block),
                                              BinaryOperator.EQ,
                                              block);
                break;
            }

            case IFNONNULL: {
                expr = Expressions.newBinExpr(stack.pop(),
                                              Expressions.newNullExpr(block),
                                              BinaryOperator.NE,
                                              block);
                break;
            }
        }

        Label label = block.getGraph().getLabelManager().getLabel(node.label);

        if (expr != null) {
            addStmt(block, new JumpIfStatement(expr, label));
        } else {
            addStmt(block, new GotoStatement(label));
        }
    }

    public void visitLabel(BasicBlock block, int index, LabelNode node) {
        if (index == block.getRange().getFirst()) {
            Label label = block.getGraph().getLabelManager().getLabel(node);
            addStmt(block, new LabelStatement(label));
        }
    }

    public void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node) {
        if (node.cst instanceof Type) {
            ClassName className = importManager.newClassName(((Type)node.cst).getClassName());
            stack.push(Expressions.newClsExpr(className, block));
        } else if (node.cst instanceof Integer) {
            stack.push(Expressions.newIntExpr((Integer) node.cst, block));
        } else if (node.cst instanceof Long) {
            stack.push(Expressions.newLongExpr((Long) node.cst, block));
        } else if (node.cst instanceof Float) {
            stack.push(Expressions.newFloatExpr((Float) node.cst, block));
        } else if (node.cst instanceof Double) {
            stack.push(Expressions.newDoubleExpr((Double) node.cst, block));
        } else if (node.cst instanceof String) {
            stack.push(Expressions.newStringExpr(node.cst.toString(), block));
        }
    }

    public void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addStmt(block, new LookupOrTableSwitchStatement(stack.pop(), labels));
    }

    public void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node) {
        int argCount = Type.getArgumentTypes(node.desc).length;
        List<Expression> args = new ArrayList<Expression>(argCount);
        for (int i = 0; i < argCount; i++) {
            args.add(0, stack.pop());
        }

        if ("<init>".equals(node.name)) {
            Expression expr = stack.pop();
            // this is an instance initialization
            if (expr instanceof ObjectCreationExpression) {
                ((ObjectCreationExpression) expr).setArguments(args);
            }
        } else {
            Type returnType = Type.getReturnType(node.desc);

            MethodCall expr = null;

            switch (node.getOpcode()) {
                case INVOKEVIRTUAL:
                case INVOKESPECIAL:
                case INVOKEINTERFACE:
                case INVOKEDYNAMIC: {
                    Expression scope = stack.pop();
                    // replace this.foo() by foo()
                    if (scope instanceof LocalVariable
                            && ((LocalVariable) scope).getIndex() == 0) {
                        scope = null;
                    }
                    expr = Expressions.newMethodExpr(scope, node.name, args, block);
                    break;
                }

                case INVOKESTATIC: {
                    ClassName className = importManager.newClassName(node.owner.replace('/', '.'));
                    JavaType type = JavaType.newRefType(className);
                    expr = Expressions.newMethodExpr(Expressions.newTypeExpr(type, block),
                                                     node.name,
                                                     args,
                                                     block);
                    break;
                }
            }

            if (returnType == Type.VOID_TYPE) {
                addStmt(block, expr);
            } else {
                stack.push(expr);
            }
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node) {
        Type type = Type.getType(node.desc).getElementType();
        JavaType javaType = JavaType.newType(type, importManager);
        List<Expression> arrayLengthExpr = new ArrayList<Expression>(node.dims);
        for (int i = 0; i < node.dims; i++) {
            arrayLengthExpr.add(0, stack.pop());
        }
        stack.push(Expressions.newArrayCreatExpr(javaType, arrayLengthExpr, block));
    }

    public void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addStmt(block, new LookupOrTableSwitchStatement(stack.pop(), labels));
    }

    public void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node) {
        ClassName className
                = importManager.newClassName(Type.getObjectType(node.desc).getClassName());

        switch (node.getOpcode()) {
            case NEW:
                stack.push(Expressions.newObjCreatExpr(className, block));
                break;

            case ANEWARRAY: {
                JavaType type = JavaType.newRefType(className);
                stack.push(Expressions.newArrayCreatExpr(type, stack.pop(), block));
                break;
            }

            case CHECKCAST: {
                JavaType type = JavaType.newRefType(className);
                stack.push(Expressions.newCastExpr(type, stack.pop(), block));
                break;
            }

            case INSTANCEOF: {
                JavaType type = JavaType.newRefType(className);
                stack.push(Expressions.newBinExpr(stack.pop(),
                                                  Expressions.newTypeExpr(type, block),
                                                  BinaryOperator.INSTANCE_OF,
                                                  block));
                break;
            }
        }
    }

    public void visitVarInsn(BasicBlock block, int index, VarInsnNode node) {
        switch (node.getOpcode()) {
            case ILOAD:
            case LLOAD:
            case FLOAD:
            case DLOAD:
            case ALOAD:
                stack.push(Expressions.newVarExpr(node.var, block));
                break;

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                AssignExpression assignExpr
                        = Expressions.newAssignExpr(Expressions.newVarExpr(node.var, block),
                                                    stack.pop(),
                                                    AssignOperator.ASSIGN, block);
                addStmt(block, assignExpr);
                break;
            }

            case RET:
                throw new ABCDException("TODO : support RET instruction");
        }
    }

    public void after(BasicBlock block) {
    }

}
