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
import fr.jamgotchian.abcd.core.ast.expr.ArrayLength;
import fr.jamgotchian.abcd.core.ast.expr.AssignExpression;
import fr.jamgotchian.abcd.core.ast.expr.AssignOperator;
import fr.jamgotchian.abcd.core.ast.expr.BinaryExpression;
import fr.jamgotchian.abcd.core.ast.expr.BinaryOperator;
import fr.jamgotchian.abcd.core.ast.expr.CastExpression;
import fr.jamgotchian.abcd.core.ast.expr.ClassExpression;
import fr.jamgotchian.abcd.core.ast.expr.Constant;
import fr.jamgotchian.abcd.core.ast.expr.Expression;
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

    private static final String[] ATYPES = { null, null, null, null, "bool", "char", "float",
                                           "double", "byte", "short", "int", "long" };

    protected final ExpressionStack stack;

    BasicBlockStmtAnalysis(ExpressionStack stack) {
        this.stack = stack;
    }

    protected void pushExpr(Expression expr, BasicBlock block) {
        expr.setBasicBlock(block);
        stack.push(expr);
    }

    private void addStmt(BasicBlock block, Expression expr) {
        addStmt(block, new ExpressionStatement(expr));
    }

    protected void addStmt(BasicBlock block, Statement stmt) {
        logger.log(Level.FINER, "Add stmt : {0}", OutputUtil.toText(stmt));

        ((BasicBlockAnalysisDataImpl) block.getData()).addStatement(stmt);
    }

    public void before(BasicBlock block) {
    }

    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {

        switch (node.getOpcode()) {
            case GETSTATIC:
                pushExpr(new FieldAccess(new ClassExpression(node.owner.replace('/', '.')),
                                             node.name), block);
                break;

            case PUTSTATIC:
                addStmt(block, new AssignExpression(new FieldAccess(new ClassExpression(node.owner.replace('/', '.')), node.name),
                                                           stack.pop(),
                                                           AssignOperator.ASSIGN));
                break;

            case GETFIELD:
                pushExpr(new FieldAccess(stack.pop(), node.name), block);
                break;

            case PUTFIELD:
                Expression value = stack.pop();
                Expression objectRef = stack.pop();
                addStmt(block, new AssignExpression(new FieldAccess(objectRef, node.name),
                                                           value,
                                                           AssignOperator.ASSIGN));
                break;
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        if (node.incr == 1) {
            addStmt(block, new UnaryExpression(new LocalVariable(node.var),
                                               UnaryOperator.POST_INCREMENT));
        } else if (node.incr == -1) {
            addStmt(block, new UnaryExpression(new LocalVariable(node.var),
                                               UnaryOperator.POST_DECREMENT));
        } else if (node.incr > 1) {
            addStmt(block, new AssignExpression(new LocalVariable(node.var),
                                                new Constant(Integer.valueOf(node.incr)),
                                                AssignOperator.PLUS));
        } else if (node.incr < -1) {
            addStmt(block, new AssignExpression(new LocalVariable(node.var),
                                                new Constant(Integer.valueOf(-node.incr)),
                                                AssignOperator.MINUS));
        }
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL:
                pushExpr(new Constant(null), block);
                break;

            case ICONST_M1:
                pushExpr(new Constant(Integer.valueOf(1)), block);
                break;

            case ICONST_0:
                pushExpr(new Constant(Integer.valueOf(0)), block);
                break;

            case ICONST_1:
                pushExpr(new Constant(Integer.valueOf(1)), block);
                break;

            case ICONST_2:
                pushExpr(new Constant(Integer.valueOf(2)), block);
                break;

            case ICONST_3:
                pushExpr(new Constant(Integer.valueOf(3)), block);
                break;

            case ICONST_4:
                pushExpr(new Constant(Integer.valueOf(4)), block);
                break;

            case ICONST_5:
                pushExpr(new Constant(Integer.valueOf(5)), block);
                break;

            case LCONST_0:
                pushExpr(new Constant(Long.valueOf(0)), block);
                break;

            case LCONST_1:
                pushExpr(new Constant(Long.valueOf(1)), block);
                break;

            case FCONST_0:
                pushExpr(new Constant(Float.valueOf(0f)), block);
                break;

            case FCONST_1:
                pushExpr(new Constant(Float.valueOf(1f)), block);
                break;

            case FCONST_2:
                pushExpr(new Constant(Float.valueOf(2f)), block);
                break;

            case DCONST_0:
                pushExpr(new Constant(Double.valueOf(0d)), block);
                break;

            case DCONST_1:
                pushExpr(new Constant(Double.valueOf(1d)), block);
                break;

            case IALOAD:
            case LALOAD:
            case FALOAD:
            case DALOAD:
            case AALOAD:
            case BALOAD:
            case CALOAD:
            case SALOAD: {
                Expression arrayCountExpr = stack.pop();
                Expression arrayRef = stack.pop();
                pushExpr(new ArrayAccess(arrayRef, arrayCountExpr), block);
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
                Expression arrayCountExpr = stack.pop();
                Expression arrayRef = stack.pop();
                if (arrayRef instanceof ArrayCreationExpression) {
                    ((ArrayCreationExpression) arrayRef).addInitValue(valueExpr);
                } else {
                    addStmt(block, new AssignExpression(new ArrayAccess(arrayRef, arrayCountExpr),
                                                                valueExpr,
                                                                AssignOperator.ASSIGN));
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
                pushExpr(stack.peek(), block);
                break;

            case DUP_X1:
            case DUP_X2:
            case DUP2_X1:
            case DUP2_X2:
                throw new ABCDException("TODO");

            case SWAP: {
                Expression expr1 = stack.pop();
                Expression expr2 = stack.pop();
                pushExpr(expr1, block);
                pushExpr(expr2, block);
                break;
            }

            case IADD:
            case LADD:
            case FADD:
            case DADD: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.PLUS), block);
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.MINUS), block);
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.MUL), block);
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.DIV), block);
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.REMAINDER), block);
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG:
                pushExpr(new UnaryExpression(stack.pop(), UnaryOperator.MINUS), block);
                break;

            case ISHL:
            case LSHL: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.SHIFT_LEFT), block);
                break;
            }

            case ISHR:
            case LSHR: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.SHIFT_RIGHT), block);
                break;
            }

            case IUSHR:
            case LUSHR:
                throw new ABCDException("TODO");

            case IAND:
            case LAND: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.AND), block);
                break;
            }

            case IOR:
            case LOR: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                pushExpr(new BinaryExpression(left, right, BinaryOperator.OR), block);
                break;
            }

            case IXOR:
            case LXOR:
                throw new ABCDException("TODO");

            case I2L:
                pushExpr(new CastExpression("long", stack.pop()), block);
                break;

            case I2F:
                pushExpr(new CastExpression("float", stack.pop()), block);
                break;

            case I2D:
                pushExpr(new CastExpression("double", stack.pop()), block);
                break;

            case L2I:
                pushExpr(new CastExpression("int", stack.pop()), block);
                break;

            case L2F:
                pushExpr(new CastExpression("float", stack.pop()), block);
                break;

            case L2D:
                pushExpr(new CastExpression("double", stack.pop()), block);
                break;

            case F2I:
                pushExpr(new CastExpression("int", stack.pop()), block);
                break;

            case F2L:
                pushExpr(new CastExpression("long", stack.pop()), block);
                break;

            case F2D:
                pushExpr(new CastExpression("double", stack.pop()), block);
                break;

            case D2I:
                pushExpr(new CastExpression("int", stack.pop()), block);
                break;

            case D2L:
                pushExpr(new CastExpression("long", stack.pop()), block);
                break;

            case D2F:
                pushExpr(new CastExpression("float", stack.pop()), block);
                break;

            case I2B:
                pushExpr(new CastExpression("byte", stack.pop()), block);
                break;

            case I2C:
                pushExpr(new CastExpression("char", stack.pop()), block);
                break;

            case I2S:
                pushExpr(new CastExpression("short", stack.pop()), block);
                break;

            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG:
                throw new ABCDException("TODO");

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
                pushExpr(new ArrayLength(stack.pop()), block);
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
                pushExpr(new Constant(Byte.valueOf((byte) node.operand)), block);
                break;

            case SIPUSH:
                pushExpr(new Constant(Short.valueOf((short) node.operand)), block);
                break;

            case NEWARRAY:
                pushExpr(new ArrayCreationExpression(ATYPES[node.operand], stack.pop()), block);
                break;
        }
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {

        Expression expr = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                expr = new BinaryExpression(stack.pop(), new Constant(Integer.valueOf(0)),
                                   BinaryOperator.EQ);
                break;
            }

            case IFNE: {
                expr = new BinaryExpression(stack.pop(), new Constant(Integer.valueOf(0)),
                                   BinaryOperator.NE);
                break;
            }

            case IFLT: {
                expr = new BinaryExpression(stack.pop(), new Constant(Integer.valueOf(0)),
                                   BinaryOperator.LT);
                break;
            }

            case IFGE: {
                expr = new BinaryExpression(stack.pop(), new Constant(Integer.valueOf(0)),
                                   BinaryOperator.GE);
                break;
            }

            case IFGT: {
                expr = new BinaryExpression(stack.pop(), new Constant(Integer.valueOf(0)),
                                   BinaryOperator.GT);
                break;
            }

            case IFLE: {
                expr = new BinaryExpression(stack.pop(), new Constant(Integer.valueOf(0)),
                                   BinaryOperator.LE);
                break;
            }

            case IF_ICMPEQ: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.EQ);
                break;
            }

            case IF_ICMPNE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.NE);
                break;
            }

            case IF_ICMPLT: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.LT);
                break;
            }

            case IF_ICMPGE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.GE);
                break;
            }

            case IF_ICMPGT: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.GT);
                break;
            }

            case IF_ICMPLE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.LE);
                break;
            }

            case IF_ACMPEQ: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.EQ);
                break;
            }

            case IF_ACMPNE: {
                Expression right = stack.pop();
                Expression left = stack.pop();
                expr = new BinaryExpression(left, right, BinaryOperator.NE);
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                expr = new BinaryExpression(stack.pop(), new Constant(null), BinaryOperator.EQ);
                break;
            }

            case IFNONNULL: {
                expr = new BinaryExpression(stack.pop(), new Constant(null), BinaryOperator.NE);
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
            pushExpr(new Constant(((Type)node.cst).getClassName()), block);
        } else {
            pushExpr(new Constant(node.cst), block);
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
                    expr = new MethodCall(scope, node.name, args);
                    break;
                }

                case INVOKESTATIC:
                    expr = new MethodCall(new ClassExpression(node.owner.replace('/', '.')),
                                                                  node.name,
                                                                  args);
                    break;
            }

            if (returnType == Type.VOID_TYPE) {
                addStmt(block, expr);
            } else {
                pushExpr(expr, block);
            }
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node) {
        throw new ABCDException("TODO");
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
        String typeName = Type.getObjectType(node.desc).getClassName();

        switch (node.getOpcode()) {
            case NEW:
                pushExpr(new ObjectCreationExpression(typeName), block);
                break;

            case ANEWARRAY:
                pushExpr(new ArrayCreationExpression(typeName, stack.pop()), block);
                break;

            case CHECKCAST:
                pushExpr(new CastExpression(typeName, stack.pop()), block);
                break;

            case INSTANCEOF:
                pushExpr(new BinaryExpression(stack.pop(),
                                                  new ClassExpression(typeName),
                                                  BinaryOperator.INSTANCE_OF), block);
                break;
        }
    }

    public void visitVarInsn(BasicBlock block, int index, VarInsnNode node) {
        switch (node.getOpcode()) {
            case ILOAD:
            case LLOAD:
            case FLOAD:
            case DLOAD:
            case ALOAD:
                pushExpr(new LocalVariable(node.var), block);
                break;

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                addStmt(block, new AssignExpression(new LocalVariable(node.var),
                                                           stack.pop(),
                                                           AssignOperator.ASSIGN));
                break;
            }

            case RET:
                throw new ABCDException("TODO : support RET instruction");
        }
    }

    public void after(BasicBlock block) {
    }

}
