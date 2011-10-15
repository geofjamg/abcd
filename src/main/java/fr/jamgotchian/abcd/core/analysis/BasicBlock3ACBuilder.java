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
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.LabelManager;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BytecodeRangeVisitor;
import fr.jamgotchian.abcd.core.controlflow.TACBinaryOperator;
import fr.jamgotchian.abcd.core.controlflow.ByteConst;
import fr.jamgotchian.abcd.core.controlflow.ClassConst;
import fr.jamgotchian.abcd.core.controlflow.DoubleConst;
import fr.jamgotchian.abcd.core.controlflow.FloatConst;
import fr.jamgotchian.abcd.core.controlflow.TACInst;
import fr.jamgotchian.abcd.core.controlflow.IntConst;
import fr.jamgotchian.abcd.core.controlflow.LongConst;
import fr.jamgotchian.abcd.core.controlflow.MethodSignature;
import fr.jamgotchian.abcd.core.controlflow.NullConst;
import fr.jamgotchian.abcd.core.controlflow.ShortConst;
import fr.jamgotchian.abcd.core.controlflow.StringConst;
import fr.jamgotchian.abcd.core.controlflow.Variable;
import fr.jamgotchian.abcd.core.controlflow.TACInstFactory;
import fr.jamgotchian.abcd.core.controlflow.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.controlflow.TACUnaryOperator;
import fr.jamgotchian.abcd.core.controlflow.util.TACInstWriter;
import fr.jamgotchian.abcd.core.controlflow.VariableStack;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
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
public class BasicBlock3ACBuilder extends BytecodeRangeVisitor {

    private static final Logger logger = Logger.getLogger(BasicBlock3ACBuilder.class.getName());

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

    protected final ClassNameFactory classNameFactory;

    protected final VariableStack stack;

    protected final TemporaryVariableFactory tmpVarFactory;

    protected final TACInstFactory instFactory;

    BasicBlock3ACBuilder(ClassNameFactory classNameFactory,
                        TemporaryVariableFactory tmpVarFactory,
                        VariableStack stack,
                        TACInstFactory instFactory) {
        this.classNameFactory = classNameFactory;
        this.tmpVarFactory = tmpVarFactory;
        this.stack = stack;
        this.instFactory = instFactory;
    }

    static void addInst(BasicBlock bb, TACInst inst) {
        logger.log(Level.FINER, "Add inst : {0}", TACInstWriter.toText(inst));
        bb.getInstructions().add(inst);
    }

    public void before(BasicBlock bb) {
    }

    public void visitFieldInsn(BasicBlock bb, int position, FieldInsnNode node) {
        JavaType fieldType = JavaType.newType(Type.getType(node.desc), classNameFactory);
        String fieldName = node.name;

        switch (node.getOpcode()) {
            case GETSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newGetStaticField(tmpVar, className, fieldName, fieldType));
                stack.push(tmpVar);
                break;
            }

            case PUTSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                Variable tmpVar = stack.pop();
                addInst(bb, instFactory.newSetStaticField(className, fieldName, fieldType, tmpVar));
                break;
            }

            case GETFIELD: {
                Variable resultVar = tmpVarFactory.create(bb);
                Variable objVar = stack.pop();
                addInst(bb, instFactory.newGetField(resultVar, objVar, fieldName, fieldType));
                stack.push(resultVar);
                break;
            }

            case PUTFIELD: {
                Variable valueVar = stack.pop();
                Variable objVar = stack.pop();
                addInst(bb, instFactory.newSetField(objVar, fieldName, fieldType, valueVar));
                break;
            }
        }
    }

    public void visitIincInsn(BasicBlock bb, int position, IincInsnNode node) {
        Variable tmpVar = tmpVarFactory.create(bb);
        addInst(bb, instFactory.newAssignVar(tmpVar, new Variable(node.var, bb, position)));
        Variable tmpValue = tmpVarFactory.create(bb);
        addInst(bb, instFactory.newAssignConst(tmpValue, new IntConst(Math.abs(node.incr))));
        Variable tmpResult = tmpVarFactory.create(bb);
        TACBinaryOperator binOp = node.incr > 0 ? TACBinaryOperator.PLUS : TACBinaryOperator.MINUS;
        addInst(bb, instFactory.newBinary(tmpResult, binOp, tmpVar, tmpValue));
        addInst(bb, instFactory.newAssignVar(new Variable(node.var, bb, position), tmpResult));
    }

    public void visitInsn(BasicBlock bb, int position, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new NullConst(classNameFactory)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_M1: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(1)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_0: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(0)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_1: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(1)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_2: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(2)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_3: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(3)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_4: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(4)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_5: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst(5)));
                stack.push(tmpVar);
                break;
            }

            case LCONST_0: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new LongConst(0)));
                stack.push(tmpVar);
                break;
            }

            case LCONST_1: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new LongConst(1)));
                stack.push(tmpVar);
                break;
            }

            case FCONST_0: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new FloatConst(0f)));
                stack.push(tmpVar);
                break;
            }

            case FCONST_1: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new FloatConst(1f)));
                stack.push(tmpVar);
                break;
            }

            case FCONST_2: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new FloatConst(2f)));
                stack.push(tmpVar);
                break;
            }

            case DCONST_0: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new DoubleConst(0d)));
                stack.push(tmpVar);
                break;
            }

            case DCONST_1: {
                Variable tmpVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpVar, new DoubleConst(1d)));
                stack.push(tmpVar);
                break;
            }

            case IALOAD:
            case LALOAD:
            case FALOAD:
            case DALOAD:
            case AALOAD:
            case BALOAD:
            case CALOAD:
            case SALOAD: {
                Variable arrayIndex = stack.pop();
                Variable arrayVar = stack.pop();
                Variable tmpResultVar = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newGetArray(tmpResultVar, arrayVar, arrayIndex));
                stack.push(tmpResultVar);
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
                Variable valueVar = stack.pop();
                Variable indexVar = stack.pop();
                Variable arrayVar = stack.pop();
                addInst(bb, instFactory.newSetArray(arrayVar, indexVar, valueVar));
                break;
            }

            case POP: {
                stack.pop();
                break;
            }

            case POP2:
                throw new ABCDException("TODO");

            case DUP: {
                Variable var = stack.pop();
                stack.push(var);
                stack.push(var);
                break;
            }

            case DUP_X1: {
                Variable var1 = stack.pop();
                Variable var2 = stack.pop();
                stack.push(var1);
                stack.push(var2);
                stack.push(var1);
                break;
            }

            case DUP_X2:
            case DUP2_X1:
            case DUP2_X2:
                throw new ABCDException("TODO");

            case SWAP: {
                Variable var1 = stack.pop();
                Variable var2 = stack.pop();
                stack.push(var1);
                stack.push(var2);
                break;
            }

            case IADD:
            case LADD:
            case FADD:
            case DADD: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.PLUS, left, right));
                stack.push(tmpResult);
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.MINUS, left, right));
                stack.push(tmpResult);
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.MUL, left, right));
                stack.push(tmpResult);
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.DIV, left, right));
                stack.push(tmpResult);
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.REMAINDER, left, right));
                stack.push(tmpResult);
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG: {
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newUnary(tmpResult, TACUnaryOperator.MINUS, stack.pop()));
                stack.push(tmpResult);
                break;
            }

            case ISHL:
            case LSHL: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.SHIFT_LEFT, left, right));
                stack.push(tmpResult);
                break;
            }

            case ISHR:
            case LSHR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.SHIFT_RIGHT, left, right));
                stack.push(tmpResult);
                break;
            }

            case IUSHR:
            case LUSHR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.LOGICAL_SHIFT_RIGHT, left, right));
                stack.push(tmpResult);
                break;
            }

            case IAND:
            case LAND: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.AND, left, right));
                stack.push(tmpResult);
                break;
            }

            case IOR:
            case LOR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.OR, left, right));
                stack.push(tmpResult);
                break;
            }

            case IXOR:
            case LXOR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.XOR, left, right));
                stack.push(tmpResult);
                break;
            }

            case I2L: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.LONG));
                stack.push(tmpResult);
                break;
            }

            case I2F: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.FLOAT));
                stack.push(tmpResult);
                break;
            }

            case I2D: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.DOUBLE));
                stack.push(tmpResult);
                break;
            }

            case L2I: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.INT));
                stack.push(tmpResult);
                break;
            }

            case L2F: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.FLOAT));
                stack.push(tmpResult);
                break;
            }

            case L2D: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.DOUBLE));
                stack.push(tmpResult);
                break;
            }

            case F2I: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.INT));
                stack.push(tmpResult);
                break;
            }

            case F2L: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.LONG));
                break;
            }

            case F2D: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.DOUBLE));
                stack.push(tmpResult);
                break;
            }

            case D2I: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.INT));
                stack.push(tmpResult);
                break;
            }

            case D2L: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.LONG));
                stack.push(tmpResult);
                break;
            }

            case D2F: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.FLOAT));
                stack.push(tmpResult);
                break;
            }

            case I2B: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.BYTE));
                stack.push(tmpResult);
                break;
            }

            case I2C: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.CHAR));
                stack.push(tmpResult);
                break;
            }

            case I2S: {
                Variable tmpResult = tmpVarFactory.create(bb);
                Variable var = stack.pop();
                addInst(bb, instFactory.newCast(tmpResult, var, JavaType.SHORT));
                stack.push(tmpResult);
                break;
            }

            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG: {
                Variable value2 = stack.pop();
                Variable value1 = stack.pop();
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.MINUS, value1, value2));
                stack.push(tmpResult);
                break;
            }

            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                addInst(bb, instFactory.newReturn(stack.pop()));
                break;

            case RETURN:
                addInst(bb, instFactory.newReturn());
                break;

            case ARRAYLENGTH: {
                Variable result = tmpVarFactory.create(bb);
                Variable arrayVar = stack.pop();
                addInst(bb, instFactory.newArrayLength(result, arrayVar));
                stack.push(result);
                break;
            }

            case ATHROW:
                addInst(bb, instFactory.newThrow(stack.pop()));
                break;

            case MONITORENTER:
                addInst(bb, instFactory.newMonitorEnter(stack.pop()));
                break;

            case MONITOREXIT:
                addInst(bb, instFactory.newMonitorExit(stack.pop()));
                break;
        }
    }

    public void visitIntInsn(BasicBlock bb, int position, IntInsnNode node) {
        Variable tmpVar = tmpVarFactory.create(bb);
        switch (node.getOpcode()) {
            case BIPUSH:
                addInst(bb, instFactory.newAssignConst(tmpVar, new ByteConst((byte) node.operand)));
                break;

            case SIPUSH:
                addInst(bb, instFactory.newAssignConst(tmpVar, new ShortConst((short) node.operand)));
                break;

            case NEWARRAY:
                addInst(bb, instFactory.newNewArray(tmpVar, ATYPES[node.operand],
                                                       Collections.singletonList(stack.pop())));
                break;
        }
        stack.push(tmpVar);
    }

    public void visitJumpInsn(BasicBlock bb, int position, JumpInsnNode node, LabelManager labelManager) {

        Variable tmpResult = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                Variable tmpZero = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.EQ, stack.pop(), tmpZero));
                break;
            }

            case IFNE: {
                Variable tmpZero = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.NE, stack.pop(), tmpZero));
                break;
            }

            case IFLT: {
                Variable tmpZero = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.LT, stack.pop(), tmpZero));
                break;
            }

            case IFGE: {
                Variable tmpZero = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.GE, stack.pop(), tmpZero));
                break;
            }

            case IFGT: {
                Variable tmpZero = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.GT, stack.pop(), tmpZero));
                break;
            }

            case IFLE: {
                Variable tmpZero = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.LE, stack.pop(), tmpZero));
                break;
            }

            case IF_ICMPEQ: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.EQ, left, right));
                break;
            }

            case IF_ICMPNE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.NE, left, right));
                break;
            }

            case IF_ICMPLT: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.LT, left, right));
                break;
            }

            case IF_ICMPGE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.GE, left, right));
                break;
            }

            case IF_ICMPGT: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.GT, left, right));
                break;
            }

            case IF_ICMPLE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.LE, left, right));
                break;
            }

            case IF_ACMPEQ: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.EQ, left, right));
                break;
            }

            case IF_ACMPNE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.NE, left, right));
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                Variable tmpNull = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpNull, new NullConst(classNameFactory)));
                stack.push(tmpNull);
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.EQ, stack.pop(), tmpNull));
                break;
            }

            case IFNONNULL: {
                Variable tmpNull = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newAssignConst(tmpNull, new NullConst(classNameFactory)));
                stack.push(tmpNull);
                tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newBinary(tmpResult, TACBinaryOperator.NE, stack.pop(), tmpNull));
                break;
            }
        }

        if (tmpResult != null) {
            addInst(bb, instFactory.newJumpIf(tmpResult.clone()));
        }
    }

    public void visitLabel(BasicBlock bb, int position, LabelNode node, LabelManager labelManager) {
    }

    public void visitLdcInsn(BasicBlock bb, int position, LdcInsnNode node) {
        Variable tmpVar = tmpVarFactory.create(bb);
        if (node.cst instanceof Type) {
            ClassName className = classNameFactory.newClassName(((Type)node.cst).getClassName());
            addInst(bb, instFactory.newAssignConst(tmpVar, new ClassConst(className, classNameFactory)));
        } else if (node.cst instanceof Integer) {
            addInst(bb, instFactory.newAssignConst(tmpVar, new IntConst((Integer) node.cst)));
        } else if (node.cst instanceof Long) {
            addInst(bb, instFactory.newAssignConst(tmpVar, new LongConst((Long) node.cst)));
        } else if (node.cst instanceof Float) {
            addInst(bb, instFactory.newAssignConst(tmpVar, new FloatConst((Float) node.cst)));
        } else if (node.cst instanceof Double) {
            addInst(bb, instFactory.newAssignConst(tmpVar, new DoubleConst((Double) node.cst)));
        } else if (node.cst instanceof String) {
            addInst(bb, instFactory.newAssignConst(tmpVar, new StringConst(node.cst.toString(), classNameFactory)));
        }
        stack.push(tmpVar);
    }

    public void visitLookupSwitchInsn(BasicBlock bb, int position, LookupSwitchInsnNode node, LabelManager labelManager) {
        addInst(bb, instFactory.newSwitch(stack.pop()));
    }

    public void visitMethodInsn(BasicBlock bb, int position, MethodInsnNode node) {
        // return type
        Type returnType = Type.getReturnType(node.desc);
        JavaType returnJavaType = JavaType.newType(returnType, classNameFactory);

        // argument types
        Type[] argTypes = Type.getArgumentTypes(node.desc);
        List<Variable> args = new ArrayList<Variable>(argTypes.length);
        List<JavaType> argJavaTypes = new ArrayList<JavaType>(argTypes.length);
        for (int i = 0; i < argTypes.length; i++) {
            args.add(0, stack.pop());
            argJavaTypes.add(0, JavaType.newType(argTypes[i], classNameFactory));
        }

        String methodName = node.name;

        MethodSignature signature = new MethodSignature(methodName, returnJavaType,
                                                        argJavaTypes);

        Variable resultVar = tmpVarFactory.create(bb);
        switch (node.getOpcode()) {
            case INVOKEVIRTUAL:
            case INVOKESPECIAL:
            case INVOKEINTERFACE:
            case INVOKEDYNAMIC: {
                Variable objVar = stack.pop();
                addInst(bb, instFactory.newCallMethod(resultVar, objVar, signature,
                                                         args));
                break;
            }

            case INVOKESTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                addInst(bb, instFactory.newCallStaticMethod(resultVar, className,
                                                               signature, args));
                break;
            }
        }
        if (returnType != Type.VOID_TYPE) {
            stack.push(resultVar);
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock bb, int position, MultiANewArrayInsnNode node) {
        Type type = Type.getType(node.desc).getElementType();
        JavaType javaType = JavaType.newType(type, classNameFactory);
        List<Variable> dimensions = new ArrayList<Variable>(node.dims);
        for (int i = 0; i < node.dims; i++) {
            dimensions.add(0, stack.pop());
        }
        Variable tmpResult = tmpVarFactory.create(bb);
        addInst(bb, instFactory.newNewArray(tmpResult, javaType, dimensions));
        stack.push(tmpResult);
    }

    public void visitTableSwitchInsn(BasicBlock bb, int position, TableSwitchInsnNode node, LabelManager labelManager) {
        addInst(bb, instFactory.newSwitch(stack.pop()));
    }

    public void visitTypeInsnInsn(BasicBlock bb, int position, TypeInsnNode node) {
        JavaType type = JavaType.newType(Type.getObjectType(node.desc), classNameFactory);

        switch (node.getOpcode()) {
            case NEW: {
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newNewObject(tmpResult, type));
                stack.push(tmpResult);
                break;
            }

            case ANEWARRAY: {
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newNewArray(tmpResult, type,
                                         Collections.singletonList(stack.pop())));
                stack.push(tmpResult);
                break;
            }

            case CHECKCAST:
                break;

            case INSTANCEOF: {
                Variable tmpResult = tmpVarFactory.create(bb);
                addInst(bb, instFactory.newInstanceOf(tmpResult, stack.pop(), type));
                stack.push(tmpResult);
                break;
            }
        }
    }

    public void visitVarInsn(BasicBlock bb, int position, VarInsnNode node) {
        switch (node.getOpcode()) {
            case ILOAD:
            case LLOAD:
            case FLOAD:
            case DLOAD:
            case ALOAD: {
                Variable tmpVar = tmpVarFactory.create(bb);
                stack.push(tmpVar);
                addInst(bb, instFactory.newAssignVar(tmpVar, new Variable(node.var, bb, position)));
                break;
            }

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                Variable var = stack.pop();
                addInst(bb, instFactory.newAssignVar(new Variable(node.var, bb, position), var));
                break;
            }

            case RET:
                throw new ABCDException("TODO : support RET instruction");
        }
    }

    public void after(BasicBlock bb) {
    }

}
