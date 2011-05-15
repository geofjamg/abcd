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
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.logging.Level;
import java.util.logging.Logger;
import fr.jamgotchian.abcd.core.common.Label;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockVisitor;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.tac.model.BinaryOp;
import fr.jamgotchian.abcd.core.tac.model.ByteConst;
import fr.jamgotchian.abcd.core.tac.model.ClassConst;
import fr.jamgotchian.abcd.core.tac.model.Const;
import fr.jamgotchian.abcd.core.tac.model.DoubleConst;
import fr.jamgotchian.abcd.core.tac.model.FloatConst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.IntConst;
import fr.jamgotchian.abcd.core.tac.model.LongConst;
import fr.jamgotchian.abcd.core.tac.model.NullConst;
import fr.jamgotchian.abcd.core.tac.model.ShortConst;
import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.model.TACInstFactory;
import fr.jamgotchian.abcd.core.tac.model.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.tac.model.UnaryOp;
import fr.jamgotchian.abcd.core.tac.util.TACInstWriter;
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
class BasicBlock3ACBuilder implements BasicBlockVisitor {

    private static final Logger logger = Logger.getLogger(BasicBlock3ACBuilder.class.getName());

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

    protected final ClassNameFactory classNameFactory;

    protected final ArrayDeque<Variable> stack;

    protected final TemporaryVariableFactory tmpVarFactory;

    protected final TACInstFactory instFactory;

    BasicBlock3ACBuilder(ClassNameFactory classNameFactory,
                        TemporaryVariableFactory tmpVarFactory,
                        ArrayDeque<Variable> stack,
                        TACInstFactory instFactory) {
        this.classNameFactory = classNameFactory;
        this.tmpVarFactory = tmpVarFactory;
        this.stack = stack;
        this.instFactory = instFactory;
    }

    static void addInst(BasicBlock block, TACInst inst) {
        logger.log(Level.FINER, "Add inst : {0}", TACInstWriter.toText(inst));

        ((AnalysisData) block.getData()).getInstructions().add(inst);
    }

    protected void pushVar(Variable var) {
        stack.push(var);
    }

    protected Variable popVar() {
        return stack.pop().clone();
    }

    public void before(BasicBlock block) {
    }

    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {

        switch (node.getOpcode()) {
            case GETSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                String varName = node.name;
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newGetStaticField(tmpVar, className, varName));
                pushVar(tmpVar);
                break;
            }

            case PUTSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                String varName = node.name;
                Variable tmpVar = popVar();
                addInst(block, instFactory.newSetStaticField(className, varName, tmpVar));
                break;
            }

            case GETFIELD: {
                Variable resultVar = tmpVarFactory.create(block);
                Variable objVar = popVar();
                String fieldName = node.name;
                addInst(block, instFactory.newGetField(resultVar, objVar, fieldName));
                pushVar(resultVar);
                break;
            }

            case PUTFIELD: {
                Variable valueVar = popVar();
                Variable objVar = popVar();
                String fieldName = node.name;
                addInst(block, instFactory.newSetField(objVar, fieldName, valueVar));
                break;
            }
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        Const value = new IntConst(Math.abs(node.incr));
        Variable var = new Variable(node.var, block);
        Variable tmpVar = tmpVarFactory.create(block);
        addInst(block, instFactory.newAssignVar(tmpVar, var));
        Variable tmpValue = tmpVarFactory.create(block);
        addInst(block, instFactory.newAssignConst(tmpValue, value));
        Variable tmpResult = tmpVarFactory.create(block);
        addInst(block, instFactory.newBinary(tmpResult, node.incr > 0 ? BinaryOp.PLUS : BinaryOp.MINUS,
                                             tmpVar, tmpValue));
        addInst(block, instFactory.newAssignVar(var, tmpResult));
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new NullConst(classNameFactory)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_M1: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(1)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_0: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(0)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_1: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(1)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_2: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(2)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_3: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(3)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_4: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(4)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_5: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new IntConst(5)));
                pushVar(tmpVar);
                break;
            }

            case LCONST_0: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new LongConst(0)));
                pushVar(tmpVar);
                break;
            }

            case LCONST_1: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new LongConst(1)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_0: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new FloatConst(0f)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_1: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new FloatConst(1f)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_2: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new FloatConst(2f)));
                pushVar(tmpVar);
                break;
            }

            case DCONST_0: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new DoubleConst(0d)));
                pushVar(tmpVar);
                break;
            }

            case DCONST_1: {
                Variable tmpVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpVar, new DoubleConst(1d)));
                pushVar(tmpVar);
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
                Variable arrayIndex = popVar();
                Variable arrayVar = popVar();
                Variable tmpResultVar = tmpVarFactory.create(block);
                addInst(block, instFactory.newGetArray(tmpResultVar, arrayVar, arrayIndex));
                pushVar(tmpResultVar);
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
                Variable valueVar = popVar();
                Variable indexVar = popVar();
                Variable arrayVar = popVar();
                addInst(block, instFactory.newSetArray(arrayVar, indexVar, valueVar));
                break;
            }

            case POP: {
                popVar();
                break;
            }

            case POP2:
                throw new ABCDException("TODO");

            case DUP: {
                Variable var = popVar();
                pushVar(var);
                pushVar(var);
                break;
            }

            case DUP_X1: {
                Variable var1 = popVar();
                Variable var2 = popVar();
                pushVar(var1);
                pushVar(var2);
                pushVar(var1);
                break;
            }

            case DUP_X2:
            case DUP2_X1:
            case DUP2_X2:
                throw new ABCDException("TODO");

            case SWAP: {
                Variable var1 = popVar();
                Variable var2 = popVar();
                pushVar(var1);
                pushVar(var2);
                break;
            }

            case IADD:
            case LADD:
            case FADD:
            case DADD: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.PLUS, left, right));
                pushVar(tmpResult);
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.MINUS, left, right));
                pushVar(tmpResult);
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.MUL, left, right));
                pushVar(tmpResult);
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.DIV, left, right));
                pushVar(tmpResult);
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.REMAINDER, left, right));
                pushVar(tmpResult);
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG: {
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newUnary(tmpResult, UnaryOp.MINUS, popVar()));
                pushVar(tmpResult);
                break;
            }

            case ISHL:
            case LSHL: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.SHIFT_LEFT, left, right));
                pushVar(tmpResult);
                break;
            }

            case ISHR:
            case LSHR: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.SHIFT_RIGHT, left, right));
                pushVar(tmpResult);
                break;
            }

            case IUSHR:
            case LUSHR: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.LOGICAL_SHIFT_RIGHT, left, right));
                pushVar(tmpResult);
                break;
            }

            case IAND:
            case LAND: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.AND, left, right));
                pushVar(tmpResult);
                break;
            }

            case IOR:
            case LOR: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.OR, left, right));
                pushVar(tmpResult);
                break;
            }

            case IXOR:
            case LXOR: {
                Variable right = popVar();
                Variable left = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.XOR, left, right));
                pushVar(tmpResult);
                break;
            }

            case I2L: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.LONG));
                pushVar(tmpResult);
                break;
            }

            case I2F: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case I2D: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case L2I: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case L2F: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case L2D: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case F2I: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case F2L: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.LONG));
                break;
            }

            case F2D: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case D2I: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case D2L: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.LONG));
                pushVar(tmpResult);
                break;
            }

            case D2F: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case I2B: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.BYTE));
                pushVar(tmpResult);
                break;
            }

            case I2C: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.CHAR));
                pushVar(tmpResult);
                break;
            }

            case I2S: {
                Variable tmpResult = tmpVarFactory.create(block);
                Variable var = popVar();
                addInst(block, instFactory.newCast(tmpResult, var, JavaType.SHORT));
                pushVar(tmpResult);
                break;
            }

            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG: {
                Variable value2 = popVar();
                Variable value1 = popVar();
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.MINUS, value1, value2));
                pushVar(tmpResult);
                break;
            }

            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                addInst(block, instFactory.newReturn(popVar()));
                break;

            case RETURN:
                addInst(block, instFactory.newReturn());
                break;

            case ARRAYLENGTH: {
                Variable result = tmpVarFactory.create(block);
                Variable arrayVar = popVar();
                addInst(block, instFactory.newArrayLength(result, arrayVar));
                pushVar(result);
                break;
            }

            case ATHROW:
                addInst(block, instFactory.newThrow(popVar()));
                break;

            case MONITORENTER:
                addInst(block, instFactory.newMonitorEnter(popVar()));
                break;

            case MONITOREXIT:
                addInst(block, instFactory.newMonitorExit(popVar()));
                break;
        }
    }

    public void visitIntInsn(BasicBlock block, int index, IntInsnNode node) {
        Variable tmpVar = tmpVarFactory.create(block);
        switch (node.getOpcode()) {
            case BIPUSH:
                addInst(block, instFactory.newAssignConst(tmpVar, new ByteConst((byte) node.operand)));
                break;

            case SIPUSH:
                addInst(block, instFactory.newAssignConst(tmpVar, new ShortConst((short) node.operand)));
                break;

            case NEWARRAY:
                addInst(block, instFactory.newNewArray(tmpVar, ATYPES[node.operand],
                                                       Collections.singletonList(popVar())));
                break;
        }
        pushVar(tmpVar);
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {

        Variable tmpResult = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                Variable tmpZero = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.EQ, popVar(), tmpZero));
                break;
            }

            case IFNE: {
                Variable tmpZero = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.NE, popVar(), tmpZero));
                break;
            }

            case IFLT: {
                Variable tmpZero = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.LT, popVar(), tmpZero));
                break;
            }

            case IFGE: {
                Variable tmpZero = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.GE, popVar(), tmpZero));
                break;
            }

            case IFGT: {
                Variable tmpZero = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.GT, popVar(), tmpZero));
                break;
            }

            case IFLE: {
                Variable tmpZero = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.LE, popVar(), tmpZero));
                break;
            }

            case IF_ICMPEQ: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ICMPNE: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case IF_ICMPLT: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.LT, left, right));
                break;
            }

            case IF_ICMPGE: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.GE, left, right));
                break;
            }

            case IF_ICMPGT: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.GT, left, right));
                break;
            }

            case IF_ICMPLE: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.LE, left, right));
                break;
            }

            case IF_ACMPEQ: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ACMPNE: {
                Variable right = popVar();
                Variable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                Variable tmpNull = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpNull, new NullConst(classNameFactory)));
                pushVar(tmpNull);
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.EQ, popVar(), tmpNull));
                break;
            }

            case IFNONNULL: {
                Variable tmpNull = tmpVarFactory.create(block);
                addInst(block, instFactory.newAssignConst(tmpNull, new NullConst(classNameFactory)));
                pushVar(tmpNull);
                tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newBinary(tmpResult, BinaryOp.NE, popVar(), tmpNull));
                break;
            }
        }

        Label label = block.getGraph().getLabelManager().getLabel(node.label);

        if (tmpResult != null) {
            addInst(block, instFactory.newJumpIf(tmpResult, label));
        } else {
            addInst(block, instFactory.newGoto(label));
        }
    }

    public void visitLabel(BasicBlock block, int index, LabelNode node) {
        Label label = block.getGraph().getLabelManager().getLabel(node);
        addInst(block, instFactory.newLabel(label));
    }

    public void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node) {
        Variable tmpVar = tmpVarFactory.create(block);
        if (node.cst instanceof Type) {
            ClassName className = classNameFactory.newClassName(((Type)node.cst).getClassName());
            addInst(block, instFactory.newAssignConst(tmpVar, new ClassConst(className, classNameFactory)));
        } else if (node.cst instanceof Integer) {
            addInst(block, instFactory.newAssignConst(tmpVar, new IntConst((Integer) node.cst)));
        } else if (node.cst instanceof Long) {
            addInst(block, instFactory.newAssignConst(tmpVar, new LongConst((Long) node.cst)));
        } else if (node.cst instanceof Float) {
            addInst(block, instFactory.newAssignConst(tmpVar, new FloatConst((Float) node.cst)));
        } else if (node.cst instanceof Double) {
            addInst(block, instFactory.newAssignConst(tmpVar, new DoubleConst((Double) node.cst)));
        } else if (node.cst instanceof String) {
            addInst(block, instFactory.newAssignConst(tmpVar, new StringConst(node.cst.toString(), classNameFactory)));
        }
        pushVar(tmpVar);
    }

    public void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(block, instFactory.newSwitch(popVar(), labels));
    }

    public void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node) {
        // return type
        Type returnType = Type.getReturnType(node.desc);
        JavaType returnJavaType = JavaType.newType(returnType, classNameFactory);

        // argument types
        Type[] argTypes = Type.getArgumentTypes(node.desc);
        List<Variable> args = new ArrayList<Variable>(argTypes.length);
        List<JavaType> argJavaTypes = new ArrayList<JavaType>(argTypes.length);
        for (int i = 0; i < argTypes.length; i++) {
            args.add(0, popVar());
            argJavaTypes.add(0, JavaType.newType(argTypes[i], classNameFactory));
        }

        String methodName = node.name;
        Variable resultVar = tmpVarFactory.create(block);
        switch (node.getOpcode()) {
            case INVOKEVIRTUAL:
            case INVOKESPECIAL:
            case INVOKEINTERFACE:
            case INVOKEDYNAMIC: {
                Variable objVar = popVar();
                addInst(block, instFactory.newCallMethod(resultVar, objVar, methodName,
                                                  returnJavaType, argJavaTypes,
                                                  args));
                break;
            }

            case INVOKESTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                addInst(block, instFactory.newCallStaticMethod(resultVar, className, methodName,
                                                        returnJavaType, argJavaTypes, args));
                break;
            }
        }
        if (returnType != Type.VOID_TYPE) {
            pushVar(resultVar);
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node) {
        Type type = Type.getType(node.desc).getElementType();
        JavaType javaType = JavaType.newType(type, classNameFactory);
        List<Variable> dimensions = new ArrayList<Variable>(node.dims);
        for (int i = 0; i < node.dims; i++) {
            dimensions.add(0, popVar());
        }
        Variable tmpResult = tmpVarFactory.create(block);
        addInst(block, instFactory.newNewArray(tmpResult, javaType, dimensions));
        pushVar(tmpResult);
    }

    public void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(block, instFactory.newSwitch(popVar(), labels));
    }

    public void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node) {
        ClassName className
                = classNameFactory.newClassName(Type.getObjectType(node.desc).getClassName());

        switch (node.getOpcode()) {
            case NEW: {
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newNewObject(tmpResult, className));
                pushVar(tmpResult);
                break;
            }

            case ANEWARRAY: {
                JavaType type = JavaType.newRefType(className);
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newNewArray(tmpResult, type,
                                         Collections.singletonList(popVar())));
                pushVar(tmpResult);
                break;
            }

            case CHECKCAST:
                break;

            case INSTANCEOF: {
                Variable tmpResult = tmpVarFactory.create(block);
                addInst(block, instFactory.newInstanceOf(tmpResult, popVar(), className));
                pushVar(tmpResult);
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
            case ALOAD: {
                Variable tmpVar = tmpVarFactory.create(block);
                pushVar(tmpVar);
                addInst(block, instFactory.newAssignVar(tmpVar, new Variable(node.var, block)));
                break;
            }

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                Variable var = popVar();
                addInst(block, instFactory.newAssignVar(new Variable(node.var, block), var));
                break;
            }

            case RET:
                throw new ABCDException("TODO : support RET instruction");
        }
    }

    public void after(BasicBlock block) {
    }

}
