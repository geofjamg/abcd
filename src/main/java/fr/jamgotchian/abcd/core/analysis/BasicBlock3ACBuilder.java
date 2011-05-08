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
import fr.jamgotchian.abcd.core.tac.model.SetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.NewArrayInst;
import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.AssignInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryOp;
import fr.jamgotchian.abcd.core.tac.model.ByteConst;
import fr.jamgotchian.abcd.core.tac.model.CallMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.ClassConst;
import fr.jamgotchian.abcd.core.tac.model.DoubleConst;
import fr.jamgotchian.abcd.core.tac.model.FloatConst;
import fr.jamgotchian.abcd.core.tac.model.GetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.GetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GotoInst;
import fr.jamgotchian.abcd.core.tac.model.InstanceOfInst;
import fr.jamgotchian.abcd.core.tac.model.StaticField;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.IntConst;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.LabelInst;
import fr.jamgotchian.abcd.core.tac.model.LongConst;
import fr.jamgotchian.abcd.core.tac.model.MonitorEnterInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorExitInst;
import fr.jamgotchian.abcd.core.tac.model.NullConst;
import fr.jamgotchian.abcd.core.tac.model.NewObjectInst;
import fr.jamgotchian.abcd.core.tac.model.Operand;
import fr.jamgotchian.abcd.core.tac.model.ReturnInst;
import fr.jamgotchian.abcd.core.tac.model.SetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.ShortConst;
import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.SwitchInst;
import fr.jamgotchian.abcd.core.tac.model.LocalVariable;
import fr.jamgotchian.abcd.core.tac.model.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.tac.model.ThrowInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryOp;
import fr.jamgotchian.abcd.core.tac.model.Variable;
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

    protected final ArrayDeque<LocalVariable> stack;

    protected final TemporaryVariableFactory tmpVarFactory;

    BasicBlock3ACBuilder(ClassNameFactory classNameFactory,
                        TemporaryVariableFactory tmpVarFactory,
                        ArrayDeque<LocalVariable> stack) {
        this.classNameFactory = classNameFactory;
        this.tmpVarFactory = tmpVarFactory;
        this.stack = stack;
    }

    static void addInst(BasicBlock block, TACInst inst) {
        logger.log(Level.FINER, "Add inst : {0}", TACInstWriter.toText(inst));

        ((AnalysisData) block.getData()).getInstructions().add(inst);
    }

    protected void pushVar(LocalVariable var) {
        stack.push(var);
    }

    protected LocalVariable popVar() {
        return stack.pop().clone();
    }

    public void before(BasicBlock block) {
    }

    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {

        switch (node.getOpcode()) {
            case GETSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                String varName = node.name;
                Variable field = new StaticField(className, varName);
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, field));
                pushVar(tmpVar);
                break;
            }

            case PUTSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                String varName = node.name;
                Variable field = new StaticField(className, varName);
                Variable tmpVar = popVar();
                addInst(block, new AssignInst(field, tmpVar));
                break;
            }

            case GETFIELD: {
                LocalVariable resultVar = tmpVarFactory.create(block);
                LocalVariable objVar = popVar();
                String fieldName = node.name;
                addInst(block, new GetFieldInst(resultVar, objVar, fieldName));
                pushVar(resultVar);
                break;
            }

            case PUTFIELD: {
                LocalVariable valueVar = popVar();
                LocalVariable objVar = popVar();
                String fieldName = node.name;
                addInst(block, new SetFieldInst(objVar, fieldName, valueVar));
                break;
            }
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        Operand value = new IntConst(Math.abs(node.incr));
        Variable var = new LocalVariable(node.var, block);
        LocalVariable tmpVar = tmpVarFactory.create(block);
        addInst(block, new AssignInst(tmpVar, var));
        LocalVariable tmpValue = tmpVarFactory.create(block);
        addInst(block, new AssignInst(tmpValue, value));
        LocalVariable tmpResult = tmpVarFactory.create(block);
        addInst(block, new BinaryInst(tmpResult, node.incr > 0 ? BinaryOp.PLUS : BinaryOp.MINUS,
                               tmpVar, tmpValue));
        addInst(block, new AssignInst(var, tmpResult));
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new NullConst(classNameFactory)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_M1: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(1)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_0: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(0)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_1: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(1)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_2: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(2)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_3: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(3)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_4: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(4)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_5: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new IntConst(5)));
                pushVar(tmpVar);
                break;
            }

            case LCONST_0: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new LongConst(0)));
                pushVar(tmpVar);
                break;
            }

            case LCONST_1: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new LongConst(1)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_0: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new FloatConst(0f)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_1: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new FloatConst(1f)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_2: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new FloatConst(2f)));
                pushVar(tmpVar);
                break;
            }

            case DCONST_0: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new DoubleConst(0d)));
                pushVar(tmpVar);
                break;
            }

            case DCONST_1: {
                LocalVariable tmpVar = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpVar, new DoubleConst(1d)));
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
                LocalVariable arrayIndex = popVar();
                LocalVariable arrayVar = popVar();
                LocalVariable tmpResultVar = tmpVarFactory.create(block);
                addInst(block, new GetArrayInst(tmpResultVar, arrayVar, arrayIndex));
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
                LocalVariable valueVar = popVar();
                LocalVariable indexVar = popVar();
                LocalVariable arrayVar = popVar();
                addInst(block, new SetArrayInst(arrayVar, indexVar, valueVar));
                break;
            }

            case POP: {
                popVar();
                break;
            }

            case POP2:
                throw new ABCDException("TODO");

            case DUP: {
                LocalVariable var = popVar();
                pushVar(var);
                pushVar(var);
                break;
            }

            case DUP_X1: {
                LocalVariable var1 = popVar();
                LocalVariable var2 = popVar();
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
                LocalVariable var1 = popVar();
                LocalVariable var2 = popVar();
                pushVar(var1);
                pushVar(var2);
                break;
            }

            case IADD:
            case LADD:
            case FADD:
            case DADD: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.PLUS, left, right));
                pushVar(tmpResult);
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.MINUS, left, right));
                pushVar(tmpResult);
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.MUL, left, right));
                pushVar(tmpResult);
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.DIV, left, right));
                pushVar(tmpResult);
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.REMAINDER, left, right));
                pushVar(tmpResult);
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new UnaryInst(tmpResult, UnaryOp.MINUS, popVar()));
                pushVar(tmpResult);
                break;
            }

            case ISHL:
            case LSHL: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.SHIFT_LEFT, left, right));
                pushVar(tmpResult);
                break;
            }

            case ISHR:
            case LSHR: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.SHIFT_RIGHT, left, right));
                pushVar(tmpResult);
                break;
            }

            case IUSHR:
            case LUSHR: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.LOGICAL_SHIFT_RIGHT, left, right));
                pushVar(tmpResult);
                break;
            }

            case IAND:
            case LAND: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.AND, left, right));
                pushVar(tmpResult);
                break;
            }

            case IOR:
            case LOR: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.OR, left, right));
                pushVar(tmpResult);
                break;
            }

            case IXOR:
            case LXOR: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.XOR, left, right));
                pushVar(tmpResult);
                break;
            }

            case I2L: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.LONG));
                pushVar(tmpResult);
                break;
            }

            case I2F: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case I2D: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case L2I: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case L2F: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case L2D: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case F2I: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case F2L: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.LONG));
                break;
            }

            case F2D: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case D2I: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case D2L: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.LONG));
                pushVar(tmpResult);
                break;
            }

            case D2F: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case I2B: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.BYTE));
                pushVar(tmpResult);
                break;
            }

            case I2C: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.CHAR));
                pushVar(tmpResult);
                break;
            }

            case I2S: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                LocalVariable var = popVar();
                addInst(block, new CastInst(tmpResult, var, JavaType.SHORT));
                pushVar(tmpResult);
                break;
            }

            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG: {
                LocalVariable value2 = popVar();
                LocalVariable value1 = popVar();
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.MINUS, value1, value2));
                pushVar(tmpResult);
                break;
            }

            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                addInst(block, new ReturnInst(popVar()));
                break;

            case RETURN:
                addInst(block, new ReturnInst());
                break;

            case ARRAYLENGTH: {
                LocalVariable result = tmpVarFactory.create(block);
                LocalVariable arrayVar = popVar();
                addInst(block, new ArrayLengthInst(result, arrayVar));
                pushVar(result);
                break;
            }

            case ATHROW:
                addInst(block, new ThrowInst(popVar()));
                break;

            case MONITORENTER:
                addInst(block, new MonitorEnterInst(popVar()));
                break;

            case MONITOREXIT:
                addInst(block, new MonitorExitInst(popVar()));
                break;
        }
    }

    public void visitIntInsn(BasicBlock block, int index, IntInsnNode node) {
        LocalVariable tmpVar = tmpVarFactory.create(block);
        switch (node.getOpcode()) {
            case BIPUSH:
                addInst(block, new AssignInst(tmpVar, new ByteConst((byte) node.operand)));
                break;

            case SIPUSH:
                addInst(block, new AssignInst(tmpVar, new ShortConst((short) node.operand)));
                break;

            case NEWARRAY:
                addInst(block, new NewArrayInst(tmpVar, ATYPES[node.operand],
                                         Collections.singletonList(popVar())));
                break;
        }
        pushVar(tmpVar);
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {

        LocalVariable tmpResult = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                LocalVariable tmpZero = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.EQ, popVar(), tmpZero));
                break;
            }

            case IFNE: {
                LocalVariable tmpZero = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.NE, popVar(), tmpZero));
                break;
            }

            case IFLT: {
                LocalVariable tmpZero = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.LT, popVar(), tmpZero));
                break;
            }

            case IFGE: {
                LocalVariable tmpZero = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.GE, popVar(), tmpZero));
                break;
            }

            case IFGT: {
                LocalVariable tmpZero = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.GT, popVar(), tmpZero));
                break;
            }

            case IFLE: {
                LocalVariable tmpZero = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpZero, new IntConst(0)));
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.LE, popVar(), tmpZero));
                break;
            }

            case IF_ICMPEQ: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ICMPNE: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case IF_ICMPLT: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.LT, left, right));
                break;
            }

            case IF_ICMPGE: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.GE, left, right));
                break;
            }

            case IF_ICMPGT: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.GT, left, right));
                break;
            }

            case IF_ICMPLE: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.LE, left, right));
                break;
            }

            case IF_ACMPEQ: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ACMPNE: {
                LocalVariable right = popVar();
                LocalVariable left = popVar();
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                LocalVariable tmpNull = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpNull, new NullConst(classNameFactory)));
                pushVar(tmpNull);
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.EQ, popVar(), tmpNull));
                break;
            }

            case IFNONNULL: {
                LocalVariable tmpNull = tmpVarFactory.create(block);
                addInst(block, new AssignInst(tmpNull, new NullConst(classNameFactory)));
                pushVar(tmpNull);
                tmpResult = tmpVarFactory.create(block);
                addInst(block, new BinaryInst(tmpResult, BinaryOp.NE, popVar(), tmpNull));
                break;
            }
        }

        Label label = block.getGraph().getLabelManager().getLabel(node.label);

        if (tmpResult != null) {
            addInst(block, new JumpIfInst(tmpResult, label));
        } else {
            addInst(block, new GotoInst(label));
        }
    }

    public void visitLabel(BasicBlock block, int index, LabelNode node) {
        Label label = block.getGraph().getLabelManager().getLabel(node);
        addInst(block, new LabelInst(label));
    }

    public void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node) {
        LocalVariable tmpVar = tmpVarFactory.create(block);
        if (node.cst instanceof Type) {
            ClassName className = classNameFactory.newClassName(((Type)node.cst).getClassName());
            addInst(block, new AssignInst(tmpVar, new ClassConst(className, classNameFactory)));
        } else if (node.cst instanceof Integer) {
            addInst(block, new AssignInst(tmpVar, new IntConst((Integer) node.cst)));
        } else if (node.cst instanceof Long) {
            addInst(block, new AssignInst(tmpVar, new LongConst((Long) node.cst)));
        } else if (node.cst instanceof Float) {
            addInst(block, new AssignInst(tmpVar, new FloatConst((Float) node.cst)));
        } else if (node.cst instanceof Double) {
            addInst(block, new AssignInst(tmpVar, new DoubleConst((Double) node.cst)));
        } else if (node.cst instanceof String) {
            addInst(block, new AssignInst(tmpVar, new StringConst(node.cst.toString(), classNameFactory)));
        }
        pushVar(tmpVar);
    }

    public void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(block, new SwitchInst(popVar(), labels));
    }

    public void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node) {
        int argCount = Type.getArgumentTypes(node.desc).length;
        List<LocalVariable> args = new ArrayList<LocalVariable>(argCount);
        for (int i = 0; i < argCount; i++) {
            args.add(0, popVar());
        }

        String methodName = node.name;
        LocalVariable resultVar = tmpVarFactory.create(block);
        switch (node.getOpcode()) {
            case INVOKEVIRTUAL:
            case INVOKESPECIAL:
            case INVOKEINTERFACE:
            case INVOKEDYNAMIC: {
                LocalVariable objVar = popVar();
                addInst(block, new CallMethodInst(resultVar, objVar, methodName, args));
                break;
            }

            case INVOKESTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                addInst(block, new CallStaticMethodInst(resultVar, className, methodName, args));
                break;
            }
        }
        Type returnType = Type.getReturnType(node.desc);
        if (returnType != Type.VOID_TYPE) {
            pushVar(resultVar);
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node) {
        Type type = Type.getType(node.desc).getElementType();
        JavaType javaType = JavaType.newType(type, classNameFactory);
        List<LocalVariable> dimensions = new ArrayList<LocalVariable>(node.dims);
        for (int i = 0; i < node.dims; i++) {
            dimensions.add(0, popVar());
        }
        LocalVariable tmpResult = tmpVarFactory.create(block);
        addInst(block, new NewArrayInst(tmpResult, javaType, dimensions));
        pushVar(tmpResult);
    }

    public void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(block, new SwitchInst(popVar(), labels));
    }

    public void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node) {
        ClassName className
                = classNameFactory.newClassName(Type.getObjectType(node.desc).getClassName());

        switch (node.getOpcode()) {
            case NEW: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new NewObjectInst(tmpResult, className));
                pushVar(tmpResult);
                break;
            }

            case ANEWARRAY: {
                JavaType type = JavaType.newRefType(className);
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new NewArrayInst(tmpResult, type,
                                         Collections.singletonList(popVar())));
                pushVar(tmpResult);
                break;
            }

            case CHECKCAST:
                break;

            case INSTANCEOF: {
                LocalVariable tmpResult = tmpVarFactory.create(block);
                addInst(block, new InstanceOfInst(tmpResult, popVar(), className));
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
                LocalVariable tmpVar = tmpVarFactory.create(block);
                pushVar(tmpVar);
                addInst(block, new AssignInst(tmpVar, new LocalVariable(node.var, block)));
                break;
            }

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                Variable var = popVar();
                addInst(block, new AssignInst(new LocalVariable(node.var, block), var));
                break;
            }

            case RET:
                throw new ABCDException("TODO : support RET instruction");
        }
    }

    public void after(BasicBlock block) {
    }

}
