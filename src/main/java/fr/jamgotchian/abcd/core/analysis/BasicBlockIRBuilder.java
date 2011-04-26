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
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.output.CodeWriter;
import fr.jamgotchian.abcd.core.output.TextCodeWriter;
import fr.jamgotchian.abcd.core.ir.SetArrayInst;
import fr.jamgotchian.abcd.core.ir.NewArrayInst;
import fr.jamgotchian.abcd.core.ir.ArrayLengthInst;
import fr.jamgotchian.abcd.core.ir.AssignInst;
import fr.jamgotchian.abcd.core.ir.BinaryInst;
import fr.jamgotchian.abcd.core.ir.BinaryOp;
import fr.jamgotchian.abcd.core.ir.ByteConst;
import fr.jamgotchian.abcd.core.ir.CallMethodInst;
import fr.jamgotchian.abcd.core.ir.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.ir.CastInst;
import fr.jamgotchian.abcd.core.ir.ClassConst;
import fr.jamgotchian.abcd.core.ir.DoubleConst;
import fr.jamgotchian.abcd.core.ir.FloatConst;
import fr.jamgotchian.abcd.core.ir.GetArrayInst;
import fr.jamgotchian.abcd.core.ir.GetFieldInst;
import fr.jamgotchian.abcd.core.ir.GotoInst;
import fr.jamgotchian.abcd.core.ir.IRInstWriter;
import fr.jamgotchian.abcd.core.ir.InstanceOfInst;
import fr.jamgotchian.abcd.core.ir.StaticField;
import fr.jamgotchian.abcd.core.ir.IRInst;
import fr.jamgotchian.abcd.core.ir.IntConst;
import fr.jamgotchian.abcd.core.ir.JumpIfInst;
import fr.jamgotchian.abcd.core.ir.LabelInst;
import fr.jamgotchian.abcd.core.ir.LocalVariable;
import fr.jamgotchian.abcd.core.ir.LongConst;
import fr.jamgotchian.abcd.core.ir.MonitorEnterInst;
import fr.jamgotchian.abcd.core.ir.MonitorExitInst;
import fr.jamgotchian.abcd.core.ir.NullConst;
import fr.jamgotchian.abcd.core.ir.NewObjectInst;
import fr.jamgotchian.abcd.core.ir.Operand;
import fr.jamgotchian.abcd.core.ir.ReturnInst;
import fr.jamgotchian.abcd.core.ir.SetFieldInst;
import fr.jamgotchian.abcd.core.ir.ShortConst;
import fr.jamgotchian.abcd.core.ir.StringConst;
import fr.jamgotchian.abcd.core.ir.SwitchInst;
import fr.jamgotchian.abcd.core.ir.TemporaryVariable;
import fr.jamgotchian.abcd.core.ir.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.ir.ThrowInst;
import fr.jamgotchian.abcd.core.ir.UnaryInst;
import fr.jamgotchian.abcd.core.ir.UnaryOp;
import fr.jamgotchian.abcd.core.ir.Variable;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayDeque;
import java.util.Collections;
import java.util.Deque;
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
class BasicBlockIRBuilder implements BasicBlockVisitor {

    private static final Logger logger = Logger.getLogger(BasicBlockIRBuilder.class.getName());

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

    protected final Deque<TemporaryVariable> stack;

    protected final List<IRInst> insts;

    protected final TemporaryVariableFactory tmpVarFactory;

    public BasicBlockIRBuilder(ClassNameFactory classNameFactory,
                               TemporaryVariableFactory tmpVarFactory) {
        this.classNameFactory = classNameFactory;
        this.tmpVarFactory = tmpVarFactory;
        stack = new ArrayDeque<TemporaryVariable>();
        insts = new ArrayList<IRInst>();
    }

    protected void addInst(IRInst inst) {
        Writer writer = new StringWriter();
        try {
            CodeWriter codeWriter = new TextCodeWriter(writer);
            inst.accept(new IRInstWriter(codeWriter), null);
        } finally {
            try {
                writer.close();
            } catch (IOException e) {
                logger.log(Level.SEVERE, e.toString(), e);
            }
        }
        logger.log(Level.FINER, "Add inst : {0}", writer.toString());

        insts.add(inst);
    }

    protected void pushVar(TemporaryVariable var) {
        stack.push(var);
    }

    protected TemporaryVariable popVar() {
        return stack.pop();
    }

    public void before(BasicBlock block) {
    }

    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {

        switch (node.getOpcode()) {
            case GETSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                String varName = node.name;
                Variable field = new StaticField(className, varName);
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, field));
                pushVar(tmpVar);
                break;
            }

            case PUTSTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                String varName = node.name;
                Variable field = new StaticField(className, varName);
                Variable tmpVar = popVar();
                addInst(new AssignInst(field, tmpVar));
                break;
            }

            case GETFIELD: {
                TemporaryVariable resultVar = tmpVarFactory.create();
                TemporaryVariable objVar = popVar();
                String fieldName = node.name;
                addInst(new GetFieldInst(resultVar, objVar, fieldName));
                pushVar(resultVar);
                break;
            }

            case PUTFIELD: {
                TemporaryVariable valueVar = popVar();
                TemporaryVariable objVar = popVar();
                String fieldName = node.name;
                addInst(new SetFieldInst(objVar, fieldName, valueVar));
                break;
            }
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        Operand value = new IntConst(Math.abs(node.incr));
        Variable var = new LocalVariable(node.var);
        TemporaryVariable tmpVar = tmpVarFactory.create();
        addInst(new AssignInst(tmpVar, value));
        TemporaryVariable tmpValue = tmpVarFactory.create();
        addInst(new AssignInst(tmpValue, value));
        TemporaryVariable tmpResult = tmpVarFactory.create();
        addInst(new BinaryInst(tmpResult, node.incr > 0 ? BinaryOp.PLUS : BinaryOp.MINUS,
                               tmpVar, tmpValue));
        addInst(new AssignInst(var, tmpResult));
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new NullConst()));
                pushVar(tmpVar);
                break;
            }

            case ICONST_M1: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(1)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_0: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(0)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_1: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(1)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_2: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(2)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_3: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(3)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_4: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(4)));
                pushVar(tmpVar);
                break;
            }

            case ICONST_5: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(5)));
                pushVar(tmpVar);
                break;
            }

            case LCONST_0: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new LongConst(0)));
                pushVar(tmpVar);
                break;
            }

            case LCONST_1: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new LongConst(1)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_0: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new FloatConst(0f)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_1: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new FloatConst(1f)));
                pushVar(tmpVar);
                break;
            }

            case FCONST_2: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new FloatConst(2f)));
                pushVar(tmpVar);
                break;
            }

            case DCONST_0: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new DoubleConst(0d)));
                pushVar(tmpVar);
                break;
            }

            case DCONST_1: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new DoubleConst(1d)));
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
                TemporaryVariable tmpResultVar = tmpVarFactory.create();
                addInst(new GetArrayInst(tmpResultVar, arrayVar, arrayIndex));
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
                TemporaryVariable valueVar = popVar();
                TemporaryVariable indexVar = popVar();
                TemporaryVariable arrayVar = popVar();
                addInst(new SetArrayInst(arrayVar, indexVar, valueVar));
                break;
            }

            case POP: {
                popVar();
                break;
            }

            case POP2:
                throw new ABCDException("TODO");

            case DUP: {
                TemporaryVariable var = popVar();
                pushVar(var);
                pushVar(var);
                break;
            }

            case DUP_X1: {
                TemporaryVariable var1 = popVar();
                TemporaryVariable var2 = popVar();
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
                TemporaryVariable var1 = popVar();
                TemporaryVariable var2 = popVar();
                pushVar(var1);
                pushVar(var2);
                break;
            }

            case IADD:
            case LADD:
            case FADD:
            case DADD: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.PLUS, left, right));
                pushVar(tmpResult);
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.MINUS, left, right));
                pushVar(tmpResult);
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.MUL, left, right));
                pushVar(tmpResult);
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.DIV, left, right));
                pushVar(tmpResult);
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.REMAINDER, left, right));
                pushVar(tmpResult);
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new UnaryInst(tmpResult, UnaryOp.MINUS, popVar()));
                pushVar(tmpResult);
                break;
            }

            case ISHL:
            case LSHL: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.SHIFT_LEFT, left, right));
                pushVar(tmpResult);
                break;
            }

            case ISHR:
            case LSHR: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.SHIFT_RIGHT, left, right));
                pushVar(tmpResult);
                break;
            }

            case IUSHR:
            case LUSHR: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LOGICAL_SHIFT_RIGHT, left, right));
                pushVar(tmpResult);
                break;
            }

            case IAND:
            case LAND: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.AND, left, right));
                pushVar(tmpResult);
                break;
            }

            case IOR:
            case LOR: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.OR, left, right));
                pushVar(tmpResult);
                break;
            }

            case IXOR:
            case LXOR: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.XOR, left, right));
                pushVar(tmpResult);
                break;
            }

            case I2L: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.LONG));
                pushVar(tmpResult);
                break;
            }

            case I2F: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case I2D: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case L2I: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case L2F: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case L2D: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case F2I: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case F2L: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.LONG));
                break;
            }

            case F2D: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.DOUBLE));
                pushVar(tmpResult);
                break;
            }

            case D2I: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.INT));
                pushVar(tmpResult);
                break;
            }

            case D2L: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.LONG));
                pushVar(tmpResult);
                break;
            }

            case D2F: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.FLOAT));
                pushVar(tmpResult);
                break;
            }

            case I2B: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.BYTE));
                pushVar(tmpResult);
                break;
            }

            case I2C: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.CHAR));
                pushVar(tmpResult);
                break;
            }

            case I2S: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                TemporaryVariable var = popVar();
                addInst(new CastInst(tmpResult, var, JavaType.SHORT));
                pushVar(tmpResult);
                break;
            }

            case LCMP:
            case FCMPL:
            case FCMPG:
            case DCMPL:
            case DCMPG: {
                TemporaryVariable value2 = popVar();
                TemporaryVariable value1 = popVar();
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.MINUS, value1, value2));
                pushVar(tmpResult);
                break;
            }

            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                addInst(new ReturnInst(popVar()));
                break;

            case RETURN:
                addInst(new ReturnInst());
                break;

            case ARRAYLENGTH: {
                TemporaryVariable tmpVar = tmpVarFactory.create();
                TemporaryVariable arrayVar = popVar();
                addInst(new ArrayLengthInst(tmpVar, arrayVar));
                break;
            }

            case ATHROW:
                addInst(new ThrowInst(popVar()));
                break;

            case MONITORENTER:
                addInst(new MonitorEnterInst(popVar()));
                break;

            case MONITOREXIT:
                addInst(new MonitorExitInst(popVar()));
                break;
        }
    }

    public void visitIntInsn(BasicBlock block, int index, IntInsnNode node) {
        TemporaryVariable tmpVar = tmpVarFactory.create();
        switch (node.getOpcode()) {
            case BIPUSH:
                addInst(new AssignInst(tmpVar, new ByteConst((byte) node.operand)));
                break;

            case SIPUSH:
                addInst(new AssignInst(tmpVar, new ShortConst((short) node.operand)));
                break;

            case NEWARRAY:
                addInst(new NewArrayInst(tmpVar, ATYPES[node.operand],
                                         Collections.singletonList(popVar())));
                break;
        }
        pushVar(tmpVar);
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {

        TemporaryVariable tmpResult = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                TemporaryVariable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                pushVar(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, popVar(), tmpZero));
                break;
            }

            case IFNE: {
                TemporaryVariable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                pushVar(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, popVar(), tmpZero));
                break;
            }

            case IFLT: {
                TemporaryVariable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                pushVar(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LT, popVar(), tmpZero));
                break;
            }

            case IFGE: {
                TemporaryVariable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                pushVar(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GE, popVar(), tmpZero));
                break;
            }

            case IFGT: {
                TemporaryVariable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                pushVar(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GT, popVar(), tmpZero));
                break;
            }

            case IFLE: {
                TemporaryVariable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                pushVar(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LE, popVar(), tmpZero));
                break;
            }

            case IF_ICMPEQ: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ICMPNE: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case IF_ICMPLT: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LT, left, right));
                break;
            }

            case IF_ICMPGE: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GE, left, right));
                break;
            }

            case IF_ICMPGT: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GT, left, right));
                break;
            }

            case IF_ICMPLE: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LE, left, right));
                break;
            }

            case IF_ACMPEQ: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ACMPNE: {
                TemporaryVariable right = popVar();
                TemporaryVariable left = popVar();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                TemporaryVariable tmpNull = tmpVarFactory.create();
                addInst(new AssignInst(tmpNull, new NullConst()));
                pushVar(tmpNull);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, popVar(), tmpNull));
                break;
            }

            case IFNONNULL: {
                TemporaryVariable tmpNull = tmpVarFactory.create();
                addInst(new AssignInst(tmpNull, new NullConst()));
                pushVar(tmpNull);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, popVar(), tmpNull));
                break;
            }
        }

        Label label = block.getGraph().getLabelManager().getLabel(node.label);

        if (tmpResult != null) {
            addInst(new JumpIfInst(tmpResult, label));
            pushVar(tmpResult);
        } else {
            addInst(new GotoInst(label));
        }
    }

    public void visitLabel(BasicBlock block, int index, LabelNode node) {
        Label label = block.getGraph().getLabelManager().getLabel(node);
        addInst(new LabelInst(label));
    }

    public void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node) {
        TemporaryVariable tmpVar = tmpVarFactory.create();
        if (node.cst instanceof Type) {
            ClassName className = classNameFactory.newClassName(((Type)node.cst).getClassName());
            addInst(new AssignInst(tmpVar, new ClassConst(className)));
        } else if (node.cst instanceof Integer) {
            addInst(new AssignInst(tmpVar, new IntConst((Integer) node.cst)));
        } else if (node.cst instanceof Long) {
            addInst(new AssignInst(tmpVar, new LongConst((Long) node.cst)));
        } else if (node.cst instanceof Float) {
            addInst(new AssignInst(tmpVar, new FloatConst((Float) node.cst)));
        } else if (node.cst instanceof Double) {
            addInst(new AssignInst(tmpVar, new DoubleConst((Double) node.cst)));
        } else if (node.cst instanceof String) {
            addInst(new AssignInst(tmpVar, new StringConst(node.cst.toString())));
        }
        pushVar(tmpVar);
    }

    public void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(new SwitchInst(popVar(), labels));
    }

    public void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node) {
        int argCount = Type.getArgumentTypes(node.desc).length;
        List<TemporaryVariable> args = new ArrayList<TemporaryVariable>(argCount);
        for (int i = 0; i < argCount; i++) {
            args.add(0, popVar());
        }

        String methodName = node.name;
        TemporaryVariable resultVar = tmpVarFactory.create();
        switch (node.getOpcode()) {
            case INVOKEVIRTUAL:
            case INVOKESPECIAL:
            case INVOKEINTERFACE:
            case INVOKEDYNAMIC: {
                TemporaryVariable objVar = popVar();
                addInst(new CallMethodInst(resultVar, objVar, methodName, args));
                break;
            }

            case INVOKESTATIC: {
                ClassName className = classNameFactory.newClassName(node.owner.replace('/', '.'));
                addInst(new CallStaticMethodInst(resultVar, className, methodName, args));
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
        List<TemporaryVariable> dimensions = new ArrayList<TemporaryVariable>(node.dims);
        for (int i = 0; i < node.dims; i++) {
            dimensions.add(0, popVar());
        }
        TemporaryVariable tmpResult = tmpVarFactory.create();
        addInst(new NewArrayInst(tmpResult, javaType, dimensions));
        pushVar(tmpResult);
    }

    public void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(new SwitchInst(popVar(), labels));
    }

    public void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node) {
        ClassName className
                = classNameFactory.newClassName(Type.getObjectType(node.desc).getClassName());

        switch (node.getOpcode()) {
            case NEW: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new NewObjectInst(tmpResult, className));
                pushVar(tmpResult);
                break;
            }

            case ANEWARRAY: {
                JavaType type = JavaType.newRefType(className);
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new NewArrayInst(tmpResult, type,
                                         Collections.singletonList(popVar())));
                pushVar(tmpResult);
                break;
            }

            case CHECKCAST:
                throw new ABCDException("TODO");

            case INSTANCEOF: {
                TemporaryVariable tmpResult = tmpVarFactory.create();
                addInst(new InstanceOfInst(tmpResult, popVar(), className));
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
                TemporaryVariable tmpVar = tmpVarFactory.create();
                pushVar(tmpVar);
                addInst(new AssignInst(tmpVar, new LocalVariable(index)));
                break;
            }

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                Variable var = popVar();
                addInst(new AssignInst(new LocalVariable(index), var));
                break;
            }

            case RET:
                throw new ABCDException("TODO : support RET instruction");
        }
    }

    public void after(BasicBlock block) {
    }

}
