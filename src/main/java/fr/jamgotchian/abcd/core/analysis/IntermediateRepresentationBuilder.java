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
class IntermediateRepresentationBuilder implements BasicBlockVisitor {

    private static final Logger logger = Logger.getLogger(IntermediateRepresentationBuilder.class.getName());

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

    protected final Deque<Variable> stack;

    protected final List<IRInst> insts;

    protected final TemporaryVariableFactory tmpVarFactory;

    public IntermediateRepresentationBuilder(ClassNameFactory classNameFactory) {
        this.classNameFactory = classNameFactory;
        stack = new ArrayDeque<Variable>();
        insts = new ArrayList<IRInst>();
        tmpVarFactory = new TemporaryVariableFactory();
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

  //      ((BasicBlockAnalysisDataImpl) block.getData()).addStatement(stmt);
        insts.add(inst);
    }

    public void before(BasicBlock block) {
    }

    public void visitFieldInsn(BasicBlock block, int index, FieldInsnNode node) {

        switch (node.getOpcode()) {
            case GETSTATIC: {
                String className = node.owner.replace('/', '.');
                String varName = node.name;
                Variable field = new StaticField(className, varName);
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, field));
                stack.push(tmpVar);
                break;
            }

            case PUTSTATIC: {
                String className = node.owner.replace('/', '.');
                String varName = node.name;
                Variable field = new StaticField(className, varName);
                Variable tmpVar = stack.pop();
                addInst(new AssignInst(field, tmpVar));
                break;
            }

            case GETFIELD: {
                Variable resultVar = tmpVarFactory.create();
                Variable objVar = stack.pop();
                String fieldName = node.name;
                addInst(new GetFieldInst(resultVar, objVar, fieldName));
                stack.push(resultVar);
                break;
            }

            case PUTFIELD: {
                Variable valueVar = stack.pop();
                Variable objVar = stack.pop();
                String fieldName = node.name;
                addInst(new SetFieldInst(objVar, fieldName, valueVar));
                break;
            }
        }
    }

    public void visitIincInsn(BasicBlock block, int index, IincInsnNode node) {
        Operand value = new IntConst(Math.abs(node.incr));
        Variable var = new LocalVariable(node.var);
        Variable tmpValue = tmpVarFactory.create();
        addInst(new AssignInst(tmpValue, value));
        Variable tmpResult = tmpVarFactory.create();
        addInst(new BinaryInst(tmpResult, node.incr > 0 ? BinaryOp.PLUS : BinaryOp.MINUS,
                                 var, tmpValue));
        addInst(new AssignInst(var, tmpResult));
    }

    public void visitInsn(BasicBlock block, int index, InsnNode node) {
        switch (node.getOpcode()) {
            case NOP:
                break;

            case ACONST_NULL: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new NullConst()));
                stack.push(tmpVar);
                break;
            }

            case ICONST_M1: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(1)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_0: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(0)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_1: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(1)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_2: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(2)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_3: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(3)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_4: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(4)));
                stack.push(tmpVar);
                break;
            }

            case ICONST_5: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new IntConst(5)));
                stack.push(tmpVar);
                break;
            }

            case LCONST_0: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new LongConst(0)));
                stack.push(tmpVar);
                break;
            }

            case LCONST_1: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new LongConst(1)));
                stack.push(tmpVar);
                break;
            }

            case FCONST_0: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new FloatConst(0f)));
                stack.push(tmpVar);
                break;
            }

            case FCONST_1: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new FloatConst(1f)));
                stack.push(tmpVar);
                break;
            }

            case FCONST_2: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new FloatConst(2f)));
                stack.push(tmpVar);
                break;
            }

            case DCONST_0: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new DoubleConst(0d)));
                stack.push(tmpVar);
                break;
            }

            case DCONST_1: {
                Variable tmpVar = tmpVarFactory.create();
                addInst(new AssignInst(tmpVar, new DoubleConst(1d)));
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
                Variable tmpResultVar = tmpVarFactory.create();
                addInst(new GetArrayInst(tmpResultVar, arrayVar, arrayIndex));
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
                addInst(new SetArrayInst(arrayVar, indexVar, valueVar));
                break;
            }

            case POP: {
                stack.pop();
                break;
            }

            case POP2:
                throw new ABCDException("TODO");

            case DUP:
                stack.push(stack.peek());
                break;

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
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.PLUS, left, right));
                stack.add(tmpResult);
                break;
            }

            case ISUB:
            case LSUB:
            case FSUB:
            case DSUB: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.MINUS, left, right));
                stack.add(tmpResult);
                break;
            }

            case IMUL:
            case LMUL:
            case FMUL:
            case DMUL: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.MUL, left, right));
                stack.add(tmpResult);
                break;
            }

            case IDIV:
            case LDIV:
            case FDIV:
            case DDIV: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.DIV, left, right));
                stack.add(tmpResult);
                break;
            }

            case IREM:
            case LREM:
            case FREM:
            case DREM: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.REMAINDER, left, right));
                stack.add(tmpResult);
                break;
            }

            case INEG:
            case LNEG:
            case FNEG:
            case DNEG: {
                Variable tmpResult = tmpVarFactory.create();
                addInst(new UnaryInst(tmpResult, UnaryOp.MINUS, stack.pop()));
                stack.add(tmpResult);
                break;
            }

            case ISHL:
            case LSHL: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.SHIFT_LEFT, left, right));
                stack.add(tmpResult);
                break;
            }

            case ISHR:
            case LSHR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.SHIFT_RIGHT, left, right));
                stack.add(tmpResult);
                break;
            }

            case IUSHR:
            case LUSHR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LOGICAL_SHIFT_RIGHT, left, right));
                stack.add(tmpResult);
                break;
            }

            case IAND:
            case LAND: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.AND, left, right));
                stack.add(tmpResult);
                break;
            }

            case IOR:
            case LOR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.OR, left, right));
                stack.add(tmpResult);
                break;
            }

            case IXOR:
            case LXOR: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.XOR, left, right));
                stack.add(tmpResult);
                break;
            }

            case I2L: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.LONG));
                stack.push(tmpResult);
                break;
            }

            case I2F: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.FLOAT));
                stack.push(tmpResult);
                break;
            }

            case I2D: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.DOUBLE));
                stack.push(tmpResult);
                break;
            }

            case L2I: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.INT));
                stack.push(tmpResult);
                break;
            }

            case L2F: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.FLOAT));
                stack.push(tmpResult);
                break;
            }

            case L2D: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.DOUBLE));
                stack.push(tmpResult);
                break;
            }

            case F2I: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.INT));
                stack.push(tmpResult);
                break;
            }

            case F2L: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.LONG));
                break;
            }

            case F2D: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.DOUBLE));
                stack.push(tmpResult);
                break;
            }

            case D2I: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.INT));
                stack.push(tmpResult);
                break;
            }

            case D2L: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.LONG));
                stack.push(tmpResult);
                break;
            }

            case D2F: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.FLOAT));
                stack.push(tmpResult);
                break;
            }

            case I2B: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.BYTE));
                stack.push(tmpResult);
                break;
            }

            case I2C: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.CHAR));
                stack.push(tmpResult);
                break;
            }

            case I2S: {
                Variable tmpResult = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new CastInst(tmpResult, var, JavaType.SHORT));
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
                Variable tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.MINUS, value1, value2));
                stack.push(tmpResult);
                break;
            }

            case IRETURN:
            case LRETURN:
            case FRETURN:
            case DRETURN:
            case ARETURN:
                addInst(new ReturnInst(stack.pop()));
                break;

            case RETURN:
                addInst(new ReturnInst());
                break;

            case ARRAYLENGTH: {
                Variable tmpVar = tmpVarFactory.create();
                Variable var = stack.pop();
                addInst(new ArrayLengthInst(tmpVar, var));
                break;
            }

            case ATHROW:
                addInst(new ThrowInst(stack.pop()));
                break;

            case MONITORENTER:
                addInst(new MonitorEnterInst(stack.pop()));
                break;

            case MONITOREXIT:
                addInst(new MonitorExitInst(stack.pop()));
                break;
        }
    }

    public void visitIntInsn(BasicBlock block, int index, IntInsnNode node) {
        Variable tmpVar = tmpVarFactory.create();
        switch (node.getOpcode()) {
            case BIPUSH:
                addInst(new AssignInst(tmpVar, new ByteConst((byte) node.operand)));
                break;

            case SIPUSH:
                addInst(new AssignInst(tmpVar, new ShortConst((short) node.operand)));
                break;

            case NEWARRAY:
                addInst(new NewArrayInst(tmpVar, ATYPES[node.operand],
                                             Collections.singletonList(stack.pop())));
                break;
        }
        stack.push(tmpVar);
    }

    public void visitJumpInsn(BasicBlock block, int index, JumpInsnNode node) {

        Variable tmpResult = null;

        switch(node.getOpcode()) {
            case IFEQ: {
                Variable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                stack.push(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, stack.pop(), tmpZero));
                break;
            }

            case IFNE: {
                Variable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                stack.push(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, stack.pop(), tmpZero));
                break;
            }

            case IFLT: {
                Variable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                stack.push(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LT, stack.pop(), tmpZero));
                break;
            }

            case IFGE: {
                Variable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                stack.push(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GE, stack.pop(), tmpZero));
                break;
            }

            case IFGT: {
                Variable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                stack.push(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GT, stack.pop(), tmpZero));
                break;
            }

            case IFLE: {
                Variable tmpZero = tmpVarFactory.create();
                addInst(new AssignInst(tmpZero, new IntConst(0)));
                stack.push(tmpZero);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LE, stack.pop(), tmpZero));
                break;
            }

            case IF_ICMPEQ: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ICMPNE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case IF_ICMPLT: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LT, left, right));
                break;
            }

            case IF_ICMPGE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GE, left, right));
                break;
            }

            case IF_ICMPGT: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.GT, left, right));
                break;
            }

            case IF_ICMPLE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.LE, left, right));
                break;
            }

            case IF_ACMPEQ: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, left, right));
                break;
            }

            case IF_ACMPNE: {
                Variable right = stack.pop();
                Variable left = stack.pop();
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, left, right));
                break;
            }

            case GOTO:
                break;

            case JSR:
                throw new ABCDException("TODO : support JSR instruction");

            case IFNULL: {
                Variable tmpNull = tmpVarFactory.create();
                addInst(new AssignInst(tmpNull, new NullConst()));
                stack.push(tmpNull);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.EQ, stack.pop(), tmpNull));
                break;
            }

            case IFNONNULL: {
                Variable tmpNull = tmpVarFactory.create();
                addInst(new AssignInst(tmpNull, new NullConst()));
                stack.push(tmpNull);
                tmpResult = tmpVarFactory.create();
                addInst(new BinaryInst(tmpResult, BinaryOp.NE, stack.pop(), tmpNull));
                break;
            }
        }

        Label label = block.getGraph().getLabelManager().getLabel(node.label);

        if (tmpResult != null) {
            addInst(new JumpIfInst(tmpResult, label));
            stack.push(tmpResult);
        } else {
            addInst(new GotoInst(label));
        }
    }

    public void visitLabel(BasicBlock block, int index, LabelNode node) {
        Label label = block.getGraph().getLabelManager().getLabel(node);
        addInst(new LabelInst(label));
    }

    public void visitLdcInsn(BasicBlock block, int index, LdcInsnNode node) {
        Variable tmpVar = tmpVarFactory.create();
        if (node.cst instanceof Type) {
            String className = ((Type)node.cst).getClassName();
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
        stack.push(tmpVar);
    }

    public void visitLookupSwitchInsn(BasicBlock block, int index, LookupSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(new SwitchInst(stack.pop(), labels));
    }

    public void visitMethodInsn(BasicBlock block, int index, MethodInsnNode node) {
        int argCount = Type.getArgumentTypes(node.desc).length;
        List<Variable> args = new ArrayList<Variable>(argCount);
        for (int i = 0; i < argCount; i++) {
            args.add(0, stack.pop());
        }

        String methodName = node.name;
        Variable resultVar = tmpVarFactory.create();
        switch (node.getOpcode()) {
            case INVOKEVIRTUAL:
            case INVOKESPECIAL:
            case INVOKEINTERFACE:
            case INVOKEDYNAMIC: {
                Variable objVar = stack.pop();
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
            stack.push(resultVar);
        }
    }

    public void visitMultiANewArrayInsn(BasicBlock block, int index, MultiANewArrayInsnNode node) {
        Type type = Type.getType(node.desc).getElementType();
        JavaType javaType = JavaType.newType(type, classNameFactory);
        List<Variable> dimensions = new ArrayList<Variable>(node.dims);
        for (int i = 0; i < node.dims; i++) {
            dimensions.add(0, stack.pop());
        }
        Variable tmpResult = tmpVarFactory.create();
        addInst(new NewArrayInst(tmpResult, javaType, dimensions));
        stack.push(tmpResult);
    }

    public void visitTableSwitchInsn(BasicBlock block, int index, TableSwitchInsnNode node) {
        List<Label> labels = new ArrayList<Label>();
        for (LabelNode labelNode : (List<LabelNode>) node.labels) {
            labels.add(block.getGraph().getLabelManager().getLabel(labelNode));
        }
        labels.add(block.getGraph().getLabelManager().getLabel(node.dflt));
        addInst(new SwitchInst(stack.pop(), labels));
    }

    public void visitTypeInsnInsn(BasicBlock block, int index, TypeInsnNode node) {
        ClassName className
                = classNameFactory.newClassName(Type.getObjectType(node.desc).getClassName());

        switch (node.getOpcode()) {
            case NEW: {
                Variable tmpResult = tmpVarFactory.create();
                addInst(new NewObjectInst(tmpResult, className));
                stack.push(tmpResult);
                break;
            }

            case ANEWARRAY: {
                JavaType type = JavaType.newRefType(className);
                Variable tmpResult = tmpVarFactory.create();
                addInst(new NewArrayInst(tmpResult, type,
                                           Collections.singletonList(stack.pop())));
                stack.push(tmpResult);
                break;
            }

            case CHECKCAST:
                throw new ABCDException("TODO");

            case INSTANCEOF: {
                Variable tmpResult = tmpVarFactory.create();
                addInst(new InstanceOfInst(tmpResult, stack.pop(), className));
                stack.push(tmpResult);
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
                Variable tmpVar = tmpVarFactory.create();
                stack.add(tmpVar);
                addInst(new AssignInst(tmpVar, new LocalVariable(index)));
                break;
            }

            case ISTORE:
            case LSTORE:
            case FSTORE:
            case DSTORE:
            case ASTORE: {
                Variable var = stack.pop();
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
