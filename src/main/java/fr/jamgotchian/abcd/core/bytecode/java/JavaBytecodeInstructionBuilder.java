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

package fr.jamgotchian.abcd.core.bytecode.java;

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.ir.AssignConstInst;
import fr.jamgotchian.abcd.core.ir.AssignVarInst;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import static fr.jamgotchian.abcd.core.ir.BasicBlockPropertyName.*;
import fr.jamgotchian.abcd.core.ir.InstructionBuilder;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.Edge;
import fr.jamgotchian.abcd.core.ir.EdgeAttribute;
import fr.jamgotchian.abcd.core.ir.ExceptionHandlerInfo;
import fr.jamgotchian.abcd.core.ir.MethodSignature;
import fr.jamgotchian.abcd.core.ir.StringConst;
import fr.jamgotchian.abcd.core.ir.IRBinaryOperator;
import fr.jamgotchian.abcd.core.ir.IRInst;
import fr.jamgotchian.abcd.core.ir.IRInstFactory;
import fr.jamgotchian.abcd.core.ir.IRInstSeq;
import fr.jamgotchian.abcd.core.ir.IRInstWriter;
import fr.jamgotchian.abcd.core.ir.IRUnaryOperator;
import fr.jamgotchian.abcd.core.ir.NewObjectInst;
import fr.jamgotchian.abcd.core.ir.VariableFactory;
import fr.jamgotchian.abcd.core.ir.ThrowInst;
import fr.jamgotchian.abcd.core.ir.Variable;
import fr.jamgotchian.abcd.core.ir.VariableStack;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameManager;
import fr.jamgotchian.abcd.core.type.ComputationalType;
import static fr.jamgotchian.abcd.core.type.ComputationalType.Category.*;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.objectweb.asm.Type;
import static org.objectweb.asm.Opcodes.*;
import org.objectweb.asm.tree.FieldInsnNode;
import org.objectweb.asm.tree.IincInsnNode;
import org.objectweb.asm.tree.InsnList;
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
public class JavaBytecodeInstructionBuilder implements InstructionBuilder {

    private static final Logger LOGGER
            = Logger.getLogger(JavaBytecodeInstructionBuilder.class.getName());

    private static final String MAGIC_STRING = "magic";

    private static final JavaType[] ATYPES = {
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

    private final InsnList instructions;

    private final LabelManager labelManager;

    private final ClassNameManager classNameManager;

    private final VariableFactory varFactory;

    private final IRInstFactory instFactory;

    private ControlFlowGraph cfg;

    private final Set<Variable> finallyTmpVars;

    private final Set<Variable> catchTmpVars;

    private class BytecodeRangeVisitorImpl extends JavaBytecodeVisitor {

        private final VariableStack stack;

        private BytecodeRangeVisitorImpl(VariableStack stack) {
            this.stack = stack;
        }

        private void pushGetArray(BasicBlock bb, ComputationalType type) {
            Variable arrayIndex = stack.pop();
            Variable arrayVar = stack.pop();
            Variable tmpResultVar = varFactory.createTmp(bb);
            bb.getInstructions().add(instFactory.newGetArray(tmpResultVar, arrayVar, arrayIndex));
            stack.push(tmpResultVar, type);
        }

        private void pushBinOp(BasicBlock bb, IRBinaryOperator operator, ComputationalType type) {
            Variable right = stack.pop();
            Variable left = stack.pop();
            Variable tmpResult = varFactory.createTmp(bb);
            bb.getInstructions().add(instFactory.newBinary(tmpResult, operator, left, right));
            stack.push(tmpResult, type);
        }

        private void pushUnaryOp(BasicBlock bb, IRUnaryOperator operator, ComputationalType type) {
            Variable tmpResult = varFactory.createTmp(bb);
            bb.getInstructions().add(instFactory.newUnary(tmpResult, operator, stack.pop()));
            stack.push(tmpResult, type);
        }

        private void pushCast(BasicBlock bb, JavaType type) {
            Variable tmpResult = varFactory.createTmp(bb);
            Variable var = stack.pop();
            bb.getInstructions().add(instFactory.newCast(tmpResult, var, type));
            stack.push(tmpResult, type.getComputationalType());
        }

        private void pushAssign(BasicBlock bb, ComputationalType type, Variable var) {
            Variable tmpVar = varFactory.createTmp(bb);
            stack.push(tmpVar, type);
            bb.getInstructions().add(instFactory.newAssignVar(tmpVar, var));
        }

        @Override
        public void before(BasicBlock bb) {
        }

        @Override
        public void visitFieldInsn(BasicBlock bb, int position, FieldInsnNode node) {
            JavaType fieldType = JavaBytecodeUtil.newType(Type.getType(node.desc), classNameManager);
            String fieldName = node.name;

            switch (node.getOpcode()) {
                case GETSTATIC: {
                    ClassName className = classNameManager.newClassName(node.owner.replace('/', '.'));
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newGetStaticField(tmpVar, className, fieldName, fieldType));
                    stack.push(tmpVar, fieldType.getComputationalType());
                    break;
                }

                case PUTSTATIC: {
                    ClassName className = classNameManager.newClassName(node.owner.replace('/', '.'));
                    Variable tmpVar = stack.pop();
                    bb.getInstructions().add(instFactory.newSetStaticField(className, fieldName, fieldType, tmpVar));
                    break;
                }

                case GETFIELD: {
                    Variable resultVar = varFactory.createTmp(bb);
                    Variable objVar = stack.pop();
                    bb.getInstructions().add(instFactory.newGetField(resultVar, objVar, fieldName, fieldType));
                    stack.push(resultVar, fieldType.getComputationalType());
                    break;
                }

                case PUTFIELD: {
                    Variable valueVar = stack.pop();
                    Variable objVar = stack.pop();
                    bb.getInstructions().add(instFactory.newSetField(objVar, fieldName, fieldType, valueVar));
                    break;
                }

                default:
                    throw new InternalError();
            }
        }

        @Override
        public void visitIincInsn(BasicBlock bb, int position, IincInsnNode node) {
            Variable tmpVar = varFactory.createTmp(bb);
            bb.getInstructions().add(instFactory.newAssignVar(tmpVar, varFactory.create(node.var, bb, position)));
            Variable tmpValue = varFactory.createTmp(bb);
            bb.getInstructions().add(instFactory.newAssignInt(tmpValue, Math.abs(node.incr)));
            Variable tmpResult = varFactory.createTmp(bb);
            IRBinaryOperator binOp = node.incr > 0 ? IRBinaryOperator.PLUS : IRBinaryOperator.MINUS;
            bb.getInstructions().add(instFactory.newBinary(tmpResult, binOp, tmpVar, tmpValue));
            bb.getInstructions().add(instFactory.newAssignVar(varFactory.create(node.var, bb, position), tmpResult));
        }

        @Override
        public void visitInsn(BasicBlock bb, int position, InsnNode node) {
            switch (node.getOpcode()) {
                case NOP:
                    break;

                case ACONST_NULL: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignNull(tmpVar));
                    stack.push(tmpVar, ComputationalType.REFERENCE);
                    break;
                }

                case ICONST_M1: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 1));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case ICONST_0: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 0));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case ICONST_1: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 1));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case ICONST_2: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 2));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case ICONST_3: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 3));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case ICONST_4: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 4));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case ICONST_5: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpVar, 5));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;
                }

                case LCONST_0: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignLong(tmpVar, 0));
                    stack.push(tmpVar, ComputationalType.LONG);
                    break;
                }

                case LCONST_1: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignLong(tmpVar, 1));
                    stack.push(tmpVar, ComputationalType.LONG);
                    break;
                }

                case FCONST_0: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignFloat(tmpVar, 0f));
                    stack.push(tmpVar, ComputationalType.FLOAT);
                    break;
                }

                case FCONST_1: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignFloat(tmpVar, 1f));
                    stack.push(tmpVar, ComputationalType.FLOAT);
                    break;
                }

                case FCONST_2: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignFloat(tmpVar, 2f));
                    stack.push(tmpVar, ComputationalType.FLOAT);
                    break;
                }

                case DCONST_0: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignDouble(tmpVar, 0d));
                    stack.push(tmpVar, ComputationalType.DOUBLE);
                    break;
                }

                case DCONST_1: {
                    Variable tmpVar = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignDouble(tmpVar, 1d));
                    stack.push(tmpVar, ComputationalType.DOUBLE);
                    break;
                }

                case IALOAD:
                case BALOAD:
                case CALOAD:
                case SALOAD:
                    pushGetArray(bb, ComputationalType.INT);
                    break;

                case LALOAD:
                    pushGetArray(bb, ComputationalType.LONG);
                    break;

                case FALOAD:
                    pushGetArray(bb, ComputationalType.FLOAT);
                    break;

                case DALOAD:
                    pushGetArray(bb, ComputationalType.DOUBLE);
                    break;

                case AALOAD:
                    pushGetArray(bb, ComputationalType.REFERENCE);
                    break;

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
                    bb.getInstructions().add(instFactory.newSetArray(arrayVar, indexVar, valueVar));
                    break;
                }

                case POP:
                    stack.pop();
                    break;

                case POP2:
                    if (stack.pop().getComputationalType().getCategory() == CATEGORY_1) {
                        stack.pop();
                    }
                    break;

                case DUP: {
                    Variable var = stack.pop();
                    stack.push(var, var.getComputationalType());
                    stack.push(var, var.getComputationalType());
                    break;
                }

                case DUP_X1: {
                    Variable var1 = stack.pop();
                    Variable var2 = stack.pop();
                    stack.push(var1, var1.getComputationalType());
                    stack.push(var2, var2.getComputationalType());
                    stack.push(var1, var1.getComputationalType());
                    break;
                }

                case DUP_X2: {
                    Variable var1 = stack.pop();
                    assert var1.getComputationalType().getCategory() == CATEGORY_1;
                    Variable var2 = stack.pop();
                    if (var2.getComputationalType().getCategory() == CATEGORY_1) {
                        // form 1
                        Variable var3 = stack.pop();
                        assert var3.getComputationalType().getCategory() == CATEGORY_1;
                        stack.push(var1, var1.getComputationalType());
                        stack.push(var3, var3.getComputationalType());
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                    } else {
                        // form 2
                        stack.push(var1, var1.getComputationalType());
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                    }
                    break;
                }

                case DUP2: {
                    Variable var1 = stack.pop();
                    if (var1.getComputationalType().getCategory() == CATEGORY_2) {
                        // form 2
                        stack.push(var1, var1.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                    } else {
                        // form 1
                        Variable var2 = stack.pop();
                        assert var2.getComputationalType().getCategory() == CATEGORY_1;
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                    }
                    break;
                }

                case DUP2_X1: {
                    Variable var1 = stack.pop();
                    Variable var2 = stack.pop();
                    assert var2.getComputationalType().getCategory() == CATEGORY_1;
                    if (var1.getComputationalType().getCategory() == CATEGORY_2) {
                        // form 2
                        stack.push(var1, var1.getComputationalType());
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                    } else {
                        // form 1
                        Variable var3 = stack.pop();
                        assert var3.getComputationalType().getCategory() == CATEGORY_1;
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                        stack.push(var3, var3.getComputationalType());
                        stack.push(var2, var2.getComputationalType());
                        stack.push(var1, var1.getComputationalType());
                    }
                    break;
                }

                case DUP2_X2: {
                    Variable var1 = stack.pop();
                    Variable var2 = stack.pop();
                    if (var1.getComputationalType().getCategory() == CATEGORY_2) {
                        if (var2.getComputationalType().getCategory() == CATEGORY_2) {
                            // form 4
                            stack.push(var1, var1.getComputationalType());
                            stack.push(var2, var2.getComputationalType());
                            stack.push(var1, var1.getComputationalType());
                        } else {
                            // form 2
                            Variable var3 = stack.pop();
                            assert var3.getComputationalType().getCategory() == CATEGORY_1;
                            stack.push(var1, var1.getComputationalType());
                            stack.push(var3, var3.getComputationalType());
                            stack.push(var2, var2.getComputationalType());
                            stack.push(var1, var1.getComputationalType());
                        }
                    } else if (var1.getComputationalType().getCategory() == CATEGORY_1
                            && var2.getComputationalType().getCategory() == CATEGORY_1) {
                        Variable var3 = stack.pop();
                        if (var3.getComputationalType().getCategory() == CATEGORY_2) {
                            // form 3
                            stack.push(var2, var2.getComputationalType());
                            stack.push(var1, var1.getComputationalType());
                            stack.push(var3, var3.getComputationalType());
                            stack.push(var2, var2.getComputationalType());
                            stack.push(var1, var1.getComputationalType());
                        } else {
                            // form 1
                            Variable var4 = stack.pop();
                            assert var4.getComputationalType().getCategory() == CATEGORY_1;
                            stack.push(var2, var2.getComputationalType());
                            stack.push(var1, var1.getComputationalType());
                            stack.push(var4, var4.getComputationalType());
                            stack.push(var3, var3.getComputationalType());
                            stack.push(var2, var2.getComputationalType());
                            stack.push(var1, var1.getComputationalType());
                        }
                    } else {
                        throw new InternalError();
                    }
                    break;
                }

                case SWAP: {
                    Variable var1 = stack.pop();
                    Variable var2 = stack.pop();
                    stack.push(var1, var1.getComputationalType());
                    stack.push(var2, var2.getComputationalType());
                    break;
                }

                case IADD:
                    pushBinOp(bb, IRBinaryOperator.PLUS, ComputationalType.INT);
                    break;

                case LADD:
                    pushBinOp(bb, IRBinaryOperator.PLUS, ComputationalType.LONG);
                    break;

                case FADD:
                    pushBinOp(bb, IRBinaryOperator.PLUS, ComputationalType.FLOAT);
                    break;

                case DADD:
                    pushBinOp(bb, IRBinaryOperator.PLUS, ComputationalType.DOUBLE);
                    break;

                case ISUB:
                    pushBinOp(bb, IRBinaryOperator.MINUS, ComputationalType.INT);
                    break;

                case LSUB:
                    pushBinOp(bb, IRBinaryOperator.MINUS, ComputationalType.LONG);
                    break;

                case FSUB:
                    pushBinOp(bb, IRBinaryOperator.MINUS, ComputationalType.FLOAT);
                    break;

                case DSUB:
                    pushBinOp(bb, IRBinaryOperator.MINUS, ComputationalType.DOUBLE);
                    break;

                case IMUL:
                    pushBinOp(bb, IRBinaryOperator.MUL, ComputationalType.INT);
                    break;

                case LMUL:
                    pushBinOp(bb, IRBinaryOperator.MUL, ComputationalType.LONG);
                    break;

                case FMUL:
                    pushBinOp(bb, IRBinaryOperator.MUL, ComputationalType.FLOAT);
                    break;

                case DMUL:
                    pushBinOp(bb, IRBinaryOperator.MUL, ComputationalType.DOUBLE);
                    break;

                case IDIV:
                    pushBinOp(bb, IRBinaryOperator.DIV, ComputationalType.INT);
                    break;

                case LDIV:
                    pushBinOp(bb, IRBinaryOperator.DIV, ComputationalType.LONG);
                    break;

                case FDIV:
                    pushBinOp(bb, IRBinaryOperator.DIV, ComputationalType.FLOAT);
                    break;

                case DDIV:
                    pushBinOp(bb, IRBinaryOperator.DIV, ComputationalType.DOUBLE);
                    break;

                case IREM:
                    pushBinOp(bb, IRBinaryOperator.REMAINDER, ComputationalType.INT);
                    break;

                case LREM:
                    pushBinOp(bb, IRBinaryOperator.REMAINDER, ComputationalType.LONG);
                    break;

                case FREM:
                    pushBinOp(bb, IRBinaryOperator.REMAINDER, ComputationalType.FLOAT);
                    break;

                case DREM:
                    pushBinOp(bb, IRBinaryOperator.REMAINDER, ComputationalType.DOUBLE);
                    break;

                case INEG:
                    pushUnaryOp(bb, IRUnaryOperator.MINUS, ComputationalType.INT);
                    break;

                case LNEG:
                    pushUnaryOp(bb, IRUnaryOperator.MINUS, ComputationalType.LONG);
                    break;

                case FNEG:
                    pushUnaryOp(bb, IRUnaryOperator.MINUS, ComputationalType.FLOAT);
                    break;

                case DNEG:
                    pushUnaryOp(bb, IRUnaryOperator.MINUS, ComputationalType.DOUBLE);
                    break;

                case ISHL:
                    pushBinOp(bb, IRBinaryOperator.SHIFT_LEFT, ComputationalType.INT);
                    break;

                case LSHL:
                    pushBinOp(bb, IRBinaryOperator.SHIFT_LEFT, ComputationalType.LONG);
                    break;

                case ISHR:
                    pushBinOp(bb, IRBinaryOperator.SHIFT_RIGHT, ComputationalType.INT);
                    break;

                case LSHR:
                    pushBinOp(bb, IRBinaryOperator.SHIFT_LEFT, ComputationalType.LONG);
                    break;

                case IUSHR:
                    pushBinOp(bb, IRBinaryOperator.LOGICAL_SHIFT_RIGHT, ComputationalType.INT);
                    break;

                case LUSHR:
                    pushBinOp(bb, IRBinaryOperator.LOGICAL_SHIFT_RIGHT, ComputationalType.LONG);
                    break;

                case IAND:
                    pushBinOp(bb, IRBinaryOperator.BITWISE_AND, ComputationalType.INT);
                    break;

                case LAND:
                    pushBinOp(bb, IRBinaryOperator.BITWISE_AND, ComputationalType.LONG);
                    break;

                case IOR:
                    pushBinOp(bb, IRBinaryOperator.BITWISE_OR, ComputationalType.INT);
                    break;

                case LOR:
                    pushBinOp(bb, IRBinaryOperator.BITWISE_OR, ComputationalType.LONG);
                    break;

                case IXOR:
                    pushBinOp(bb, IRBinaryOperator.BITWISE_XOR, ComputationalType.INT);
                    break;

                case LXOR:
                    pushBinOp(bb, IRBinaryOperator.BITWISE_XOR, ComputationalType.LONG);
                    break;

                case I2L:
                    pushCast(bb, JavaType.LONG);
                    break;

                case I2F:
                    pushCast(bb, JavaType.FLOAT);
                    break;

                case I2D:
                    pushCast(bb, JavaType.DOUBLE);
                    break;

                case L2I:
                    pushCast(bb, JavaType.INT);
                    break;

                case L2F:
                    pushCast(bb, JavaType.FLOAT);
                    break;

                case L2D:
                    pushCast(bb, JavaType.DOUBLE);
                    break;

                case F2I:
                    pushCast(bb, JavaType.INT);
                    break;

                case F2L:
                    pushCast(bb, JavaType.LONG);
                    break;

                case F2D:
                    pushCast(bb, JavaType.DOUBLE);
                    break;

                case D2I:
                    pushCast(bb, JavaType.INT);
                    break;

                case D2L:
                    pushCast(bb, JavaType.LONG);
                    break;

                case D2F:
                    pushCast(bb, JavaType.FLOAT);
                    break;

                case I2B:
                    pushCast(bb, JavaType.BYTE);
                    break;

                case I2C:
                    pushCast(bb, JavaType.CHAR);
                    break;

                case I2S:
                    pushCast(bb, JavaType.SHORT);
                    break;

                case LCMP:
                case FCMPL:
                case FCMPG:
                case DCMPL:
                case DCMPG: {
                    Variable value2 = stack.pop();
                    Variable value1 = stack.pop();
                    Variable tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.MINUS, value1, value2));
                    stack.push(tmpResult, ComputationalType.INT);
                    break;
                }

                case IRETURN:
                case LRETURN:
                case FRETURN:
                case DRETURN:
                case ARETURN:
                    bb.getInstructions().add(instFactory.newReturn(stack.pop()));
                    break;

                case RETURN:
                    bb.getInstructions().add(instFactory.newReturn());
                    break;

                case ARRAYLENGTH: {
                    Variable result = varFactory.createTmp(bb);
                    Variable arrayVar = stack.pop();
                    bb.getInstructions().add(instFactory.newArrayLength(result, arrayVar));
                    stack.push(result, ComputationalType.INT);
                    break;
                }

                case ATHROW:
                    bb.getInstructions().add(instFactory.newThrow(stack.pop()));
                    break;

                case MONITORENTER:
                    bb.getInstructions().add(instFactory.newMonitorEnter(stack.pop()));
                    break;

                case MONITOREXIT:
                    bb.getInstructions().add(instFactory.newMonitorExit(stack.pop()));
                    break;

                default:
                    throw new InternalError();
            }
        }

        @Override
        public void visitIntInsn(BasicBlock bb, int position, IntInsnNode node) {
            Variable tmpVar = varFactory.createTmp(bb);
            switch (node.getOpcode()) {
                case BIPUSH:
                    bb.getInstructions().add(instFactory.newAssignByte(tmpVar, (byte) node.operand));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;

                case SIPUSH:
                    bb.getInstructions().add(instFactory.newAssignShort(tmpVar, (short) node.operand));
                    stack.push(tmpVar, ComputationalType.INT);
                    break;

                case NEWARRAY:
                    bb.getInstructions().add(instFactory.newNewArray(tmpVar, ATYPES[node.operand],
                                                           Collections.singletonList(stack.pop())));
                    stack.push(tmpVar, ComputationalType.REFERENCE);
                    break;

                default:
                    throw new InternalError();
            }
        }

        @Override
        public void visitJumpInsn(BasicBlock bb, int position, JumpInsnNode node, LabelManager labelManager) {

            Variable tmpResult = null;

            switch(node.getOpcode()) {
                case IFEQ: {
                    Variable tmpZero = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpZero, 0));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.EQ, stack.pop(), tmpZero));
                    break;
                }

                case IFNE: {
                    Variable tmpZero = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpZero, 0));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.NE, stack.pop(), tmpZero));
                    break;
                }

                case IFLT: {
                    Variable tmpZero = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpZero, 0));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.LT, stack.pop(), tmpZero));
                    break;
                }

                case IFGE: {
                    Variable tmpZero = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpZero, 0));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.GE, stack.pop(), tmpZero));
                    break;
                }

                case IFGT: {
                    Variable tmpZero = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpZero, 0));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.GT, stack.pop(), tmpZero));
                    break;
                }

                case IFLE: {
                    Variable tmpZero = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignInt(tmpZero, 0));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.LE, stack.pop(), tmpZero));
                    break;
                }

                case IF_ICMPEQ: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.EQ, left, right));
                    break;
                }

                case IF_ICMPNE: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.NE, left, right));
                    break;
                }

                case IF_ICMPLT: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.LT, left, right));
                    break;
                }

                case IF_ICMPGE: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.GE, left, right));
                    break;
                }

                case IF_ICMPGT: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.GT, left, right));
                    break;
                }

                case IF_ICMPLE: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.LE, left, right));
                    break;
                }

                case IF_ACMPEQ: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.EQ, left, right));
                    break;
                }

                case IF_ACMPNE: {
                    Variable right = stack.pop();
                    Variable left = stack.pop();
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.NE, left, right));
                    break;
                }

                case GOTO:
                    break;

                case JSR:
                    throw new ABCDException("TODO : support JSR instruction");

                case IFNULL: {
                    Variable tmpNull = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignNull(tmpNull));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.EQ, stack.pop(), tmpNull));
                    break;
                }

                case IFNONNULL: {
                    Variable tmpNull = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newAssignNull(tmpNull));
                    tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newBinary(tmpResult, IRBinaryOperator.NE, stack.pop(), tmpNull));
                    break;
                }

                default:
                    throw new InternalError();
            }

            if (tmpResult != null) {
                bb.getInstructions().add(instFactory.newJumpIf(tmpResult.clone()));
            }
        }

        @Override
        public void visitLabel(BasicBlock bb, int position, LabelNode node, LabelManager labelManager) {
        }

        @Override
        public void visitLdcInsn(BasicBlock bb, int position, LdcInsnNode node) {
            Variable tmpVar = varFactory.createTmp(bb);
            if (node.cst instanceof Type) {
                ClassName className = classNameManager.newClassName(((Type)node.cst).getClassName());
                bb.getInstructions().add(instFactory.newAssignClass(tmpVar, className));
                stack.push(tmpVar, ComputationalType.REFERENCE);
            } else if (node.cst instanceof Integer) {
                bb.getInstructions().add(instFactory.newAssignInt(tmpVar, (Integer) node.cst));
                stack.push(tmpVar, ComputationalType.INT);
            } else if (node.cst instanceof Long) {
                bb.getInstructions().add(instFactory.newAssignLong(tmpVar, (Long) node.cst));
                stack.push(tmpVar, ComputationalType.LONG);
            } else if (node.cst instanceof Float) {
                bb.getInstructions().add(instFactory.newAssignFloat(tmpVar, (Float) node.cst));
                stack.push(tmpVar, ComputationalType.FLOAT);
            } else if (node.cst instanceof Double) {
                bb.getInstructions().add(instFactory.newAssignDouble(tmpVar, (Double) node.cst));
                stack.push(tmpVar, ComputationalType.DOUBLE);
            } else if (node.cst instanceof String) {
                bb.getInstructions().add(instFactory.newAssignString(tmpVar, node.cst.toString()));
                stack.push(tmpVar, ComputationalType.REFERENCE);
            } else {
                throw new InternalError();
            }
        }

        @Override
        public void visitLookupSwitchInsn(BasicBlock bb, int position, LookupSwitchInsnNode node, LabelManager labelManager) {
            bb.getInstructions().add(instFactory.newSwitch(stack.pop()));
        }

        @Override
        public void visitMethodInsn(BasicBlock bb, int position, MethodInsnNode node) {
            // return type
            Type returnType = Type.getReturnType(node.desc);
            JavaType returnJavaType = JavaBytecodeUtil.newType(returnType, classNameManager);

            // argument types
            Type[] argTypes = Type.getArgumentTypes(node.desc);
            List<Variable> args = new ArrayList<Variable>(argTypes.length);
            List<JavaType> argJavaTypes = new ArrayList<JavaType>(argTypes.length);
            for (int i = 0; i < argTypes.length; i++) {
                args.add(0, stack.pop());
                argJavaTypes.add(0, JavaBytecodeUtil.newType(argTypes[i], classNameManager));
            }

            String methodName = node.name;

            MethodSignature signature = new MethodSignature(methodName, returnJavaType,
                                                            argJavaTypes);

            Variable resultVar = varFactory.createTmp(bb);
            switch (node.getOpcode()) {
                case INVOKEVIRTUAL:
                case INVOKESPECIAL:
                case INVOKEINTERFACE:
                case INVOKEDYNAMIC: {
                    Variable objVar = stack.pop();
                    bb.getInstructions().add(instFactory.newCallMethod(resultVar, objVar, signature,
                                                             args));
                    break;
                }

                case INVOKESTATIC: {
                    ClassName className = classNameManager.newClassName(node.owner.replace('/', '.'));
                    bb.getInstructions().add(instFactory.newCallStaticMethod(resultVar, className,
                                                                   signature, args));
                    break;
                }

                default:
                    throw new InternalError();
            }
            if (returnType != Type.VOID_TYPE) {
                stack.push(resultVar, returnJavaType.getComputationalType());
            }
        }

        @Override
        public void visitMultiANewArrayInsn(BasicBlock bb, int position, MultiANewArrayInsnNode node) {
            Type type = Type.getType(node.desc).getElementType();
            JavaType javaType = JavaBytecodeUtil.newType(type, classNameManager);
            List<Variable> dimensions = new ArrayList<Variable>(node.dims);
            for (int i = 0; i < node.dims; i++) {
                dimensions.add(0, stack.pop());
            }
            Variable tmpResult = varFactory.createTmp(bb);
            bb.getInstructions().add(instFactory.newNewArray(tmpResult, javaType, dimensions));
            stack.push(tmpResult, ComputationalType.REFERENCE);
        }

        @Override
        public void visitTableSwitchInsn(BasicBlock bb, int position, TableSwitchInsnNode node, LabelManager labelManager) {
            bb.getInstructions().add(instFactory.newSwitch(stack.pop()));
        }

        @Override
        public void visitTypeInsnInsn(BasicBlock bb, int position, TypeInsnNode node) {
            JavaType type = JavaBytecodeUtil.newType(Type.getObjectType(node.desc), classNameManager);

            switch (node.getOpcode()) {
                case NEW: {
                    Variable tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newNewObject(tmpResult, type));
                    stack.push(tmpResult, ComputationalType.REFERENCE);
                    break;
                }

                case ANEWARRAY: {
                    Variable tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newNewArray(tmpResult, type,
                                             Collections.singletonList(stack.pop())));
                    stack.push(tmpResult, ComputationalType.REFERENCE);
                    break;
                }

                case CHECKCAST: {
                    Variable tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newCast(tmpResult, stack.pop(), type));
                    stack.push(tmpResult, ComputationalType.REFERENCE);
                    break;
                }

                case INSTANCEOF: {
                    Variable tmpResult = varFactory.createTmp(bb);
                    bb.getInstructions().add(instFactory.newInstanceOf(tmpResult, stack.pop(), type));
                    stack.push(tmpResult, ComputationalType.INT);
                    break;
                }

                default:
                    throw new InternalError();
            }
        }

        @Override
        public void visitVarInsn(BasicBlock bb, int position, VarInsnNode node) {
            Variable var = varFactory.create(node.var, bb, position);
            switch (node.getOpcode()) {
                case ILOAD:
                    pushAssign(bb, ComputationalType.INT, var);
                    break;

                case LLOAD:
                    pushAssign(bb, ComputationalType.LONG, var);
                    break;

                case FLOAD:
                    pushAssign(bb, ComputationalType.FLOAT, var);
                    break;

                case DLOAD:
                    pushAssign(bb, ComputationalType.DOUBLE, var);
                    break;

                case ALOAD:
                    pushAssign(bb, ComputationalType.REFERENCE, var);
                    break;

                case ISTORE:
                case LSTORE:
                case FSTORE:
                case DSTORE:
                case ASTORE:
                    bb.getInstructions().add(instFactory.newAssignVar(var, stack.pop()));
                    break;

                case RET:
                    throw new ABCDException("TODO : support RET instruction");

                default:
                    throw new InternalError();
            }
        }

        @Override
        public void after(BasicBlock bb) {
        }

    }

    public JavaBytecodeInstructionBuilder(InsnList instructions,
                                    LabelManager labelManager,
                                    ClassNameManager classNameManager,
                                    VariableFactory varFactory,
                                    IRInstFactory instFactory) {
        this.instructions = instructions;
        this.labelManager = labelManager;
        this.classNameManager = classNameManager;
        this.varFactory = varFactory;
        this.instFactory = instFactory;
        finallyTmpVars = new HashSet<Variable>();
        catchTmpVars = new HashSet<Variable>();
    }

    private void processBB(BasicBlock bb, List<VariableStack> inputStacks) {

        LOGGER.log(Level.FINER, "Process {0}", bb);

        VariableStack inputStack = null;

        if (bb.hasProperty(EXCEPTION_HANDLER_ENTRY)) {
            // at the entry block of an exception handler the stack only contains
            // the exception variable
            inputStack = new VariableStack();
            Variable exceptionVar = varFactory.createTmp(bb);
            IRInst tmpInst;
            if (bb.hasProperty(FINALLY_ENTRY)) {
                finallyTmpVars.add(exceptionVar);
                tmpInst = instFactory.newAssignString(exceptionVar, MAGIC_STRING);
            } else { // catch
                ExceptionHandlerInfo info
                        = (ExceptionHandlerInfo) bb.getProperty(EXCEPTION_HANDLER_ENTRY);
                catchTmpVars.add(exceptionVar);
                ClassName className = classNameManager.newClassName(info.getClassName());
                tmpInst = instFactory.newNewObject(exceptionVar, JavaType.newRefType(className));
            }
            bb.getInstructions().add(tmpInst);
            inputStack.push(exceptionVar, ComputationalType.REFERENCE);
        } else {
            if (inputStacks.isEmpty()) {
                inputStack = new VariableStack();
            } else if (inputStacks.size() == 1) {
                inputStack = inputStacks.get(0).clone();
            } else {
                inputStack = mergeStacks(inputStacks, bb);
            }
        }

        bb.setInputStack(inputStack.clone());

        if (bb.getInputStack().size() > 0) {
            LOGGER.log(Level.FINEST, ">>> Input stack : {0}", bb.getInputStack());
        }

        VariableStack outputStack = inputStack.clone();

        new BytecodeRangeVisitorImpl(outputStack).visit(instructions, bb, labelManager);

        bb.setOutputStack(outputStack);

        if (bb.getOutputStack().size() > 0) {
            LOGGER.log(Level.FINEST, "<<< Output stack : {0}", bb.getOutputStack());
        }
    }

    private VariableStack mergeStacks(List<VariableStack> stacks, BasicBlock bb) {
        if (stacks.size() <= 1) {
            throw new ABCDException("stacks.size() <= 1");
        }
        List<Integer> sizes = new ArrayList<Integer>(stacks.size());
        for (int i = 0; i < stacks.size(); i++) {
            sizes.add(stacks.get(i).size());
        }
        for (int i = 0; i < sizes.size() - 1; i++) {
            if (sizes.get(i) != sizes.get(i + 1)) {
                throw new ABCDException("Cannot merge stacks with differents sizes : "
                        + sizes);
            }
        }

        VariableStack stacksMerge = new VariableStack();

        List<List<Variable>> toList = new ArrayList<List<Variable>>(stacks.size());
        for (int i = 0; i < stacks.size(); i++) {
            toList.add(stacks.get(i).toList());
        }
        for (int i = stacks.get(0).size()-1; i >= 0 ; i--) {
            Set<Variable> vars = new HashSet<Variable>(stacks.size());
            for (int j = 0; j < stacks.size(); j++) {
                vars.add(toList.get(j).get(i));
            }
            if (vars.size() == 1) {
                Variable var1 = vars.iterator().next();
                stacksMerge.push(var1, var1.getComputationalType());
            } else {
                Variable result = varFactory.createTmp(bb);
                bb.getInstructions().add(instFactory.newChoice(result, vars));
                stacksMerge.push(result, vars.iterator().next().getComputationalType());
            }
        }

        return stacksMerge;
    }

    private void cleanupExceptionHandlers() {
        Set<Variable> finallyVars = new HashSet<Variable>();

        for (BasicBlock bb : cfg.getBasicBlocks()) {
            if (!bb.hasProperty(EXCEPTION_HANDLER_ENTRY)) {
                continue;
            }

            IRInstSeq seq = bb.getInstructions();
            for (int i = 0; i < seq.size()-1; i++) {
                IRInst inst = seq.get(i);
                IRInst inst2 = seq.get(i+1);

                boolean remove = false;
                Variable excVar = null;

                if (inst instanceof AssignConstInst
                        && inst2 instanceof AssignVarInst) {
                    AssignConstInst assignCstInst = (AssignConstInst) inst;
                    AssignVarInst assignVarInst = (AssignVarInst) inst2;
                    if (finallyTmpVars.contains(assignCstInst.getResult())
                            && assignCstInst.getConst() instanceof StringConst
                            && ((StringConst) assignCstInst.getConst()).getValue().equals(MAGIC_STRING)
                            && !assignVarInst.getResult().isTemporary()
                            && assignCstInst.getResult().equals(assignVarInst.getValue())) {
                        excVar = assignVarInst.getResult();
                        finallyVars.add(assignVarInst.getResult());
                        remove = true;
                    }
                }
                if (inst instanceof NewObjectInst
                        && inst2 instanceof AssignVarInst) {
                    NewObjectInst newObjInst = (NewObjectInst) inst;
                    AssignVarInst assignVarInst = (AssignVarInst) inst2;
                    if (catchTmpVars.contains(newObjInst.getResult())
                            && !assignVarInst.getResult().isTemporary()
                            && newObjInst.getResult().equals(assignVarInst.getValue())) {
                        excVar = assignVarInst.getResult();
                        remove = true;
                    }
                }

                if (remove) {
                    ((ExceptionHandlerInfo) bb.getProperty(EXCEPTION_HANDLER_ENTRY)).setVariable(excVar);
                    LOGGER.log(Level.FINEST, "Cleanup exception handler (bb={0}, excVar={1}) :",
                            new Object[] {bb, excVar});
                    LOGGER.log(Level.FINEST, "  Remove inst : {0}", IRInstWriter.toText(inst));
                    LOGGER.log(Level.FINEST, "  Remove inst : {0}", IRInstWriter.toText(inst2));
                    inst.setIgnored(true);
                    inst2.setIgnored(true);
                }
            }
        }

        for (BasicBlock bb : cfg.getBasicBlocks()) {
            IRInstSeq seq = bb.getInstructions();
            for (int i = 0; i < seq.size()-1; i++) {
                IRInst inst = seq.get(i);
                IRInst inst2 = seq.get(i+1);

                boolean remove = false;

                Variable excVar = null;
                if (inst instanceof AssignVarInst
                        && inst2 instanceof ThrowInst) {
                    AssignVarInst assignVarInst = (AssignVarInst) inst;
                    ThrowInst throwInst = (ThrowInst) inst2;
                    if (finallyVars.contains(assignVarInst.getValue())
                            && assignVarInst.getResult().equals(throwInst.getVar())) {
                        excVar = assignVarInst.getValue();
                        remove = true;
                    }
                }

                if (remove) {
                    LOGGER.log(Level.FINEST, "Cleanup finally rethrow (excVar={0}) :", excVar);
                    LOGGER.log(Level.FINEST, "  Remove inst : {0}", IRInstWriter.toText(inst));
                    LOGGER.log(Level.FINEST, "  Remove inst : {0}", IRInstWriter.toText(inst2));
                    inst.setIgnored(true);
                    inst2.setIgnored(true);
                }
            }
        }
    }

    @Override
    public void build(ControlFlowGraph cfg) {
        this.cfg = cfg;

        ConsoleUtil.logTitledSeparator(LOGGER, Level.FINE, "Build instructions of {0}",
                '=', cfg.getName());

        catchTmpVars.clear();
        finallyTmpVars.clear();

        for (BasicBlock bb : cfg.getBasicBlocks()) {
            bb.setInstructions(new IRInstSeq());
        }

        List<BasicBlock> blocksToProcess = new ArrayList<BasicBlock>(cfg.getDFST().getNodes());
        while (blocksToProcess.size() > 0) {
            for (Iterator<BasicBlock> it = blocksToProcess.iterator(); it.hasNext();) {
                BasicBlock bb = it.next();

                List<VariableStack> inputStacks = new ArrayList<VariableStack>();
                for (Edge incomingEdge : cfg.getIncomingEdgesOf(bb)) {
                    if (incomingEdge.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
                        continue;
                    }
                    BasicBlock pred = cfg.getEdgeSource(incomingEdge);
                    inputStacks.add(pred.getOutputStack().clone());
                }

                processBB(bb, inputStacks);
                it.remove();
            }
        }

        // cleanup exception handler by removing fake instructions
        cleanupExceptionHandlers();
    }
}
