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
package fr.jamgotchian.abcd.core.ir;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameManager;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.type.PrimitiveType;
import fr.jamgotchian.abcd.core.type.TypeHierarchyIndexer;
import fr.jamgotchian.abcd.core.type.TypeKind;
import java.util.Collection;
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
public class LocalVariableTypeAnalyser {

    private static final Logger LOGGER
            = Logger.getLogger(LocalVariableTypeAnalyser.class.getName());

    private static PrimitiveType[] PRIMITIVE_TYPES = { PrimitiveType.BOOLEAN,
                                                       PrimitiveType.CHAR,
                                                       PrimitiveType.BYTE,
                                                       PrimitiveType.SHORT,
                                                       PrimitiveType.INTEGER,
                                                       PrimitiveType.LONG,
                                                       PrimitiveType.FLOAT,
                                                       PrimitiveType.DOUBLE };

    public static final PrimitiveType[] ARITHMETIC_TYPES = { PrimitiveType.INTEGER,
                                                             PrimitiveType.LONG,
                                                             PrimitiveType.BYTE,
                                                             PrimitiveType.SHORT,
                                                             PrimitiveType.FLOAT,
                                                             PrimitiveType.DOUBLE };

    private class Visitor extends EmptyIRInstVisitor<Void, Void> {

        @Override
        public Void visit(IRInstSeq seq, Void arg) {
            return super.visit(seq, arg);
        }

        @Override
        public Void visit(ArrayLengthInst inst, Void arg) {
            addConstraint(inst.getResult(), PrimitiveType.INTEGER);
            return null;
        }

        @Override
        public Void visit(AssignConstInst inst, Void arg) {
            if (inst.getConst() instanceof ByteConst) {
                byte value = ((ByteConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addConstraint(inst.getResult(), PrimitiveType.BYTE,
                                                    PrimitiveType.CHAR,
                                                    PrimitiveType.BOOLEAN);
                } else {
                    addConstraint(inst.getResult(), PrimitiveType.BYTE,
                                                    PrimitiveType.CHAR);
                }
            } else if (inst.getConst() instanceof ClassConst) {
                ClassName subclass = classNameManager.newClassName(Class.class.getName());
                // TODO
            } else if (inst.getConst() instanceof DoubleConst) {
                addConstraint(inst.getResult(), PrimitiveType.DOUBLE);
            } else if (inst.getConst() instanceof FloatConst) {
                addConstraint(inst.getResult(), PrimitiveType.FLOAT);
            } else if (inst.getConst() instanceof IntConst) {
                int value = ((IntConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addConstraint(inst.getResult(), PrimitiveType.INTEGER,
                                                    PrimitiveType.BOOLEAN);
                } else {
                    addConstraint(inst.getResult(), PrimitiveType.INTEGER);
                }
            } else if (inst.getConst() instanceof LongConst) {
                long value = ((LongConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addConstraint(inst.getResult(), PrimitiveType.LONG,
                                                    PrimitiveType.BOOLEAN);
                } else {
                    addConstraint(inst.getResult(), PrimitiveType.LONG);
                }
            } else if (inst.getConst() instanceof NullConst) {
                // TODO
            } else if (inst.getConst() instanceof ShortConst) {
                short value = ((ShortConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addConstraint(inst.getResult(), PrimitiveType.SHORT,
                                                    PrimitiveType.BOOLEAN);
                } else {
                    addConstraint(inst.getResult(), PrimitiveType.SHORT);
                }
            } else if (inst.getConst() instanceof StringConst) {
                // TODO
            } else {
                throw new InternalError();
            }
            return null;
        }

        @Override
        public Void visit(AssignVarInst inst, Void arg) {
            addConstraint(inst.getResult(), inst.getValue());
            return null;
        }

        @Override
        public Void visit(BinaryInst inst, Void arg) {
            switch (inst.getOperator()) {
                case PLUS:
                case MINUS:
                case MUL:
                    addConstraint(inst.getResult(), ARITHMETIC_TYPES);
                    addConstraint(inst.getLeft(), inst.getRight());
                    addConstraint(inst.getResult(), inst.getLeft());
                    break;

                case DIV:
                    addConstraint(inst.getResult(), PrimitiveType.FLOAT);
                    addConstraint(inst.getLeft(), ARITHMETIC_TYPES);
                    addConstraint(inst.getRight(), ARITHMETIC_TYPES);
                    break;

                case LT:
                case LE:
                case GT:
                case GE:
                    addConstraint(inst.getResult(), PrimitiveType.BOOLEAN);
                    addConstraint(inst.getLeft(), ARITHMETIC_TYPES);
                    addConstraint(inst.getLeft(), inst.getRight());
                    break;

                case EQ:
                case NE:
                    addConstraint(inst.getResult(), PrimitiveType.BOOLEAN);
                    addConstraint(inst.getResult(), PRIMITIVE_TYPES);

                case OR:
                case AND:
                    addConstraint(inst.getResult(), PrimitiveType.BOOLEAN);
                    addConstraint(inst.getLeft(), PrimitiveType.BOOLEAN);
                    addConstraint(inst.getRight(), PrimitiveType.BOOLEAN);
                    break;
            }
            return null;
        }

        private void visitCallableInst(CallableInst inst) {
            MethodSignature signature = inst.getSignature();
            JavaType returnType = signature.getReturnType();
            if (returnType.getKind() == TypeKind.PRIMITIVE) {
                if (returnType.getPrimitiveType() != PrimitiveType.VOID) {
                    addConstraint(inst.getResult(), returnType.getPrimitiveType());
                }
            } else {
                // TODO
            }
            for (int i = 0; i < signature.getArgumentTypes().size(); i++) {
                JavaType argType = signature.getArgumentTypes().get(i);
                Variable argVar = inst.getArguments().get(i);
                if (argType.getKind() == TypeKind.PRIMITIVE) {
                    addConstraint(argVar, argType.getPrimitiveType());
                } else {
                    // TODO
                }
            }
        }

        @Override
        public Void visit(CallMethodInst inst, Void arg) {
            visitCallableInst(inst);
            return null;
        }

        @Override
        public Void visit(CallStaticMethodInst inst, Void arg) {
            visitCallableInst(inst);
            return null;
        }

        @Override
        public Void visit(CastInst inst, Void arg) {
            JavaType castType = inst.getCastType();
            if (castType.getKind() == TypeKind.PRIMITIVE) {
                addConstraint(inst.getResult(), castType.getPrimitiveType());
            } else {
                // TODO
            }
            return null;
        }

        @Override
        public Void visit(ConditionalInst inst, Void arg) {
            addConstraint(inst.getCond(), PrimitiveType.BOOLEAN);
            addConstraint(inst.getThen(), inst.getElse());
            addConstraint(inst.getResult(), inst.getThen());
            return null;
        }

        @Override
        public Void visit(GetArrayInst inst, Void arg) {
            addConstraint(inst.getIndex(), PrimitiveType.INTEGER);
            return null;
        }

        @Override
        public Void visit(SetArrayInst inst, Void arg) {
            addConstraint(inst.getIndex(), PrimitiveType.INTEGER);
            return null;
        }

        @Override
        public Void visit(GetFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (fieldType.getKind() == TypeKind.PRIMITIVE) {
                addConstraint(inst.getResult(), fieldType.getPrimitiveType());
            } else {
                // TODO
            }
            return null;
        }

        @Override
        public Void visit(SetFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (inst.getFieldType().getKind() == TypeKind.PRIMITIVE) {
                addConstraint(inst.getValue(), fieldType.getPrimitiveType());
            } else {
                // TODO
            }
            return null;
        }

        @Override
        public Void visit(JumpIfInst inst, Void arg) {
            addConstraint(inst.getCond(), PrimitiveType.BOOLEAN);
            return null;
        }

        @Override
        public Void visit(InstanceOfInst inst, Void arg) {
            addConstraint(inst.getResult(), PrimitiveType.BOOLEAN);
            // TODO
            return null;
        }

        @Override
        public Void visit(MonitorEnterInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(MonitorExitInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(NewArrayInst inst, Void arg) {
            for (Variable dimVar : inst.getDimensions()) {
                addConstraint(dimVar, PrimitiveType.INTEGER);
            }
            return null;
        }

        @Override
        public Void visit(NewObjectInst inst, Void arg) {
            ClassName superclass = classNameManager.newClassName(Object.class.getName());
            ClassName subclass = inst.getType().getClassName();
            // TODO
            return null;
        }

        @Override
        public Void visit(ReturnInst inst, Void arg) {
            if (methodReturnType != null) {
                if (methodReturnType.getKind() == TypeKind.PRIMITIVE) {
                    if (methodReturnType.getPrimitiveType() != PrimitiveType.VOID) {
                        addConstraint(inst.getVar(), methodReturnType.getPrimitiveType());
                    }
                } else {
                    // TODO
                }
            }
            return null;
        }

        @Override
        public Void visit(SwitchInst inst, Void arg) {
            addConstraint(inst.getIndex(), PrimitiveType.INTEGER);
            return null;
        }

        @Override
        public Void visit(ThrowInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(UnaryInst inst, Void arg) {
            switch (inst.getOperator()) {
                case MINUS:
                    addConstraint(inst.getResult(), ARITHMETIC_TYPES);
                    addConstraint(inst.getResult(), inst.getVar());
                    break;

                case NOT:
                    addConstraint(inst.getResult(), PrimitiveType.BOOLEAN);
                    addConstraint(inst.getVar(), PrimitiveType.BOOLEAN);
                    break;
            }
            return null;
        }

        @Override
        public Void visit(ChoiceInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(PhiInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(GetStaticFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (inst.getFieldType().getKind() == TypeKind.PRIMITIVE) {
                addConstraint(inst.getResult(), fieldType.getPrimitiveType());
            } else {
                // TODO
            }
            return null;
        }

        @Override
        public Void visit(SetStaticFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (inst.getFieldType().getKind() == TypeKind.PRIMITIVE) {
                addConstraint(inst.getValue(), fieldType.getPrimitiveType());
            } else {
                // TODO
            }
            return null;
        }

    }

    private final ControlFlowGraph cfg;

    private final JavaType thisType;

    private final JavaType methodReturnType;

    private final List<Variable> methodArgs;

    private final ClassNameManager classNameManager;

    private final Visitor visitor = new Visitor();

    private final ClassName javaLangObjectClassName;

    private final ClassName javaLangStringClassName;

    private final ClassName javaLangClassClassName;

    private Model model;

    private int cstID = 0;

    private final Map<VariableID, SetVariable> variables = new HashMap<VariableID, SetVariable>();

    private final TypeHierarchyIndexer indexer = new TypeHierarchyIndexer();

    public LocalVariableTypeAnalyser(ControlFlowGraph cfg, JavaType thisType,
                                     JavaType methodReturnType, List<Variable> methodArgs,
                                     ClassNameManager classNameManager) {
        this.cfg = cfg;
        this.thisType = thisType;
        this.methodReturnType = methodReturnType;
        this.methodArgs = methodArgs;
        this.classNameManager = classNameManager;
        javaLangObjectClassName = classNameManager.newClassName(Object.class.getName());
        javaLangStringClassName = classNameManager.newClassName(String.class.getName());
        javaLangClassClassName = classNameManager.newClassName(Class.class.getName());
    }

    private void addConstraint(Variable var1, Variable var2) {
        SetVariable set1 = variables.get(var1.getID());
        if (set1 == null) {
            set1 = Choco.makeSetVar(var1.getID().toString(),
                                    indexer.getFirstIndex(),
                                    indexer.getLastIndex());
            variables.put(var1.getID(), set1);
        }
        SetVariable set2 = variables.get(var2.getID());
        if (set2 == null) {
            set2 = Choco.makeSetVar(var2.getID().toString(),
                                    indexer.getFirstIndex(),
                                    indexer.getLastIndex());
            variables.put(var2.getID(), set2);
        }
        model.addConstraints(Choco.eq(set1, set2));
    }

    private void addConstraint(Variable var, PrimitiveType... types) {
        int[] valuesArray = new int[types.length];
        for (int i = 0; i < types.length; i++) {
            valuesArray[i] = indexer.getIndex(JavaType.newPrimitiveType(types[i]));
        }
        SetVariable cst = Choco.makeSetVar("c" + cstID++, valuesArray);
        SetVariable set = variables.get(var.getID());
        if (set == null) {
            set = Choco.makeSetVar(var.getID().toString(),
                                   indexer.getFirstPrimitiveTypeIndex(),
                                   indexer.getLastPrimitiveTypeIndex());
            variables.put(var.getID(), set);
        }
        model.addConstraints(Choco.eq(set, cst));
    }

    public void analyse() {
        model = new CPModel();

        // this type constraint
        if (thisType != null) {
            // TODO
        }
        // method arguments constraints
        for (Variable varArg : methodArgs) {
            JavaType varType = varArg.getType();
            if (varType.getKind() == TypeKind.PRIMITIVE) {
                addConstraint(varArg, varType.getPrimitiveType());
            } else {
                // TODO
            }
        }
        // local variables constraints
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            bb.getInstructions().accept(visitor, null);
        }

        LOGGER.log(Level.FINEST, model.constraintsToString());

        Solver s = new CPSolver();
        s.read(model);
        if (Boolean.FALSE.equals(s.solve())) {
            throw new ABCDException("Type analysis failed");
        }

        LOGGER.log(Level.FINEST, "Solution :");
        Map<VariableID, JavaType> types = new HashMap<VariableID, JavaType>();
        for (Map.Entry<VariableID, SetVariable> entry : variables.entrySet()) {
            VariableID ID = entry.getKey();
            SetVariable var = entry.getValue();
            Collection<JavaType> possibleTypes = indexer.getTypes(s.getVar(var).getValue());
            LOGGER.log(Level.FINEST, "{0} = {1}", new Object[] {ID, possibleTypes});
            if (possibleTypes.size() == 1) {
                types.put(ID, possibleTypes.iterator().next());
            }
        }

        for (BasicBlock block : cfg.getBasicBlocks()) {
            for (IRInst inst : block.getInstructions()) {
                if (inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    def.setType(types.get(def.getID()));
                }
                for (Variable use : inst.getUses()) {
                    use.setType(types.get(use.getID()));
                }
            }
        }
    }
}
