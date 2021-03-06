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
import com.google.common.primitives.Ints;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.type.ClassNameManager;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.type.TypeHierarchyIndexer;
import fr.jamgotchian.abcd.core.type.TypeKind;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariableTypeAnalyser {

    private static final Logger LOGGER
            = LoggerFactory.getLogger(LocalVariableTypeAnalyser.class);

    private static final JavaType[] PRIMITIVE_TYPES = { JavaType.BOOLEAN,
                                                        JavaType.CHAR,
                                                        JavaType.BYTE,
                                                        JavaType.SHORT,
                                                        JavaType.INT,
                                                        JavaType.LONG,
                                                        JavaType.FLOAT,
                                                        JavaType.DOUBLE };

    private static final JavaType[] ARITHMETIC_TYPES = { JavaType.INT,
                                                         JavaType.LONG,
                                                         JavaType.BYTE,
                                                         JavaType.SHORT,
                                                         JavaType.FLOAT,
                                                         JavaType.DOUBLE };

    private enum ReferenceConstraintType {
        SUPER_CLASS_CONSTRAINT,
        SUB_CLASS_CONSTRAINT
    }

    private class TypeIndexer extends EmptyIRInstVisitor<Void, Void> {

        @Override
        public Void visit(NewArrayInst inst, Void arg) {
            indexer.addIndex(JavaType.newArrayType(inst.getElementType(), inst.getDimensionCount()));
            return null;
        }

        @Override
        public Void visit(NewObjectInst inst, Void arg) {
            indexer.addIndex(inst.getType());
            return null;
        }

        @Override
        public Void visit(CastInst inst, Void arg) {
            indexer.addIndex(inst.getCastType());
            return null;
        }

        private void visitCallableInst(CallableInst inst) {
            JavaType returnType = inst.getSignature().getReturnType();
            if (returnType.getKind() == TypeKind.REFERENCE) {
                indexer.addIndex(returnType);
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
        public Void visit(GetFieldInst inst, Void arg) {
            indexer.addIndex(inst.getFieldType());
            return null;
        }

        @Override
        public Void visit(SetFieldInst inst, Void arg) {
            indexer.addIndex(inst.getFieldType());
            return null;
        }

        @Override
        public Void visit(GetStaticFieldInst inst, Void arg) {
            indexer.addIndex(inst.getFieldType());
            return null;
        }

        @Override
        public Void visit(SetStaticFieldInst inst, Void arg) {
            indexer.addIndex(inst.getFieldType());
            return null;
        }
    }

    private class ConstraintsAdder extends EmptyIRInstVisitor<Void, Void> {

        @Override
        public Void visit(IRInstSeq seq, Void arg) {
            return super.visit(seq, arg);
        }

        @Override
        public Void visit(ArrayLengthInst inst, Void arg) {
            addPrimConstraint(inst.getResult(), JavaType.INT);
            return null;
        }

        @Override
        public Void visit(AssignConstInst inst, Void arg) {
            if (inst.getConst() instanceof ByteConst) {
                byte value = ((ByteConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addPrimConstraint(inst.getResult(), JavaType.BYTE,
                                                        JavaType.CHAR,
                                                        JavaType.BOOLEAN);
                } else {
                    addPrimConstraint(inst.getResult(), JavaType.BYTE,
                                                        JavaType.CHAR);
                }
            } else if (inst.getConst() instanceof ClassConst) {
                addRefConstraint(inst.getResult(),
                                 JavaType.newRefType(Class.class, classNameManager),
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            } else if (inst.getConst() instanceof DoubleConst) {
                addPrimConstraint(inst.getResult(), JavaType.DOUBLE);
            } else if (inst.getConst() instanceof FloatConst) {
                addPrimConstraint(inst.getResult(), JavaType.FLOAT);
            } else if (inst.getConst() instanceof IntConst) {
                int value = ((IntConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addPrimConstraint(inst.getResult(), JavaType.INT,
                                                        JavaType.BOOLEAN);
                } else {
                    addPrimConstraint(inst.getResult(), JavaType.INT);
                }
            } else if (inst.getConst() instanceof LongConst) {
                long value = ((LongConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addPrimConstraint(inst.getResult(), JavaType.LONG,
                                                        JavaType.BOOLEAN);
                } else {
                    addPrimConstraint(inst.getResult(), JavaType.LONG);
                }
            } else if (inst.getConst() instanceof NullConst) {
                // could be every reference type...
            } else if (inst.getConst() instanceof ShortConst) {
                short value = ((ShortConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    addPrimConstraint(inst.getResult(), JavaType.SHORT,
                                                        JavaType.BOOLEAN);
                } else {
                    addPrimConstraint(inst.getResult(), JavaType.SHORT);
                }
            } else if (inst.getConst() instanceof StringConst) {
                addRefConstraint(inst.getResult(),
                                 JavaType.newRefType(String.class, classNameManager),
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
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
                    addPrimConstraint(inst.getResult(), ARITHMETIC_TYPES);
                    addConstraint(inst.getLeft(), inst.getRight());
                    addConstraint(inst.getResult(), inst.getLeft());
                    break;

                case DIV:
                    addPrimConstraint(inst.getResult(), JavaType.FLOAT);
                    addPrimConstraint(inst.getLeft(), ARITHMETIC_TYPES);
                    addPrimConstraint(inst.getRight(), ARITHMETIC_TYPES);
                    break;

                case LT:
                case LE:
                case GT:
                case GE:
                    addPrimConstraint(inst.getResult(), JavaType.BOOLEAN);
                    addPrimConstraint(inst.getLeft(), ARITHMETIC_TYPES);
                    addConstraint(inst.getLeft(), inst.getRight());
                    break;

                case EQ:
                case NE:
                    addPrimConstraint(inst.getResult(), JavaType.BOOLEAN);
                    addPrimConstraint(inst.getResult(), PRIMITIVE_TYPES);
                    break;

                case OR:
                case AND:
                    addPrimConstraint(inst.getResult(), JavaType.BOOLEAN);
                    addPrimConstraint(inst.getLeft(), JavaType.BOOLEAN);
                    addPrimConstraint(inst.getRight(), JavaType.BOOLEAN);
                    break;
            }
            return null;
        }

        private void visitCallableInst(CallableInst inst) {
            MethodSignature signature = inst.getSignature();
            JavaType returnType = signature.getReturnType();
            if (returnType.getKind() == TypeKind.PRIMITIVE) {
                if (!returnType.equals(JavaType.VOID)) {
                    addPrimConstraint(inst.getResult(), returnType);
                }
            } else {
                addRefConstraint(inst.getResult(), returnType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
            for (int i = 0; i < signature.getArgumentTypes().size(); i++) {
                JavaType argType = signature.getArgumentTypes().get(i);
                Variable argVar = inst.getArguments().get(i);
                if (argType.getKind() == TypeKind.PRIMITIVE) {
                    addPrimConstraint(argVar, argType);
                } else {
                    addRefConstraint(argVar, argType,
                                     ReferenceConstraintType.SUB_CLASS_CONSTRAINT);
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
                addPrimConstraint(inst.getResult(), castType);
            } else {
                addRefConstraint(inst.getResult(), castType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
            return null;
        }

        @Override
        public Void visit(ConditionalInst inst, Void arg) {
            addPrimConstraint(inst.getCond(), JavaType.BOOLEAN);
            addConstraint(inst.getThen(), inst.getElse());
            addConstraint(inst.getResult(), inst.getThen());
            return null;
        }

        @Override
        public Void visit(GetArrayInst inst, Void arg) {
            addPrimConstraint(inst.getIndex(), JavaType.INT);
            return null;
        }

        @Override
        public Void visit(SetArrayInst inst, Void arg) {
            addPrimConstraint(inst.getIndex(), JavaType.INT);
            return null;
        }

        @Override
        public Void visit(GetFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (fieldType.getKind() == TypeKind.PRIMITIVE) {
                addPrimConstraint(inst.getResult(), fieldType);
            } else {
                addRefConstraint(inst.getResult(), fieldType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
            return null;
        }

        @Override
        public Void visit(SetFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (inst.getFieldType().getKind() == TypeKind.PRIMITIVE) {
                addPrimConstraint(inst.getValue(), fieldType);
            } else {
                addRefConstraint(inst.getValue(), fieldType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
            return null;
        }

        @Override
        public Void visit(JumpIfInst inst, Void arg) {
            addPrimConstraint(inst.getCond(), JavaType.BOOLEAN);
            return null;
        }

        @Override
        public Void visit(InstanceOfInst inst, Void arg) {
            addPrimConstraint(inst.getResult(), JavaType.BOOLEAN);
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
            addRefConstraint(inst.getResult(),
                             JavaType.newArrayType(inst.getElementType(), inst.getDimensionCount()),
                             ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            for (Variable dimVar : inst.getDimensions()) {
                addPrimConstraint(dimVar, JavaType.INT);
            }
            return null;
        }

        @Override
        public Void visit(NewObjectInst inst, Void arg) {
            addRefConstraint(inst.getResult(), inst.getType(),
                             ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            return null;
        }

        @Override
        public Void visit(ReturnInst inst, Void arg) {
            if (methodContext.getReturnType() != null) {
                if (methodContext.getReturnType().getKind() == TypeKind.PRIMITIVE) {
                    if (!methodContext.getReturnType().equals(JavaType.VOID)) {
                        addPrimConstraint(inst.getVar(), methodContext.getReturnType());
                    }
                } else {
                    // TODO
                }
            }
            return null;
        }

        @Override
        public Void visit(SwitchInst inst, Void arg) {
            addPrimConstraint(inst.getIndex(), JavaType.INT);
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
                    addPrimConstraint(inst.getResult(), ARITHMETIC_TYPES);
                    addConstraint(inst.getResult(), inst.getVar());
                    break;

                case NOT:
                    addPrimConstraint(inst.getResult(), JavaType.BOOLEAN);
                    addPrimConstraint(inst.getVar(), JavaType.BOOLEAN);
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
                addPrimConstraint(inst.getResult(), fieldType);
            } else {
                addRefConstraint(inst.getResult(), fieldType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
            return null;
        }

        @Override
        public Void visit(SetStaticFieldInst inst, Void arg) {
            JavaType fieldType = inst.getFieldType();
            if (inst.getFieldType().getKind() == TypeKind.PRIMITIVE) {
                addPrimConstraint(inst.getValue(), fieldType);
            } else {
                addRefConstraint(inst.getValue(), fieldType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
            return null;
        }

    }

    private final ControlFlowGraph cfg;

    private final MethodContext methodContext;

    private final ClassNameManager classNameManager;

    private final VariableFactory varFactory;

    private Model model;

    private int cstID = 0;

    private final Map<VariableID, SetVariable> variables = new HashMap<>();

    private final TypeHierarchyIndexer indexer;

    public LocalVariableTypeAnalyser(ControlFlowGraph cfg, MethodContext methodContext,
                                     ClassNameManager classNameManager,
                                     VariableFactory varFactory, ClassLoader classLoader) {
        this.cfg = cfg;
        this.methodContext = methodContext;
        this.classNameManager = classNameManager;
        this.varFactory = varFactory;
        indexer = new TypeHierarchyIndexer(classLoader);
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

    private void addPrimConstraint(Variable var, JavaType... types) {
        Set<Integer> values = new TreeSet<>();
        for (int i = 0; i < types.length; i++) {
            JavaType type = types[i];
            assert type.getKind() == TypeKind.PRIMITIVE;
            values.addAll(indexer.getParentIndexes(type));
        }
        SetVariable cst = Choco.makeSetVar("c" + cstID++, Ints.toArray(values));
        SetVariable set = variables.get(var.getID());
        if (set == null) {
            set = Choco.makeSetVar(var.getID().toString(),
                                   Ints.toArray(indexer.getPrimitiveTypeIndexes()));
            variables.put(var.getID(), set);
        }
        model.addConstraints(Choco.eq(set, cst));
    }

    private void addRefConstraint(Variable var, JavaType type, ReferenceConstraintType constraintType) {
        assert type.getKind() == TypeKind.REFERENCE;
        Set<Integer> values = null;
        switch (constraintType) {
            case SUPER_CLASS_CONSTRAINT:
                values = indexer.getParentIndexes(type);
                break;

            case SUB_CLASS_CONSTRAINT:
                values = indexer.getChildIndexes(type);
                break;

            default:
                throw new InternalError();
        }
        SetVariable cst = Choco.makeSetVar("c" + cstID++, Ints.toArray(values));
        SetVariable set = variables.get(var.getID());
        if (set == null) {
            set = Choco.makeSetVar(var.getID().toString(),
                                   Ints.toArray(indexer.getReferenceTypeIndexes()));
            variables.put(var.getID(), set);
        }
        model.addConstraints(Choco.eq(set, cst));
    }

    private void createIndexes() {
        if (methodContext.getThisType() != null) {
            indexer.addIndex(methodContext.getThisType());
        }
        for (Variable varArg : methodContext.getArguments()) {
            indexer.addIndex(varArg.getType());
        }
        indexer.addIndex(JavaType.newRefType(String.class, classNameManager));
        indexer.addIndex(JavaType.newRefType(Class.class, classNameManager));
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            bb.getInstructions().accept(new TypeIndexer(), null);
        }
        LOGGER.trace("Type indexes :\n{}", indexer.indexesToString());
    }

    public void analyse() {
        createIndexes();

        model = new CPModel();

        // this type constraint
        if (methodContext.getThisType() != null) {
            addRefConstraint(varFactory.create(0), methodContext.getThisType(),
                             ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
        }
        // method arguments constraints
        for (Variable varArg : methodContext.getArguments()) {
            JavaType varType = varArg.getType();
            if (varType.getKind() == TypeKind.PRIMITIVE) {
                addPrimConstraint(varArg, varType);
            } else {
                addRefConstraint(varArg, varType,
                                 ReferenceConstraintType.SUPER_CLASS_CONSTRAINT);
            }
        }
        // local variables constraints
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            bb.getInstructions().accept(new ConstraintsAdder(), null);
        }

        LOGGER.trace(model.constraintsToString());

        Solver s = new CPSolver();
        s.read(model);
        if (Boolean.FALSE.equals(s.solve())) {
            throw new ABCDException("Type analysis failed");
        }

        LOGGER.trace("Solution :");
        Map<VariableID, JavaType> types = new HashMap<>();
        for (Map.Entry<VariableID, SetVariable> entry : variables.entrySet()) {
            VariableID ID = entry.getKey();
            SetVariable var = entry.getValue();
            int[] values = s.getVar(var).getValue();
            LOGGER.trace("{} = {}", ID, Arrays.toString(values));
            if (values.length == 0) {
                LOGGER.warn("  => No solution found");
            } else {
                JavaType type = indexer.resolveType(values, classNameManager);
                LOGGER.trace("  => {}", type);
                types.put(ID, type);
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
