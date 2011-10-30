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
package fr.jamgotchian.abcd.core;

import com.google.common.collect.Sets;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.LocalVariableTable;
import fr.jamgotchian.abcd.core.ir.ArrayLengthInst;
import fr.jamgotchian.abcd.core.ir.AssignConstInst;
import fr.jamgotchian.abcd.core.ir.AssignVarInst;
import fr.jamgotchian.abcd.core.ir.BinaryInst;
import fr.jamgotchian.abcd.core.ir.CallMethodInst;
import fr.jamgotchian.abcd.core.ir.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.ir.CastInst;
import fr.jamgotchian.abcd.core.ir.ChoiceInst;
import fr.jamgotchian.abcd.core.ir.ConditionalInst;
import fr.jamgotchian.abcd.core.ir.DefInst;
import fr.jamgotchian.abcd.core.ir.GetArrayInst;
import fr.jamgotchian.abcd.core.ir.GetFieldInst;
import fr.jamgotchian.abcd.core.ir.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.ir.InstanceOfInst;
import fr.jamgotchian.abcd.core.ir.JumpIfInst;
import fr.jamgotchian.abcd.core.ir.MethodSignature;
import fr.jamgotchian.abcd.core.ir.MonitorEnterInst;
import fr.jamgotchian.abcd.core.ir.MonitorExitInst;
import fr.jamgotchian.abcd.core.ir.NewArrayInst;
import fr.jamgotchian.abcd.core.ir.NewObjectInst;
import fr.jamgotchian.abcd.core.ir.ReturnInst;
import fr.jamgotchian.abcd.core.ir.SetArrayInst;
import fr.jamgotchian.abcd.core.ir.SetFieldInst;
import fr.jamgotchian.abcd.core.ir.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.ir.SwitchInst;
import fr.jamgotchian.abcd.core.ir.IRInst;
import fr.jamgotchian.abcd.core.ir.IRInstFactory;
import fr.jamgotchian.abcd.core.ir.IRInstSeq;
import fr.jamgotchian.abcd.core.ir.UnaryInst;
import fr.jamgotchian.abcd.core.ir.Variable;
import fr.jamgotchian.abcd.core.ir.VariableID;
import fr.jamgotchian.abcd.core.ir.EmptyIRInstVisitor;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
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

    private static final Logger logger
            = Logger.getLogger(LocalVariableTypeAnalyser.class.getName());

    private final ControlFlowGraph graph;

    private final Method method;

    private final ClassNameFactory classNameFactory;

    private final IRInstFactory instFactory;

    private final Map<VariableID, Set<JavaType>> possibleTypes
            = new HashMap<VariableID, Set<JavaType>>();

    private class Visitor extends EmptyIRInstVisitor<Boolean, Void> {

        @Override
        public Boolean visit(IRInstSeq seq, Void arg) {
            Boolean change = Boolean.FALSE;
            for (IRInst inst : seq) {
            if (Boolean.TRUE.equals(inst.accept(this, arg))) {
                    change = Boolean.TRUE;
                }
            }
            return change;
        }

        private Boolean infereTypes(Set<JavaType> possibleLeftTypes,
                                    JavaType... possibleRightTypes) {
            return infereTypes(possibleLeftTypes, Sets.newHashSet(possibleRightTypes));
        }

        private Boolean infereTypes(Set<JavaType> possibleLeftTypes,
                                    Set<JavaType> possibleRightTypes) {
            if (possibleLeftTypes.isEmpty()) {
                if (possibleRightTypes.isEmpty()) {
                    return Boolean.FALSE;
                } else {
                    possibleLeftTypes.addAll(possibleRightTypes);
                    return Boolean.TRUE;
                }
            } else {
                Set<JavaType> newPossibleLeftTypes
                        = fr.jamgotchian.abcd.core.util.Sets.intersection(possibleLeftTypes, possibleRightTypes);
                if (newPossibleLeftTypes.isEmpty()) {
                    newPossibleLeftTypes
                            = JavaType.widen(possibleLeftTypes, possibleRightTypes, classNameFactory);
                }
                if (Collections3.sameContent(possibleLeftTypes, newPossibleLeftTypes)) {
                    return Boolean.FALSE;
                } else {
                    possibleLeftTypes.clear();
                    possibleLeftTypes.addAll(newPossibleLeftTypes);
                    return Boolean.TRUE;
                }
            }
        }

        @Override
        public Boolean visit(AssignConstInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               inst.getConst().getPossibleTypes());
        }

        @Override
        public Boolean visit(AssignVarInst inst, Void arg) {
            boolean change
                    = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                  getPossibleTypes(inst.getValue().getID()));

            change |= infereTypes(getPossibleTypes(inst.getValue().getID()),
                                  getPossibleTypes(inst.getResult().getID()));

            return change;
        }

        @Override
        public Boolean visit(UnaryInst inst, Void arg) {
            switch (inst.getOperator()) {
                case MINUS: {
                    boolean change
                            = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                          JavaType.ARITHMETIC_TYPES);

                    change |= infereTypes(getPossibleTypes(inst.getVar().getID()),
                                          JavaType.ARITHMETIC_TYPES);

                    change |= infereTypes(getPossibleTypes(inst.getResult().getID()),
                                          getPossibleTypes(inst.getVar().getID()));

                    change |= infereTypes(getPossibleTypes(inst.getVar().getID()),
                                          getPossibleTypes(inst.getResult().getID()));

                    return change;
                }

                default:
                    return Boolean.FALSE;
            }
        }

        @Override
        public Boolean visit(BinaryInst inst, Void arg) {
            switch (inst.getOperator()) {
                case AND:
                case OR: {
                    boolean change
                            = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                          JavaType.BOOLEAN);

                    change |= infereTypes(getPossibleTypes(inst.getLeft().getID()),
                                          JavaType.BOOLEAN);

                    change |= infereTypes(getPossibleTypes(inst.getRight().getID()),
                                          JavaType.BOOLEAN);

                    return change;
                }

                case EQ:
                case NE:
                case GE:
                case GT:
                case LE:
                case LT: {
                    boolean change
                            = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                          JavaType.BOOLEAN);

                    change |= infereTypes(getPossibleTypes(inst.getLeft().getID()),
                                          getPossibleTypes(inst.getRight().getID()));

                    change |= infereTypes(getPossibleTypes(inst.getRight().getID()),
                                          getPossibleTypes(inst.getLeft().getID()));

                    return change;
                }

                case PLUS:
                case MINUS:
                case MUL: {
                    // the result type is an arithmetic type
                    boolean change
                            = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                          JavaType.ARITHMETIC_TYPES);

                    change |= infereTypes(getPossibleTypes(inst.getLeft().getID()),
                                          JavaType.ARITHMETIC_TYPES);

                    change |= infereTypes(getPossibleTypes(inst.getRight().getID()),
                                          JavaType.ARITHMETIC_TYPES);

                    change |= infereTypes(getPossibleTypes(inst.getLeft().getID()),
                                          getPossibleTypes(inst.getRight().getID()));

                    change |= infereTypes(getPossibleTypes(inst.getRight().getID()),
                                          getPossibleTypes(inst.getLeft().getID()));

                    change |= infereTypes(getPossibleTypes(inst.getResult().getID()),
                                          getPossibleTypes(inst.getLeft().getID()));

                    return change;
                }

                default:
                    return Boolean.FALSE;
            }
        }

        @Override
        public Boolean visit(ConditionalInst inst, Void arg) {
            boolean change
                    = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                  getPossibleTypes(inst.getThen().getID()));
            change |= infereTypes(getPossibleTypes(inst.getResult().getID()),
                                  getPossibleTypes(inst.getElse().getID()));
            change |= infereTypes(getPossibleTypes(inst.getThen().getID()),
                                  getPossibleTypes(inst.getResult().getID()));
            change |= infereTypes(getPossibleTypes(inst.getElse().getID()),
                                  getPossibleTypes(inst.getResult().getID()));
            change |= infereTypes(getPossibleTypes(inst.getCond().getID()),
                                  JavaType.BOOLEAN);

            return change;
        }

        @Override
        public Boolean visit(GetFieldInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               inst.getFieldType());
        }

        @Override
        public Boolean visit(SetFieldInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getValue().getID()),
                               inst.getFieldType());
        }

        @Override
        public Boolean visit(GetStaticFieldInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               inst.getFieldType());
        }

        @Override
        public Boolean visit(SetStaticFieldInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getValue().getID()),
                               inst.getFieldType());
        }

        @Override
        public Boolean visit(NewObjectInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               inst.getType());
        }

        @Override
        public Boolean visit(ArrayLengthInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               JavaType.INT);
        }

        @Override
        public Boolean visit(CastInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               inst.getCastType());
        }

        @Override
        public Boolean visit(InstanceOfInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getResult().getID()),
                               JavaType.BOOLEAN);
        }

        @Override
        public Boolean visit(CallMethodInst inst, Void arg) {
            MethodSignature signature = inst.getSignature();

            boolean change = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                         signature.getReturnType());

            for (int i = 0; i < signature.getArgumentTypes().size(); i++) {
                JavaType argType = signature.getArgumentTypes().get(i);
                Variable argVar = inst.getArguments().get(i);
                change |= infereTypes(getPossibleTypes(argVar.getID()), argType);
            }

            return change;
        }

        @Override
        public Boolean visit(CallStaticMethodInst inst, Void arg) {
            MethodSignature signature = inst.getSignature();

            boolean change = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                         signature.getReturnType());

            for (int i = 0; i < signature.getArgumentTypes().size(); i++) {
                JavaType argType = signature.getArgumentTypes().get(i);
                Variable argVar = inst.getArguments().get(i);
                change |= infereTypes(getPossibleTypes(argVar.getID()), argType);
            }

            return change;
        }

        @Override
        public Boolean visit(JumpIfInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getCond().getID()),
                               JavaType.BOOLEAN);
        }

        @Override
        public Boolean visit(SwitchInst inst, Void arg) {
            return infereTypes(getPossibleTypes(inst.getIndex().getID()),
                               JavaType.INT);
        }

        @Override
        public Boolean visit(MonitorEnterInst inst, Void arg) {
            return Boolean.FALSE;
        }

        @Override
        public Boolean visit(MonitorExitInst inst, Void arg) {
            return Boolean.FALSE;
        }

        @Override
        public Boolean visit(ChoiceInst inst, Void arg) {
            throw new ABCDException("Should not have choice instruction during type analysis");
        }

        @Override
        public Boolean visit(NewArrayInst inst, Void arg) {
            boolean change
                    = infereTypes(getPossibleTypes(inst.getResult().getID()),
                                  JavaType.newArrayType(inst.getType(), inst.getDimensions().size()));
            for (Variable dimVar : inst.getDimensions()) {
                change |= infereTypes(getPossibleTypes(dimVar.getID()), JavaType.INT);
            }
            return change;
        }

        @Override
        public Boolean visit(GetArrayInst inst, Void arg) {
            boolean change
                    = infereTypes(getPossibleTypes(inst.getIndex().getID()),
                                  JavaType.INT);
            Set<JavaType> possibleEltTypes = new HashSet<JavaType>();
            for (JavaType type : getPossibleTypes(inst.getArray().getID())) {
                possibleEltTypes.add(type.getArrayElementType());
            }
            change |= infereTypes(getPossibleTypes(inst.getResult().getID()),
                                  possibleEltTypes);
            return change;
        }

        @Override
        public Boolean visit(SetArrayInst inst, Void arg) {
            boolean change
                    = infereTypes(getPossibleTypes(inst.getIndex().getID()),
                                  JavaType.INT);
            Set<JavaType> possibleEltTypes = new HashSet<JavaType>();
            for (JavaType type : getPossibleTypes(inst.getArray().getID())) {
                possibleEltTypes.add(type.getArrayElementType());
            }
            change |= infereTypes(getPossibleTypes(inst.getValue().getID()),
                                  possibleEltTypes);
            return change;
        }

        @Override
        public Boolean visit(ReturnInst inst, Void arg) {
            if (inst.getVar() != null && method.getReturnType() != null) {
                return infereTypes(getPossibleTypes(inst.getVar().getID()),
                                   method.getReturnType());
            } else {
                return Boolean.FALSE;
            }
        }
    }

    private final Visitor visitor = new Visitor();

    public LocalVariableTypeAnalyser(ControlFlowGraph graph, Method method,
                                     ClassNameFactory classNameFactory,
                                     IRInstFactory instFactory) {
        this.graph = graph;
        this.method = method;
        this.classNameFactory = classNameFactory;
        this.instFactory = instFactory;
    }

    private Set<JavaType> getPossibleTypes(VariableID ID) {
        Set<JavaType> types = possibleTypes.get(ID);
        if (types == null) {
            types = new HashSet<JavaType>();
            possibleTypes.put(ID, types);
        }
        return types;
    }

    private void printTypeTable() {
        List<String> indexColumn = new ArrayList<String>(1);
        List<String> typeColumn = new ArrayList<String>(1);
        indexColumn.add("Index");
        typeColumn.add("Type");
        for (Map.Entry<VariableID, Set<JavaType>> entry : possibleTypes.entrySet()) {
            VariableID ID = entry.getKey();
            Set<JavaType> types = entry.getValue();
            indexColumn.add(ID.toString());
            StringBuilder builder = new StringBuilder();
            builder.append("[");
            for (Iterator<JavaType> it = types.iterator(); it.hasNext();) {
                builder.append(it.next().getQualifiedName());
                if (it.hasNext()) {
                    builder.append(", ");
                }
            }
            builder.append("]");
            typeColumn.add(builder.toString());
        }

        logger.log(Level.FINEST, "Variable types :\n{0}",
                ConsoleUtil.printTable(indexColumn, typeColumn));
    }

    private void printVariableName(Set<Variable> variables) {
        List<String> indexColumn = new ArrayList<String>(1);
        List<String> positionColumn = new ArrayList<String>(1);
        List<String> nameColumn = new ArrayList<String>(1);
        indexColumn.add("Index");
        positionColumn.add("Position");
        nameColumn.add("Name");
        for (Variable v : variables) {
            indexColumn.add(v.getID().toString());
            positionColumn.add(Integer.toString(v.getPosition()));
            nameColumn.add(v.getName() != null ? v.getName() : "<undefined>");
        }
        logger.log(Level.FINEST, "Variable names :\n{0}",
                ConsoleUtil.printTable(indexColumn, positionColumn, nameColumn));
    }


    public void analyse() {
        // find type of this
        ClassName thisClassName = classNameFactory.newClassName(method.getClazz().getQualifiedName());
        getPossibleTypes(new VariableID(0)).add(JavaType.newRefType(thisClassName));

        // and types of method parameters
        for (LocalVariableDeclaration decl : method.getArguments()) {
            LocalVariable var = decl.getVariable();
            getPossibleTypes(var.getID()).add(decl.getType());
        }

        boolean change = true;
        while (change) {
            change = false;
            for (BasicBlock block : graph.getBasicBlocks()) {
                if (Boolean.TRUE.equals(block.getInstructions().accept(visitor, null))) {
                    change = true;
                }
            }
        }

        // check that all variable have been types correctly
        for (BasicBlock bb : graph.getBasicBlocks()) {
            for (IRInst inst : bb.getInstructions()) {
                if (inst instanceof DefInst) {
                    Variable v = ((DefInst) inst).getResult();
                    assert v.getType() == null;
                    Set<JavaType> types = getPossibleTypes(v.getID());
                    if (types.size() == 1) {
                        v.setType(types.iterator().next());
                    } else {
                        if (types.isEmpty()) {
                            logger.log(Level.WARNING, "Unable to find type of variable {0}",
                                    v.getID());
                        } else {
                            logger.log(Level.WARNING, "Multiple type found for variable {0} : {1}",
                                new Object[] {v.getID(), types});
                        }
                    }
                }
            }
        }

        printTypeTable();

        // assign a name to each variable
        LocalVariableTable table = graph.getLocalVariableTable();
        Set<Variable> variables = new HashSet<Variable>();
        for (BasicBlock block : graph.getBasicBlocks()) {
            for (IRInst inst : block.getInstructions()) {
                if (inst instanceof DefInst) {
                    Variable def = ((DefInst) inst).getResult();
                    if (!def.isTemporary()) {
                        variables.add(def);
                        def.setName(table.getName(def.getIndex(), def.getPosition()));
                    }
                }
                for (Variable use : inst.getUses()) {
                    if (!use.isTemporary()) {
                        variables.add(use);
                        use.setName(table.getName(use.getIndex(), use.getPosition()));
                    }
                }
            }
        }

        printVariableName(variables);
    }
}
