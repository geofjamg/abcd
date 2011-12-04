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

import com.google.common.base.Objects;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.type.JavaType.PrimitiveType;
import fr.jamgotchian.abcd.core.type.TypeKind;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
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

    private final ControlFlowGraph graph;

    private final JavaType thisType;

    private final JavaType methodReturnType;

    private final List<Variable> methodArgs;

    private final ClassNameFactory classNameFactory;

    private static interface TypeVariableOrConstant {
    }

    private static interface TypeConstant extends TypeVariableOrConstant {
    }

    private static class TypeVariable implements TypeVariableOrConstant {

        private final VariableID ID;

        private TypeVariable(VariableID ID) {
            this.ID = ID;
        }

        public VariableID getID() {
            return ID;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof TypeVariable) {
                return ID.equals(((TypeVariable) obj).ID);
            } else {
                return false;
            }
        }

        @Override
        public int hashCode() {
            return ID.hashCode();
        }

        @Override
        public String toString() {
            return "type(" + ID.toString() + ")";
        }
    }

    private static class ReferenceTypeConstant implements TypeConstant  {

        private final ClassName superclass;

        private final ClassName subclass;

        private ReferenceTypeConstant(ClassName superclass, ClassName subclass) {
            this.superclass = superclass;
            this.subclass = subclass;
        }

        public ClassName getSuperclass() {
            return superclass;
        }

        public ClassName getSubclass() {
            return subclass;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof ReferenceTypeConstant) {
                ReferenceTypeConstant other = (ReferenceTypeConstant) obj;
                return Objects.equal(superclass, other.superclass)
                        && Objects.equal(subclass, other.subclass);
            } else {
                return false;
            }
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(superclass, subclass);
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            if (superclass != null) {
                builder.append("[").append(superclass.getQualifiedName());
            } else {
                builder.append("]-\u221E");
            }
            builder.append("; ");
            if (subclass != null) {
                builder.append(subclass.getQualifiedName()).append("]");
            } else {
                builder.append("+\u221E[");
            }
            return builder.toString();
        }
    }

    private static class PrimitiveTypeConstant implements TypeConstant {

        private final Set<PrimitiveType> types;

        private PrimitiveTypeConstant(Set<PrimitiveType> types) {
            this.types = types;
        }

        private PrimitiveTypeConstant(PrimitiveType first, PrimitiveType... rest) {
            this.types = EnumSet.of(first, rest);
        }

        public Set<PrimitiveType> getTypes() {
            return types;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof PrimitiveTypeConstant) {
                PrimitiveTypeConstant other = (PrimitiveTypeConstant) obj;
                return types.equals(other.types);
            } else {
                return false;
            }
        }

        @Override
        public int hashCode() {
            return types.hashCode();
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            builder.append("{");
            for (Iterator<PrimitiveType> it = types.iterator(); it.hasNext(); ) {
                builder.append(it.next());
                if (it.hasNext()) {
                    builder.append(", ");
                }
            }
            builder.append("}");
            return builder.toString();
        }
    }

    private static class TypeConstraint {

        private final TypeVariable left;

        private TypeVariableOrConstant right;

        private TypeConstraint(TypeVariable left, TypeVariableOrConstant right) {
            this.left = left;
            this.right = right;
        }

        public TypeVariable getLeft() {
            return left;
        }

        public TypeVariableOrConstant getRight() {
            return right;
        }

        public void setRight(TypeVariableOrConstant right) {
            this.right = right;
        }

        @Override
        public String toString() {
            return left + " = " + right;
        }
    }

    private final List<TypeConstraint> constaints = new ArrayList<TypeConstraint>();

    private class Visitor extends EmptyIRInstVisitor<Void, Void> {

        @Override
        public Void visit(IRInstSeq seq, Void arg) {
            return super.visit(seq, arg);
        }

        @Override
        public Void visit(ArrayLengthInst inst, Void arg) {
            constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                              new PrimitiveTypeConstant(PrimitiveType.INTEGER)));
            return null;
        }

        @Override
        public Void visit(AssignConstInst inst, Void arg) {
            TypeConstant typeCst = null;
            if (inst.getConst() instanceof ByteConst) {
                byte value = ((ByteConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.BYTE,
                                                        PrimitiveType.CHAR,
                                                        PrimitiveType.BOOLEAN);
                } else {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.BYTE,
                                                        PrimitiveType.CHAR);
                }
            } else if (inst.getConst() instanceof ClassConst) {
                ClassName subclass = classNameFactory.newClassName(Class.class.getName());
                typeCst = new ReferenceTypeConstant(javaLangClassClassName, subclass);
            } else if (inst.getConst() instanceof DoubleConst) {
                typeCst = new PrimitiveTypeConstant(PrimitiveType.DOUBLE);
            } else if (inst.getConst() instanceof FloatConst) {
                typeCst = new PrimitiveTypeConstant(PrimitiveType.FLOAT);
            } else if (inst.getConst() instanceof IntConst) {
                int value = ((IntConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.INTEGER,
                                                        PrimitiveType.BOOLEAN);
                } else {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.INTEGER);
                }
            } else if (inst.getConst() instanceof LongConst) {
                long value = ((LongConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.LONG,
                                                        PrimitiveType.BOOLEAN);
                } else {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.LONG);
                }
            } else if (inst.getConst() instanceof NullConst) {
                typeCst = new ReferenceTypeConstant(javaLangObjectClassName, null);
            } else if (inst.getConst() instanceof ShortConst) {
                short value = ((ShortConst) inst.getConst()).getValue();
                if (value == 0 || value == 1) {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.SHORT,
                                                        PrimitiveType.BOOLEAN);
                } else {
                    typeCst = new PrimitiveTypeConstant(PrimitiveType.SHORT);
                }
            } else if (inst.getConst() instanceof StringConst) {
                typeCst = new ReferenceTypeConstant(javaLangObjectClassName, javaLangStringClassName);
            } else {
                throw new InternalError();
            }
            constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                              typeCst));
            return null;
        }

        @Override
        public Void visit(AssignVarInst inst, Void arg) {
            constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                              new TypeVariable(inst.getValue().getID())));
            constaints.add(new TypeConstraint(new TypeVariable(inst.getValue().getID()),
                                              new TypeVariable(inst.getResult().getID())));
            return null;
        }

        @Override
        public Void visit(BinaryInst inst, Void arg) {
            switch (inst.getOperator()) {
                case PLUS:
                case MINUS:
                case MUL:
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                      new PrimitiveTypeConstant(JavaType.ARITHMETIC_TYPES)));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getLeft().getID()),
                                                      new TypeVariable(inst.getRight().getID())));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getRight().getID()),
                                                      new TypeVariable(inst.getLeft().getID())));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                      new TypeVariable(inst.getLeft().getID())));
                    break;

                case LT:
                case LE:
                case GT:
                case GE:
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                      new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getLeft().getID()),
                                                      new PrimitiveTypeConstant(JavaType.ARITHMETIC_TYPES)));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getRight().getID()),
                                                      new PrimitiveTypeConstant(JavaType.ARITHMETIC_TYPES)));
                    break;

                case EQ:
                case NE:
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                      new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));

                case OR:
                case AND:
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                      new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getLeft().getID()),
                                                      new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getRight().getID()),
                                                      new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
                    break;
            }
            return null;
        }

        private void visitCallableInst(CallableInst inst) {
            MethodSignature signature = inst.getSignature();
            JavaType returnType = signature.getReturnType();
            if (returnType.getKind() == TypeKind.PRIMITIVE) {
                if (returnType.getPrimitiveType() != PrimitiveType.VOID) {
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                      new PrimitiveTypeConstant(returnType.getPrimitiveType())));
                }
            } else {
                constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                  new ReferenceTypeConstant(returnType.getClassName(), null)));
            }
            for (int i = 0; i < signature.getArgumentTypes().size(); i++) {
                JavaType argType = signature.getArgumentTypes().get(i);
                Variable argVar = inst.getArguments().get(i);
                if (argType.getKind() == TypeKind.PRIMITIVE) {
                    constaints.add(new TypeConstraint(new TypeVariable(argVar.getID()),
                                                      new PrimitiveTypeConstant(argType.getPrimitiveType())));
                } else {
                    constaints.add(new TypeConstraint(new TypeVariable(argVar.getID()),
                                                      new ReferenceTypeConstant(argType.getClassName(), null)));
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
                constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                  new PrimitiveTypeConstant(castType.getPrimitiveType())));
            } else {
                constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                                  new ReferenceTypeConstant(castType.getClassName(), null)));
            }
            return null;
        }

        @Override
        public Void visit(ConditionalInst inst, Void arg) {
            constaints.add(new TypeConstraint(new TypeVariable(inst.getCond().getID()),
                                              new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
            constaints.add(new TypeConstraint(new TypeVariable(inst.getThen().getID()),
                                              new TypeVariable(inst.getElse().getID())));
            constaints.add(new TypeConstraint(new TypeVariable(inst.getThen().getID()),
                                              new TypeVariable(inst.getElse().getID())));
            constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                              new TypeVariable(inst.getThen().getID())));
            constaints.add(new TypeConstraint(new TypeVariable(inst.getThen().getID()),
                                              new TypeVariable(inst.getResult().getID())));
            return null;
        }

        @Override
        public Void visit(GetArrayInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(SetArrayInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(GetFieldInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(SetFieldInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(JumpIfInst inst, Void arg) {
            constaints.add(new TypeConstraint(new TypeVariable(inst.getCond().getID()),
                                              new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
            return null;
        }

        @Override
        public Void visit(InstanceOfInst inst, Void arg) {
            constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                              new PrimitiveTypeConstant(PrimitiveType.BOOLEAN)));
            constaints.add(new TypeConstraint(new TypeVariable(inst.getVar().getID()),
                                              new ReferenceTypeConstant(javaLangObjectClassName, null)));
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
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(NewObjectInst inst, Void arg) {
            ClassName superclass = classNameFactory.newClassName(Object.class.getName());
            ClassName subclass = inst.getType().getClassName();
            constaints.add(new TypeConstraint(new TypeVariable(inst.getResult().getID()),
                                              new ReferenceTypeConstant(superclass, subclass)));

            return null;
        }

        @Override
        public Void visit(ReturnInst inst, Void arg) {
            if (methodReturnType != null) {
                if (methodReturnType.getKind() == TypeKind.PRIMITIVE) {
                    if (methodReturnType.getPrimitiveType() != PrimitiveType.VOID) {
                        constaints.add(new TypeConstraint(new TypeVariable(inst.getVar().getID()),
                                                          new PrimitiveTypeConstant(methodReturnType.getPrimitiveType())));
                    }
                } else {
                    constaints.add(new TypeConstraint(new TypeVariable(inst.getVar().getID()),
                                                      new ReferenceTypeConstant(null, methodReturnType.getClassName())));
                }
            }
            return null;
        }

        @Override
        public Void visit(SwitchInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(ThrowInst inst, Void arg) {
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(UnaryInst inst, Void arg) {
            return super.visit(inst, arg);
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
            return super.visit(inst, arg);
        }

        @Override
        public Void visit(SetStaticFieldInst inst, Void arg) {
            return super.visit(inst, arg);
        }

    }

    private final Visitor visitor = new Visitor();
    private final ClassName javaLangObjectClassName;
    private final ClassName javaLangStringClassName;
    private final ClassName javaLangClassClassName;

    public LocalVariableTypeAnalyser(ControlFlowGraph graph, JavaType thisType,
                                     JavaType methodReturnType, List<Variable> methodArgs,
                                     ClassNameFactory classNameFactory) {
        this.graph = graph;
        this.thisType = thisType;
        this.methodReturnType = methodReturnType;
        this.methodArgs = methodArgs;
        this.classNameFactory = classNameFactory;
        javaLangObjectClassName = classNameFactory.newClassName(Object.class.getName());
        javaLangStringClassName = classNameFactory.newClassName(String.class.getName());
        javaLangClassClassName = classNameFactory.newClassName(Class.class.getName());
    }

    private void printConstraints() {
        LOGGER.log(Level.FINEST, "Type constraints :");
        for (TypeConstraint constaint : constaints) {
            LOGGER.log(Level.FINEST, constaint.toString());
        }
    }

    public void analyse() {
        // this type constraint
        if (thisType != null) {
            constaints.add(new TypeConstraint(new TypeVariable(new VariableID(0)),
                                              new ReferenceTypeConstant(thisType.getClassName(),
                                                                        thisType.getClassName())));
        }
        // method arguments constraints
        for (Variable varArg : methodArgs) {
            JavaType varType = varArg.getType();
            TypeConstant typeCst = null;
            if (varType.getKind() == TypeKind.PRIMITIVE) {
                typeCst = new PrimitiveTypeConstant(varType.getPrimitiveType());
            } else {
                typeCst = new ReferenceTypeConstant(null, varType.getClassName());
            }
            constaints.add(new TypeConstraint(new TypeVariable(varArg.getID()), typeCst));
        }
        // local variables constraints
        for (BasicBlock bb : graph.getBasicBlocks()) {
            bb.getInstructions().accept(visitor, null);
        }

        printConstraints();

        // unify constraints
        Deque<TypeConstraint> constraintStack = new ArrayDeque<TypeConstraint>();
        Deque<TypeConstraint> substitutionStack = new ArrayDeque<TypeConstraint>();
        for (TypeConstraint constraint : constaints) {
            if (constraint.getRight() instanceof TypeConstant) {
                substitutionStack.push(constraint);
            } else {
                constraintStack.push(constraint);
            }
        }
        while (substitutionStack.size() > 0) {
            TypeConstraint substitution = substitutionStack.pop();
            TypeVariable typeVar = substitution.getLeft();
            TypeConstant typeCst = (TypeConstant) substitution.getRight();
            for (Iterator<TypeConstraint> it = constraintStack.iterator(); it.hasNext();) {
                TypeConstraint constraint = it.next();
                if (constraint.getRight().equals(typeVar)) {
                    constraint.setRight(typeCst);
                    it.remove();
                    substitutionStack.push(constraint);
                }
            }
        }

        printConstraints();

        if (constraintStack.size() > 0) {
            throw new ABCDException("Type analysis failed");
        }
    }
}
