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
package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.AssignConstInst;
import fr.jamgotchian.abcd.core.tac.model.AssignVarInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryInst;
import fr.jamgotchian.abcd.core.tac.model.ByteConst;
import fr.jamgotchian.abcd.core.tac.model.CallMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.ClassConst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.Const;
import fr.jamgotchian.abcd.core.tac.model.DoubleConst;
import fr.jamgotchian.abcd.core.tac.model.FloatConst;
import fr.jamgotchian.abcd.core.tac.model.GetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.GetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.GotoInst;
import fr.jamgotchian.abcd.core.tac.model.InstanceOfInst;
import fr.jamgotchian.abcd.core.tac.model.IntConst;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.LabelInst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.model.LongConst;
import fr.jamgotchian.abcd.core.tac.model.MonitorEnterInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorExitInst;
import fr.jamgotchian.abcd.core.tac.model.NewArrayInst;
import fr.jamgotchian.abcd.core.tac.model.NewObjectInst;
import fr.jamgotchian.abcd.core.tac.model.NullConst;
import fr.jamgotchian.abcd.core.tac.model.PhiInst;
import fr.jamgotchian.abcd.core.tac.model.ReturnInst;
import fr.jamgotchian.abcd.core.tac.model.SetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.SetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.ShortConst;
import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.SwitchInst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.tac.model.TACInstVisitor;
import fr.jamgotchian.abcd.core.tac.model.ThrowInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;
import fr.jamgotchian.abcd.core.tac.model.VariableID;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariableTypeAnalyser implements TACInstVisitor<Boolean, Void> {

    private static final Logger logger
            = Logger.getLogger(LocalVariableTypeAnalyser.class.getName());

    private final ControlFlowGraph graph;

    private final Method method;

    private final ClassNameFactory factory;

    private final Map<VariableID, NarrowestType> types
            = new HashMap<VariableID, NarrowestType>();

    public LocalVariableTypeAnalyser(ControlFlowGraph graph, Method method,
                                     ClassNameFactory factory) {
        this.graph = graph;
        this.method = method;
        this.factory = factory;
    }

    private NarrowestType getType(VariableID ID) {
        NarrowestType type = types.get(ID);
        if (type == null) {
            type = new NarrowestType();
            types.put(ID, type);
        }
        return type;
    }

    private void printTypeTable() {
        List<String> nameColumn = new ArrayList<String>(1);
        List<String> typeColumn = new ArrayList<String>(1);
        nameColumn.add("Name");
        typeColumn.add("Type");
        for (Map.Entry<VariableID, NarrowestType> entry : types.entrySet()) {
            VariableID ID = entry.getKey();
            NarrowestType type = entry.getValue();
            nameColumn.add(ID.toString());
            if (type.get() != null) {
                typeColumn.add(type.get().getQualifiedName());
            } else {
                typeColumn.add("?");
            }
        }

        logger.log(Level.FINEST, "Variables type :\n{0}",
                ConsoleUtil.printTable(nameColumn, typeColumn));
    }

    public void analyse() {
        ClassName thisClassName = factory.newClassName(method.getClazz().getQualifiedName());
        getType(new VariableID(0)).narrow(JavaType.newRefType(thisClassName), factory);
        for (LocalVariableDeclaration arg : method.getArguments()) {
            getType(new VariableID(arg.getIndex())).narrow(arg.getType(), factory);
        }

        boolean change = true;
        while (change) {
            change = false;
            for (BasicBlock block : graph.getBasicBlocks()) {
                AnalysisData data = (AnalysisData) block.getData();
                TACInstSeq seq = new TACInstSeq(data.getInstructions());
                if (Boolean.TRUE.equals(seq.accept(this, null))) {
                    change = true;
                }
            }
        }

        printTypeTable();
    }

    public Boolean visit(TACInstSeq seq, Void arg) {
        Boolean change = Boolean.FALSE;
        for (TACInst inst : seq.getInsts()) {
            if (Boolean.TRUE.equals(inst.accept(this, arg))) {
                change = Boolean.TRUE;
            }
        }
        return change;
    }

    public Boolean visit(Variable inst, Void arg) {
        return null;
    }

    public Boolean visit(IntConst inst, Void arg) {
        return null;
    }

    public Boolean visit(LongConst inst, Void arg) {
        return null;
    }

    public Boolean visit(ByteConst inst, Void arg) {
        return null;
    }

    public Boolean visit(ShortConst inst, Void arg) {
        return null;
    }

    public Boolean visit(FloatConst inst, Void arg) {
        return null;
    }

    public Boolean visit(DoubleConst inst, Void arg) {
        return null;
    }

    public Boolean visit(StringConst inst, Void arg) {
        return null;
    }

    public Boolean visit(NullConst inst, Void arg) {
        return null;
    }

    public Boolean visit(ClassConst inst, Void arg) {
        return null;
    }

    public Boolean visit(ArrayLengthInst inst, Void arg) {
        Variable result = inst.getResult();
        getType(result.getID()).narrow(JavaType.INT, factory);
        return Boolean.FALSE;
    }

    public Boolean visit(AssignConstInst inst, Void arg) {
        Variable result = inst.getResult();
        Const value = inst.getValue();
        return getType(result.getID()).narrow(value.getType(), factory);
    }

    public Boolean visit(AssignVarInst inst, Void arg) {
        Variable result = inst.getResult();
        Variable value = inst.getValue();
        return getType(result.getID()).narrow(getType(value.getID()).get(), factory);
    }

    public Boolean visit(BinaryInst inst, Void arg) {
        Variable result = inst.getResult();
        Variable var1 = inst.getVar1();
        Variable var2 = inst.getVar2();
        switch (inst.getOperator()) {
            case PLUS:
            case MINUS:
                NarrowestType tmpType = new NarrowestType();
                tmpType.narrow(getType(var1.getID()).get(), factory);
                tmpType.narrow(getType(var2.getID()).get(), factory);
                return getType(result.getID()).narrow(tmpType.get(), factory);

            case EQ:
            case NE:
            case GT:
            case GE:
            case LT:
            case LE:
                return getType(result.getID()).narrow(JavaType.BOOLEAN, factory);
        }
        return Boolean.FALSE;
    }

    public Boolean visit(CallMethodInst inst, Void arg) {
        Variable result = inst.getResult();
        boolean change = getType(result.getID()).narrow(inst.getReturnType(), factory);
        for (int i = 0; i < inst.getArgs().size(); i++) {
            JavaType argType = inst.getArgTypes().get(i);
            Variable var = inst.getArgs().get(i);
            if (getType(var.getID()).narrow(argType, factory)) {
                change = true;
            }
        }
        return change;
    }

    public Boolean visit(CallStaticMethodInst inst, Void arg) {
        Variable result = inst.getResult();
        boolean change = getType(result.getID()).narrow(inst.getReturnType(), factory);
        for (int i = 0; i < inst.getArgs().size(); i++) {
            JavaType argType = inst.getArgTypes().get(i);
            Variable var = inst.getArgs().get(i);
            if (getType(var.getID()).narrow(argType, factory)) {
                change = true;
            }
        }
        return change;
    }

    public Boolean visit(CastInst inst, Void arg) {
        Variable result = inst.getResult();
        return getType(result.getID()).narrow(inst.getType(), factory);
    }

    public Boolean visit(ConditionalInst inst, Void arg) {
        Variable result = inst.getResult();
        Variable cond = inst.getThen();
        Variable then = inst.getCond();
        Variable _else = inst.getElse();
        boolean change = getType(cond.getID()).narrow(JavaType.BOOLEAN, factory);
        if (getType(result.getID()).narrow(getType(then.getID()).get(), factory)) {
            change = true;
        }
        if (getType(result.getID()).narrow(getType(_else.getID()).get(), factory)) {
            change = true;
        }
        return change;
    }

    public Boolean visit(GetArrayInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(SetArrayInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(GetStaticFieldInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(SetStaticFieldInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(GetFieldInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(SetFieldInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(GotoInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(JumpIfInst inst, Void arg) {
        Variable cond = inst.getCond();
        return getType(cond.getID()).narrow(JavaType.BOOLEAN, factory);
    }

    public Boolean visit(LabelInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(InstanceOfInst inst, Void arg) {
        Variable result = inst.getResult();
        return getType(result.getID()).narrow(JavaType.BOOLEAN, factory);
    }

    public Boolean visit(MonitorEnterInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(MonitorExitInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(NewArrayInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(NewObjectInst inst, Void arg) {
        Variable result = inst.getResult();
        return getType(result.getID()).narrow(JavaType.newRefType(inst.getClassName()), factory);
    }

    public Boolean visit(ReturnInst inst, Void arg) {
        Variable ret = inst.getVar();
        JavaType returnType = method.getReturnType();
        if (ret != null && returnType != null) {
            getType(ret.getID()).narrow(returnType, factory);
        }
        return Boolean.FALSE;
    }

    public Boolean visit(SwitchInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(ThrowInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(UnaryInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(ChoiceInst inst, Void arg) {
        return Boolean.FALSE;
    }

    public Boolean visit(PhiInst inst, Void arg) {
        return Boolean.FALSE;
    }
}
