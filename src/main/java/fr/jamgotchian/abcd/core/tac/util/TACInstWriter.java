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
package fr.jamgotchian.abcd.core.tac.util;

import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;
import fr.jamgotchian.abcd.core.tac.model.AssignInst;
import fr.jamgotchian.abcd.core.tac.model.ClassConst;
import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.ByteConst;
import fr.jamgotchian.abcd.core.tac.model.DoubleConst;
import fr.jamgotchian.abcd.core.tac.model.NullConst;
import fr.jamgotchian.abcd.core.tac.model.StaticField;
import fr.jamgotchian.abcd.core.tac.model.GotoInst;
import fr.jamgotchian.abcd.core.tac.model.TACInst;
import fr.jamgotchian.abcd.core.tac.model.ReturnInst;
import fr.jamgotchian.abcd.core.tac.model.SetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.InstanceOfInst;
import fr.jamgotchian.abcd.core.tac.model.IntConst;
import fr.jamgotchian.abcd.core.tac.model.FloatConst;
import fr.jamgotchian.abcd.core.tac.model.SwitchInst;
import fr.jamgotchian.abcd.core.tac.model.GetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorEnterInst;
import fr.jamgotchian.abcd.core.tac.model.ThrowInst;
import fr.jamgotchian.abcd.core.tac.model.SetArrayInst;
import fr.jamgotchian.abcd.core.tac.model.NewObjectInst;
import fr.jamgotchian.abcd.core.tac.model.LabelInst;
import fr.jamgotchian.abcd.core.tac.model.CallStaticMethodInst;
import fr.jamgotchian.abcd.core.tac.model.LocalVariable;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
import fr.jamgotchian.abcd.core.tac.model.TemporaryVariable;
import fr.jamgotchian.abcd.core.tac.model.NewArrayInst;
import fr.jamgotchian.abcd.core.tac.model.BinaryInst;
import fr.jamgotchian.abcd.core.tac.model.MonitorExitInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstVisitor;
import fr.jamgotchian.abcd.core.tac.model.ShortConst;
import fr.jamgotchian.abcd.core.tac.model.GetFieldInst;
import fr.jamgotchian.abcd.core.tac.model.LongConst;
import fr.jamgotchian.abcd.core.tac.model.CallMethodInst;
import fr.jamgotchian.abcd.core.common.Label;
import fr.jamgotchian.abcd.core.output.CodeWriter;
import fr.jamgotchian.abcd.core.output.CodeWriterFactory;
import fr.jamgotchian.abcd.core.output.HTMLCodeWriterFactory;
import fr.jamgotchian.abcd.core.output.TextCodeWriterFactory;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TACInstWriter implements TACInstVisitor<Void, Void> {

    private static final Logger logger = Logger.getLogger(TACInstWriter.class.getName());

    public static String toString(TACInst inst, CodeWriterFactory factory) {
        Writer writer = new StringWriter();
        try {
            inst.accept(new TACInstWriter(factory.create(writer)), null);
        } finally {
            try {
                writer.close();
            } catch (IOException e) {
                logger.log(Level.SEVERE, e.toString(), e);
            }
        }
        return writer.toString();
    }

    public static String toText(TACInst inst) {
        return toString(inst, new TextCodeWriterFactory());
    }

    public static String toHTML(TACInst inst) {
        return toString(inst, new HTMLCodeWriterFactory());
    }

    public static String toString2(Iterable<TemporaryVariable> vars, CodeWriterFactory factory) {
        StringBuilder builder = new StringBuilder("[");
        Iterator<TemporaryVariable> it =  vars.iterator();
        while (it.hasNext()) {
            TemporaryVariable var = it.next();
            Writer writer = new StringWriter();
            try {
                var.accept(new TACInstWriter(factory.create(writer)), null);
            } finally {
                try {
                    writer.close();
                } catch (IOException e) {
                    logger.log(Level.SEVERE, e.toString(), e);
                }
            }
            builder.append(writer.toString());
            if (it.hasNext()) {
                builder.append(", ");
            }
        }
        builder.append("]");
        return builder.toString();
    }

    public static String toText2(Iterable<TemporaryVariable> exprs) {
        return toString2(exprs, new TextCodeWriterFactory());
    }

    public static String toHTML2(Iterable<TemporaryVariable> exprs) {
        return toString2(exprs, new HTMLCodeWriterFactory());
    }

    private final CodeWriter writer;

    private TACInstWriter(CodeWriter writer) {
        this.writer = writer;
    }

    public Void visit(List<TACInst> insts, Void arg) {
        for (TACInst inst : insts) {
            inst.accept(this, arg);
            writer.newLine();
        }
        return null;
    }

    public Void visit(LocalVariable inst, Void arg) {
        writer.write("v").write(inst.getIndex());
        return null;
    }

    public Void visit(StaticField inst, Void arg) {
        writer.write(inst.getScope()).write(".").write(inst.getFieldName());
        return null;
    }

    public Void visit(TemporaryVariable inst, Void arg) {
        writer.write("_t").write(inst.getNum());
        return null;
    }

    public Void visit(IntConst inst, Void arg) {
        writer.write(inst.getValue());
        return null;
    }

    public Void visit(LongConst inst, Void arg) {
        writer.write(inst.getValue());
        return null;
    }

    public Void visit(ByteConst inst, Void arg) {
        writer.write(inst.getValue());
        return null;
    }

    public Void visit(ShortConst inst, Void arg) {
        writer.write(inst.getValue());
        return null;
    }

    public Void visit(FloatConst inst, Void arg) {
        writer.write(inst.getValue());
        return null;
    }

    public Void visit(DoubleConst inst, Void arg) {
        writer.write(inst.getValue());
        return null;
    }

    public Void visit(StringConst inst, Void arg) {
        writer.write("\"").write(inst.getValue()).write("\"");
        return null;
    }

    public Void visit(NullConst inst, Void arg) {
        writer.write("null");
        return null;
    }

    public Void visit(ClassConst inst, Void arg) {
        writer.write(inst.getClassName()).write(".class");
        return null;
    }

    public Void visit(ArrayLengthInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace().write("arrayLength").writeSpace();
        inst.getArray().accept(this, arg);
        return null;
    }

    public Void visit(AssignInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        inst.getValue().accept(this, arg);
        return null;
    }

    public Void visit(BinaryInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        inst.getVar1().accept(this, arg);
        writer.writeSpace();
        switch (inst.getOperator()) {
            case PLUS:
                writer.write("+");
                break;

            case MINUS:
                writer.write("-");
                break;

            case MUL:
                writer.write("*");
                break;

            case DIV:
                writer.write("/");
                break;

            case REMAINDER:
                writer.write("%");
                break;

            case EQ:
                writer.write("==");
                break;

            case NE:
                writer.write("!=");
                break;

            case GE:
                writer.writeGt();
                writer.write("=");
                break;

            case GT:
                writer.writeGt();
                break;

            case LT:
                writer.writeLt();
                break;

            case LE:
                writer.writeLt();
                writer.write("=");
                break;

            case AND:
                writer.writeAmpersand();
                writer.writeAmpersand();
                break;

            case OR:
                writer.write("||");
                break;

            case XOR:
                writer.write("^");
                break;

            case SHIFT_LEFT:
                writer.write("<<");
                break;

            case SHIFT_RIGHT:
                writer.writeGt();
                writer.writeGt();
                break;

            case LOGICAL_SHIFT_RIGHT:
                writer.writeGt();
                writer.writeGt();
                writer.writeGt();
                break;

            default:
                throw new AssertionError();
        }
        writer.writeSpace();
        inst.getVar2().accept(this, arg);
        return null;
    }

    public Void visit(CallMethodInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace().writeKeyword("call").writeSpace();
        inst.getObject().accept(this, arg);
        writer.writeSpace().write(inst.getMethodName()).writeSpace();
        for (Iterator<TemporaryVariable> it = inst.getArgs().iterator(); it.hasNext();) {
            Variable argVar = it.next();
            argVar.accept(this, arg);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(CallStaticMethodInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace().writeKeyword("callstatic")
                .writeSpace().write(inst.getScope()).writeSpace().write(inst.getMethodName())
                .writeSpace();
        for (Iterator<TemporaryVariable> it = inst.getArgs().iterator(); it.hasNext();) {
            Variable argVar = it.next();
            argVar.accept(this, arg);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(CastInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace().write("(")
              .write(inst.getType()).write(")").writeSpace();
        inst.getVar().accept(this, arg);
        return null;
    }

    public Void visit(ConditionalInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        inst.getCond().accept(this, arg);
        writer.writeSpace().write("?").writeSpace();
        inst.getThen().accept(this, arg);
        writer.writeSpace().write(":").writeSpace();
        inst.getElse().accept(this, arg);
        return null;
    }

    public Void visit(GetArrayInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        inst.getArray().accept(this, arg);
        writer.write("[");
        inst.getIndex().accept(this, arg);
        writer.write("]");
        return null;
    }

    public Void visit(SetArrayInst inst, Void arg) {
        inst.getArray().accept(this, arg);
        writer.write("[");
        inst.getIndex().accept(this, arg);
        writer.write("]").writeSpace().write("=").writeSpace();
        inst.getValue().accept(this, arg);
        return null;
    }

    public Void visit(GetFieldInst inst, Void arg) {
        writer.writeKeyword("getfield").writeSpace();
        inst.getResult().accept(this, arg);
        writer.writeSpace();
        inst.getObject().accept(this, arg);
        writer.writeSpace().write(inst.getFieldName());
        return null;
    }

    public Void visit(SetFieldInst inst, Void arg) {
        writer.writeKeyword("setfield").writeSpace();
        inst.getObject().accept(this, arg);
        writer.writeSpace().write(inst.getFieldName()).writeSpace();
        inst.getValue().accept(this, arg);
        return null;
    }

    public Void visit(GotoInst inst, Void arg) {
        writer.writeKeyword("goto").writeSpace().writeLabel(inst.getLabel());
        return null;
    }

    public Void visit(JumpIfInst inst, Void arg) {
        writer.writeKeyword("jumpif").writeSpace();
        inst.getCond().accept(this, arg);
        writer.writeSpace().writeLabel(inst.getLabel());
        return null;
    }

    public Void visit(LabelInst inst, Void arg) {
        writer.writeLabel(inst.getLabel());
        return null;
    }

    public Void visit(InstanceOfInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        inst.getVar().accept(this, arg);
        writer.writeSpace().writeKeyword("instanceof").writeSpace()
                .write(inst.getClassName());
        return null;
    }

    public Void visit(MonitorEnterInst inst, Void arg) {
        writer.writeKeyword("monitorenter").writeSpace();
        inst.getVar().accept(this, arg);
        return null;
    }

    public Void visit(MonitorExitInst inst, Void arg) {
        writer.writeKeyword("monitorexit").writeSpace();
        inst.getVar().accept(this, arg);
        return null;
    }

    public Void visit(NewArrayInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace().writeKeyword("new")
                .writeSpace().write(inst.getType());
        for (Variable dimVar : inst.getDimensions()) {
            writer.write("[");
            dimVar.accept(this, arg);
            writer.write("]");
        }
        return null;
    }

    public Void visit(NewObjectInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace().writeKeyword("new")
                .writeSpace().write(inst.getClassName()).write("(");
        for (Iterator<Variable> it = inst.getArgs().iterator(); it.hasNext();) {
            Variable argVar = it.next();
            argVar.accept(this, arg);
            if (it.hasNext()) {
                writer.write(",").writeSpace();
            }
        }
        writer.write(")");
        return null;
    }

    public Void visit(ReturnInst inst, Void arg) {
        writer.writeKeyword("return");
        if (inst.getVar() != null) {
            writer.writeSpace();
            inst.getVar().accept(this, arg);
        }
        return null;
    }

    public Void visit(SwitchInst inst, Void arg) {
        writer.writeKeyword("switch").writeSpace();
        inst.getCond().accept(this, arg);
        writer.writeSpace();
        for (Iterator<Label> it = inst.getLabels().iterator(); it.hasNext();) {
            writer.writeLabel(it.next());
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(ThrowInst inst, Void arg) {
        writer.writeKeyword("throw").writeSpace();
        inst.getVar().accept(this, arg);
        return null;
    }

    public Void visit(UnaryInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        switch (inst.getOperator()) {
            case MINUS:
                writer.write("-");
                break;

            default:
                throw new AssertionError();
        }
        writer.writeSpace();
        inst.getVar().accept(this, arg);
        return null;
    }

    public Void visit(ChoiceInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        writer.writeKeyword("choice").writeSpace();
        for (Iterator<TemporaryVariable> it = inst.getVariables().iterator(); it.hasNext();) {
            TemporaryVariable var = it.next();
            var.accept(this, arg);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }
}