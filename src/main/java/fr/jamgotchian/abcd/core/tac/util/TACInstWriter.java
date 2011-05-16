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

import fr.jamgotchian.abcd.core.tac.model.GetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.PhiInst;
import fr.jamgotchian.abcd.core.tac.model.SetStaticFieldInst;
import fr.jamgotchian.abcd.core.tac.model.StringConst;
import fr.jamgotchian.abcd.core.tac.model.ConditionalInst;
import fr.jamgotchian.abcd.core.tac.model.ChoiceInst;
import fr.jamgotchian.abcd.core.tac.model.CastInst;
import fr.jamgotchian.abcd.core.tac.model.UnaryInst;
import fr.jamgotchian.abcd.core.tac.model.AssignVarInst;
import fr.jamgotchian.abcd.core.tac.model.ClassConst;
import fr.jamgotchian.abcd.core.tac.model.ArrayLengthInst;
import fr.jamgotchian.abcd.core.tac.model.ByteConst;
import fr.jamgotchian.abcd.core.tac.model.DoubleConst;
import fr.jamgotchian.abcd.core.tac.model.NullConst;
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
import fr.jamgotchian.abcd.core.tac.model.Variable;
import fr.jamgotchian.abcd.core.tac.model.JumpIfInst;
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
import fr.jamgotchian.abcd.core.output.ColoredString;
import fr.jamgotchian.abcd.core.output.DOTHTMLLikeCodeWriterFactory;
import fr.jamgotchian.abcd.core.output.HTMLCodeWriterFactory;
import fr.jamgotchian.abcd.core.output.TextCodeWriterFactory;
import fr.jamgotchian.abcd.core.tac.model.AssignConstInst;
import fr.jamgotchian.abcd.core.tac.model.DefInst;
import fr.jamgotchian.abcd.core.tac.model.TACInstSeq;
import fr.jamgotchian.abcd.core.util.Range;
import java.awt.Color;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayDeque;
import java.util.ArrayList;
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

    public static String toString(Range range,
                                  TACInstSeq seq,
                                  ArrayDeque<Variable> inputStack,
                                  ArrayDeque<Variable> outputStack,
                                  CodeWriterFactory factory) {
        Writer writer = new StringWriter();
        try {
            CodeWriter codeWriter = factory.create(writer);
            List<ColoredString> infosBefore = new ArrayList<ColoredString>(2);
            infosBefore.add(new ColoredString(range != null ? range.toString() : "", Color.LIGHT_GRAY));
            if (inputStack != null && inputStack.size() > 0) {
                infosBefore.add(new ColoredString("Input stack : " + inputStack,
                                                  Color.ORANGE));
            }
            codeWriter.before(infosBefore);
            seq.accept(new TACInstWriter(codeWriter), null);
            List<ColoredString> infosAfter = new ArrayList<ColoredString>(1);
            if (outputStack != null && outputStack.size() > 0) {
                infosAfter.add(new ColoredString("Output stack : " + outputStack,
                                                 Color.ORANGE));
            }
            codeWriter.after(infosAfter);
        } finally {
            try {
                writer.close();
            } catch (IOException e) {
                logger.log(Level.SEVERE, e.toString(), e);
            }
        }
        return writer.toString();
    }

    public static String toDOTHTMLLike(Range range,
                                       TACInstSeq seq,
                                       ArrayDeque<Variable> inputStack,
                                       ArrayDeque<Variable> outputStack) {
        return toString(range, seq, inputStack, outputStack, new DOTHTMLLikeCodeWriterFactory());
    }

    private final CodeWriter writer;

    private TACInstWriter(CodeWriter writer) {
        this.writer = writer;
    }

    public Void visit(TACInstSeq insts, Void arg) {
        for (Iterator<TACInst> it = insts.getInsts().iterator(); it.hasNext();) {
            TACInst inst = it.next();
            if (inst instanceof DefInst) {
                writer.write("d")
                        .write(Integer.toString(((DefInst) inst).getDefID()))
                        .write(":").writeSpace();
            }
            inst.accept(this, arg);
            if (it.hasNext()) {
                writer.newLine();
            }
        }
        return null;
    }

    public Void visit(Variable inst, Void arg) {
        writer.write(inst.toString());
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
        writer.writeQuotedString(inst.getValue());
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

    public Void visit(AssignVarInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        inst.getValue().accept(this, arg);
        return null;
    }

    public Void visit(AssignConstInst inst, Void arg) {
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
        writer.writeSpace();
        writer.write(inst.getSignature());
        writer.writeSpace();
        for (Iterator<Variable> it = inst.getArguments().iterator(); it.hasNext();) {
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
                .writeSpace().write(inst.getScope()).writeSpace().write(inst.getSignature())
                .writeSpace();
        for (Iterator<Variable> it = inst.getArguments().iterator(); it.hasNext();) {
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

    public Void visit(GetStaticFieldInst inst, Void arg) {
        writer.writeKeyword("getstaticfield").writeSpace();
        inst.getResult().accept(this, arg);
        writer.writeSpace().write(inst.getScope()).write(".").write(inst.getFieldName());
        return null;
    }

    public Void visit(SetStaticFieldInst inst, Void arg) {
        writer.writeKeyword("setstaticfield").writeSpace();
        writer.write(inst.getScope()).write(".").write(inst.getFieldName()).writeSpace();
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
        for (Iterator<Variable> it = inst.getChoices().iterator(); it.hasNext();) {
            Variable var = it.next();
            var.accept(this, arg);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(PhiInst inst, Void arg) {
        inst.getResult().accept(this, arg);
        writer.writeSpace().write("=").writeSpace();
        writer.writeKeyword("phi ").writeSpace();
        for (Iterator<Variable> it = inst.getArgs().iterator(); it.hasNext();) {
            Variable var = it.next();
            var.accept(this, arg);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }
}
