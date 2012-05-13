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

import fr.jamgotchian.abcd.core.code.CodeWriter;
import fr.jamgotchian.abcd.core.code.CodeWriterFactory;
import fr.jamgotchian.abcd.core.code.ColoredString;
import fr.jamgotchian.abcd.core.code.DOTHTMLLikeCodeWriterFactory;
import fr.jamgotchian.abcd.core.code.HTMLCodeWriterFactory;
import fr.jamgotchian.abcd.core.code.TextCodeWriterFactory;
import fr.jamgotchian.abcd.core.util.Range;
import java.awt.Color;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IRInstWriter implements IRInstVisitor<Void, Void> {

    private static final Logger LOGGER = LoggerFactory.getLogger(IRInstWriter.class);

    public static String toString(IRInst inst, CodeWriterFactory factory) {
        Writer writer = new StringWriter();
        try {
            inst.accept(new IRInstWriter(factory.create(writer)), null);
        } finally {
            try {
                writer.close();
            } catch (IOException e) {
                LOGGER.error(e.toString(), e);
            }
        }
        return writer.toString();
    }

    public static String toText(IRInst inst) {
        return toString(inst, new TextCodeWriterFactory());
    }

    public static String toHTML(IRInst inst) {
        return toString(inst, new HTMLCodeWriterFactory());
    }

    public static String toString(BasicBlock bb,
                                  CodeWriterFactory factory) {
        Range range = bb.getRange();
        IRInstSeq seq = bb.getInstructions();
        VariableStack inputStack = bb.getInputStack();
        VariableStack outputStack = bb.getOutputStack();
        Map<BasicBlockPropertyName, Object> attributes = bb.getProperties();
        Writer writer = new StringWriter();
        try {
            CodeWriter codeWriter = factory.create(writer);
            List<ColoredString> infosBefore = new ArrayList<ColoredString>(2);
            infosBefore.add(new ColoredString(range != null ? range.toString() : "", Color.LIGHT_GRAY));
            if (inputStack != null && inputStack.size() > 0) {
                infosBefore.add(new ColoredString("Input stack : " + inputStack,
                                                  Color.ORANGE));
            }
            if (attributes.size() > 0) {
                infosBefore.add(new ColoredString("Properties : " + attributes,
                                                  Color.PINK));
            }
            codeWriter.before(infosBefore);
            seq.accept(new IRInstWriter(codeWriter), null);
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
                LOGGER.error(e.toString(), e);
            }
        }
        return writer.toString();
    }

    public static String toDOTHTMLLike(BasicBlock bb) {
        return toString(bb, new DOTHTMLLikeCodeWriterFactory());
    }

    private final CodeWriter writer;

    private IRInstWriter(CodeWriter writer) {
        this.writer = writer;
    }

    public Void visit(IRInstSeq insts, Void arg) {
        for (Iterator<IRInst> it = insts.iterator(); it.hasNext();) {
            IRInst inst = it.next();
            if (inst.isIgnored()) {
                writer.setEnabled(false);
            }
            inst.accept(this, arg);
            if (inst.isIgnored()) {
                writer.setEnabled(true);
            }
            if (it.hasNext()) {
                writer.newLine();
            }
        }
        return null;
    }

    public Void visit(ArrayLengthInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write("arrayLength").writeSpace().write(inst.getArray());
        return null;
    }

    public Void visit(AssignVarInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write(inst.getValue());
        return null;
    }

    public Void visit(AssignConstInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace();
        if (inst.getConst() instanceof StringConst) {
            writer.writeQuotedString(((StringConst) inst.getConst()).getValue());
        } else {
            writer.write(inst.getConst());
        }
        return null;
    }

    public Void visit(BinaryInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write(inst.getLeft()).writeSpace();
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

            case BITWISE_AND:
                writer.writeAmpersand();
                break;

            case BITWISE_OR:
                writer.write("|");
                break;

            case BITWISE_XOR:
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
        writer.writeSpace()
              .write(inst.getRight());
        return null;
    }

    public Void visit(CallMethodInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .writeKeyword("call").writeSpace()
              .write(inst.getObject()).writeSpace();
        String signature = writer.removeSpecialCharacters(inst.getSignature().toString());
        writer.write(signature).writeSpace();
        for (Iterator<Variable> it = inst.getArguments().iterator(); it.hasNext();) {
            Variable argVar = it.next();
            writer.write(argVar);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(CallStaticMethodInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .writeKeyword("callstatic")
              .writeSpace().write(inst.getScope()).writeSpace();
        String signature = writer.removeSpecialCharacters(inst.getSignature().toString());
        writer.write(signature).writeSpace();
        for (Iterator<Variable> it = inst.getArguments().iterator(); it.hasNext();) {
            Variable argVar = it.next();
            writer.write(argVar);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(CastInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write("(").write(inst.getCastType()).write(")").writeSpace()
              .write(inst.getVar());
        return null;
    }

    public Void visit(ConditionalInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write(inst.getCond())
              .writeSpace().write("?").writeSpace()
              .write(inst.getThen())
              .writeSpace().write(":").writeSpace()
              .write(inst.getElse());
        return null;
    }

    public Void visit(GetArrayInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write(inst.getArray()).write("[").write(inst.getIndex()).write("]");
        return null;
    }

    public Void visit(SetArrayInst inst, Void arg) {
        writer.write(inst.getArray())
              .write("[").write(inst.getIndex()).write("]").writeSpace()
              .write("=").writeSpace().write(inst.getValue());
        return null;
    }

    public Void visit(GetFieldInst inst, Void arg) {
        writer.writeKeyword("getfield").writeSpace()
              .write(inst.getResult()).writeSpace()
              .write(inst.getObject()).writeSpace()
              .write(inst.getFieldName());
        return null;
    }

    public Void visit(SetFieldInst inst, Void arg) {
        writer.writeKeyword("setfield").writeSpace()
              .write(inst.getObject()).writeSpace()
              .write(inst.getFieldName()).writeSpace()
              .write(inst.getValue());
        return null;
    }

    public Void visit(GetStaticFieldInst inst, Void arg) {
        writer.writeKeyword("getstaticfield").writeSpace()
              .write(inst.getResult()).writeSpace()
              .write(inst.getScope()).write(".").write(inst.getFieldName());
        return null;
    }

    public Void visit(SetStaticFieldInst inst, Void arg) {
        writer.writeKeyword("setstaticfield").writeSpace()
              .write(inst.getScope()).write(".").write(inst.getFieldName()).writeSpace()
              .write(inst.getValue());
        return null;
    }

    public Void visit(JumpIfInst inst, Void arg) {
        writer.writeKeyword("jumpif").writeSpace()
              .write(inst.getCond());
        return null;
    }

    public Void visit(InstanceOfInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .write(inst.getVar()).writeSpace()
              .writeKeyword("instanceof").writeSpace()
              .write(inst.getType());
        return null;
    }

    public Void visit(MonitorEnterInst inst, Void arg) {
        writer.writeKeyword("monitorenter").writeSpace()
              .write(inst.getObj());
        return null;
    }

    public Void visit(MonitorExitInst inst, Void arg) {
        writer.writeKeyword("monitorexit").writeSpace()
              .write(inst.getObj());
        return null;
    }

    public Void visit(NewArrayInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .writeKeyword("new").writeSpace().write(inst.getElementType());
        for (Variable dimVar : inst.getDimensions()) {
            writer.write("[").write(dimVar).write("]");
        }
        return null;
    }

    public Void visit(NewObjectInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .writeKeyword("new").writeSpace()
              .write(inst.getType()).write("(");
        for (Iterator<Variable> it = inst.getArguments().iterator(); it.hasNext();) {
            Variable argVar = it.next();
            writer.write(argVar);
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
            writer.writeSpace().write(inst.getVar());
        }
        return null;
    }

    public Void visit(SwitchInst inst, Void arg) {
        writer.writeKeyword("switch").writeSpace().write(inst.getIndex());
        return null;
    }

    public Void visit(ThrowInst inst, Void arg) {
        writer.writeKeyword("throw").writeSpace().write(inst.getVar());
        return null;
    }

    public Void visit(UnaryInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace();
        switch (inst.getOperator()) {
            case MINUS:
                writer.write("-");
                break;

            case NOT:
                writer.write("!");
                break;

            case NONE:
                break;

            default:
                throw new AssertionError();
        }
        writer.writeSpace().write(inst.getVar());
        return null;
    }

    public Void visit(ChoiceInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .writeKeyword("choice").writeSpace();
        for (Iterator<Variable> it = inst.getChoices().iterator(); it.hasNext();) {
            Variable var = it.next();
            writer.write(var);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }

    public Void visit(PhiInst inst, Void arg) {
        writer.write(inst.getResult()).writeSpace().write("=").writeSpace()
              .writeKeyword("phi ").writeSpace();
        for (Iterator<Variable> it = inst.getArgs().iterator(); it.hasNext();) {
            Variable var = it.next();
            writer.write(var);
            if (it.hasNext()) {
                writer.writeSpace();
            }
        }
        return null;
    }
}
