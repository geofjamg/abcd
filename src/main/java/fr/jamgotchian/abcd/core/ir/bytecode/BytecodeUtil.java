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

package fr.jamgotchian.abcd.core.ir.bytecode;

import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraphImpl;
import fr.jamgotchian.abcd.core.code.DOTHTMLLikeCodeWriter;
import fr.jamgotchian.abcd.core.code.HTMLCodeWriter;
import fr.jamgotchian.abcd.core.code.TextCodeWriter;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.Opcodes;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BytecodeUtil implements Opcodes {

    private BytecodeUtil() {
    }

    public static Set<Modifier> getModifiers(int access) {
        Set<Modifier> modifiers = EnumSet.noneOf(Modifier.class);

        if ((access & ACC_ABSTRACT) != 0) {
            modifiers.add(Modifier.ABSTRACT);
        }
        if ((access & ACC_FINAL) != 0) {
            modifiers.add(Modifier.FINAL);
        }
        if ((access & ACC_NATIVE) != 0) {
            modifiers.add(Modifier.NATIVE);
        }
        if ((access & ACC_PRIVATE) != 0) {
            modifiers.add(Modifier.PRIVATE);
        }
        if ((access & ACC_PROTECTED) != 0) {
            modifiers.add(Modifier.PROTECTED);
        }
        if ((access & ACC_PUBLIC) != 0) {
            modifiers.add(Modifier.PUBLIC);
        }
        if ((access & ACC_STATIC) != 0) {
            modifiers.add(Modifier.STATIC);
        }
        if ((access & ACC_SYNCHRONIZED) != 0) {
            modifiers.add(Modifier.SYNCHRONIZED);
        }
        if ((access & ACC_TRANSIENT) != 0) {
            modifiers.add(Modifier.TRANSIENT);
        }
        if ((access & ACC_VOLATILE) != 0) {
            modifiers.add(Modifier.VOLATILE);
        }
        return modifiers;
    }

    public static void printInnerClasses(Map<String, String> innerClasses, StringBuilder builder) {
        int rowCount = innerClasses.size()+1;
        List<String> innerClassColumn = new ArrayList<String>(rowCount);
        List<String> outerClassColumn = new ArrayList<String>(rowCount);
        innerClassColumn.add("Inner class");
        outerClassColumn.add("Outer class");
        for (Map.Entry<String, String> entry : innerClasses.entrySet()) {
            String innerClass = entry.getKey();
            String outerClass = entry.getValue();
            innerClassColumn.add(innerClass);
            outerClassColumn.add(outerClass);
        }
        ConsoleUtil.printTable(builder, innerClassColumn, outerClassColumn);
    }

    public static String toText(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new BytecodeWriter(new TextCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toText(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph cfg = new ControlFlowGraphImpl("tmp", instructions.size()-1);
        BasicBlock bb = cfg.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        new BytecodeWriter(new TextCodeWriter(writer)).visit(instructions, bb, new LabelManager());
        return writer.toString();
    }

    public static String toHTML(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new BytecodeWriter(new HTMLCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toDOTHTMLLike(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new BytecodeWriter(new DOTHTMLLikeCodeWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toHTML(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph cfg = new ControlFlowGraphImpl("tmp", instructions.size()-1);
        BasicBlock bb = cfg.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        new BytecodeWriter(new HTMLCodeWriter(writer)).visit(instructions, bb, new LabelManager());
        return writer.toString();
    }
}
