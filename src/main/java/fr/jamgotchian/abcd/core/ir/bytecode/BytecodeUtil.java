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
import fr.jamgotchian.abcd.core.output.DOTHTMLLikeCodeWriter;
import fr.jamgotchian.abcd.core.output.HTMLCodeWriter;
import fr.jamgotchian.abcd.core.output.TextCodeWriter;
import java.io.StringWriter;
import org.objectweb.asm.tree.InsnList;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BytecodeUtil {

    private BytecodeUtil() {
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
