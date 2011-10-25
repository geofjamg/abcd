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

package fr.jamgotchian.abcd.core.controlflow;

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.common.LabelManager;
import fr.jamgotchian.abcd.core.output.DOTHTMLLikeInstnWriter;
import fr.jamgotchian.abcd.core.output.HTMLInstnWriter;
import fr.jamgotchian.abcd.core.output.InstnWriter;
import fr.jamgotchian.abcd.core.output.TextInstnWriter;
import java.io.IOException;
import java.io.StringWriter;
import org.objectweb.asm.tree.InsnList;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BytecodeUtil {

    private BytecodeUtil() {
    }

    private static class InstnPrintVisitor extends BytecodeWriter {

        private InstnPrintVisitor(InstnWriter writer) {
            super(writer);
        }

        @Override
        public void before(BasicBlock cfg) {
            try {
                writer.begin();
            } catch(IOException exc) {
                throw new ABCDException(exc);
            }
        }
    }

    public static String toText(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new BytecodeWriter(new TextInstnWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toText(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph cfg = new ControlFlowGraphImpl("tmp", instructions.size()-1);
        BasicBlock bb = cfg.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        new InstnPrintVisitor(new TextInstnWriter(writer)).visit(instructions, bb, new LabelManager());
        return writer.toString();
    }

    public static String toHTML(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new BytecodeWriter(new HTMLInstnWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toDOTHTMLLike(InsnList instructions, BasicBlock bb, LabelManager labelManager) {
        StringWriter writer = new StringWriter();
        new BytecodeWriter(new DOTHTMLLikeInstnWriter(writer)).visit(instructions, bb, labelManager);
        return writer.toString();
    }

    public static String toHTML(InsnList instructions) {
        StringWriter writer = new StringWriter();
        ControlFlowGraph cfg = new ControlFlowGraphImpl("tmp", instructions.size()-1);
        BasicBlock bb = cfg.getBasicBlocksWithinRange(0, instructions.size()-1).iterator().next();
        new InstnPrintVisitor(new HTMLInstnWriter(writer)).visit(instructions, bb, new LabelManager());
        return writer.toString();
    }
}
