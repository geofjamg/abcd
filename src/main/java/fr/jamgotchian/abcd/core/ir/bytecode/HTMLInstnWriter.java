/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ir.bytecode;

import fr.jamgotchian.abcd.core.util.Colors;
import java.awt.Color;
import java.io.IOException;
import java.io.Writer;
import static org.objectweb.asm.util.AbstractVisitor.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class HTMLInstnWriter extends AbstractInstnWriter {

    private static final int INDEX_PADDING = 4;

    public HTMLInstnWriter(Writer writer) {
        super(writer);
    }

    @Override
    void writeSpace() throws IOException {
        writer.write("&nbsp ");
    }

    void writeIndex(int index) throws IOException {
        String indexStr = "";
        if (index != -1) {
            indexStr = String.format("%d:", index);
        }
        writer.write("<font color=\"gray\">");
        writer.write(indexStr);
        for(int i = indexStr.length(); i < INDEX_PADDING; i++) {
            writeSpace();
        }
        writer.write("</font>");
    }

    void writeIndent(int count) throws IOException {
        for (int i = 0; i < count; i++) {
            writeSpace();
            writeSpace();
        }
    }

    void writeOpcode(int opcode) throws IOException {
        writer.write("<b>");
        writer.write(OPCODES[opcode].toLowerCase());
        writer.write("</b>");
    }

    void writeLabel(int label) throws IOException {
        writer.write("<font color=\"green\">");
        writer.write("L");
        writer.write(Integer.toString(label));
        writer.write("</font>");
    }

    public void writeEol() throws IOException {
        writer.append("<br/>");
    }

    void writeLt() throws IOException {
        writer.append("&lt;");
    }

    void writeGt() throws IOException {
        writer.append("&gt;");
    }

    void writeLineOpcode() throws IOException {
        writer.write("<b>");
        writer.append("line");
        writer.write("</b>");
    }

    public void write(String name, Color color) throws IOException {
        writer.write("<font color=\"");
        writer.write(Colors.toString(color));
        writer.write("\">");
        writer.write(name);
        writer.write("</font>");
    }

    public void begin() throws IOException {
        writer.write("<html><body>");
    }

    public void end() throws IOException {
        writer.write("</body></html>");
    }

    @Override
    public void writeFieldOrMethodInstn(int index, int opcode, String scope,
                                        String fieldOrMethodName) throws IOException {
        if ("<init>".equals(fieldOrMethodName)) {
            fieldOrMethodName = "&lt;init&gt;";
        }
        super.writeFieldOrMethodInstn(index, opcode, scope, fieldOrMethodName);
    }
}
