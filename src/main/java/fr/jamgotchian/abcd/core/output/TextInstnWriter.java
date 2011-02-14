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

package fr.jamgotchian.abcd.core.output;

import java.awt.Color;
import java.io.IOException;
import java.io.Writer;
import static org.objectweb.asm.util.AbstractVisitor.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TextInstnWriter extends AbstractInstnWriter {

    private static final int INDEX_PADDING = 4;

    public TextInstnWriter(Writer writer) {
        super(writer);
    }

    void writeIndex(int index) throws IOException {
        StringBuilder builder = new StringBuilder(INDEX_PADDING);
        String indexStr = "";
        if (index != -1) {
            indexStr = String.format("%d:", index);
        }
        builder.append(indexStr);
        for(int i = indexStr.length(); i < INDEX_PADDING; i++) {
            builder.append(" ");
        }
        writer.write(builder.toString());
    }

    void writeIndent(int count) throws IOException {
        for (int i = 0; i < count; i++) {
            writer.write("  ");
        }
    }
    
    void writeOpcode(int opcode) throws IOException {
        writer.write(OPCODES[opcode].toLowerCase());
    }

    void writeLabel(int label) throws IOException {
        writer.write("L");
        writer.write(Integer.toString(label));
    }

    void writeEol() throws IOException {
        writer.append("\n");
    }

    void writeLt() throws IOException {
        writer.append("<;");
    }

    void writegt() throws IOException {
        writer.append(">");
    }
    
    void writeLineOpcode() throws IOException {
        writer.append("line");
    }

    public void writeProperty(String name, Color color) throws IOException {
        writer.write(name);
        writeEol();
    }

    public void begin() throws IOException {
    }

    public void end() throws IOException {
    }
}
