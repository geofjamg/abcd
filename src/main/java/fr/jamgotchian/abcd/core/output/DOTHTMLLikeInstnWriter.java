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
package fr.jamgotchian.abcd.core.output;

import java.io.IOException;
import java.io.Writer;
import static org.objectweb.asm.util.AbstractVisitor.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DOTHTMLLikeInstnWriter extends HTMLInstnWriter {
       
    public DOTHTMLLikeInstnWriter(Writer writer) {
        super(writer);
    }

    @Override
    public void begin() throws IOException {
        writer.write("<table border=\"0\">");
        writer.write("<tr><td align=\"left\" balign=\"left\">");
    }

    @Override
    public void end() throws IOException {
        writer.write("</td></tr>");
        writer.write("</table>");
    }

    @Override
    void writeSpace() throws IOException {
        writer.write(" ");
    }

    @Override
    void writeOpcode(int opcode) throws IOException {
        // graphviz bug : <b> doesn't work
        //writer.write("<b>");
        writer.write("<font color=\"blue\">");
        writer.write(OPCODES[opcode].toLowerCase());
        writer.write("</font>");
        //writer.write("</b>");
    }
}
