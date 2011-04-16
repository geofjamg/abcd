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

import fr.jamgotchian.abcd.core.common.Label;
import fr.jamgotchian.abcd.core.util.Colors;
import java.awt.Color;
import java.io.IOException;
import java.io.Writer;
import java.util.logging.Level;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class HTMLCodeWriter extends CodeWriter {

    public HTMLCodeWriter(Writer writer) {
        super(writer);
    }
    
    public HTMLCodeWriter(Writer writer, int indentSpace) {
        super(writer, indentSpace);
    }

    @Override
    protected void writeEol() throws IOException {
        writer.write("<br/>");
    }

    @Override
    public CodeWriter writeSpace() {
        try {
            writer.write("&nbsp ");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        return this;
    }

    @Override
    public CodeWriter writeLabel(Label label) {
        try {
            writer.write("<font color=\"green\">");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        super.writeLabel(label);
        try {
            writer.write("</font>");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        return this;
    }

    @Override
    public CodeWriter writeKeyword(String keyword) {
        try {
            writer.write("<b>");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        super.writeKeyword(keyword);
        try {
            writer.write("</b>");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        return this;
    }

    @Override
    public CodeWriter writeQuotedString(String str) {
        try {
            writer.write("<font color=\"");
            writer.write(Colors.toString(Color.MAGENTA));
            writer.write("\">");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        super.writeQuotedString(str);
        try {
            writer.write("</font>");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
        return this;
    }
       
    public void writeLt() {
        try {
            writer.append("&lt;");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
    }

    public void writeGt() {
        try {
            writer.append("&gt;");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
    }
    
    public void writeAmpersand() {
        try {
            writer.append("&amp;");
        } catch(IOException exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
    }
}
