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

package fr.jamgotchian.abcd.core.code;

import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class CodeWriter {

    protected static final Logger LOGGER = Logger.getLogger(CodeWriter.class.getName());

    private static final int INDEX_PADDING = 4;

    private static final int DEFAULT_IDENT = 4;

    protected final Writer writer;

    private int indentSpaces;

    private int indentLevel = 0;

    private boolean indentNeeded = false;

    protected boolean enabled = true;

    public CodeWriter(Writer writer) {
        this(writer, DEFAULT_IDENT);
    }

    public CodeWriter(Writer writer, int indentSpace) {
        this.writer = writer;
        this.indentSpaces = indentSpace;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public void decrIndent() {
        if(indentLevel > 0) {
            indentLevel--;
        }
    }

    public void incrIndent() {
        indentLevel++;
    }

    public abstract void before(List<ColoredString> infos);

    public abstract void after(List<ColoredString> infos);

    protected abstract void writeEol() throws IOException;

    public abstract CodeWriter writeSpace();

    private void indent() {
        for (int i = 0; i < indentSpaces * indentLevel; i++) {
            writeSpace();
        }
    }

    public CodeWriter newLine() {
        try {
            writeEol();
        } catch(IOException exc) {
            LOGGER.log(Level.SEVERE, exc.toString(), exc);
        }
        indentNeeded = true;
        return this;
    }

    public CodeWriter writeIndex(int index) {
        StringBuilder builder = new StringBuilder(INDEX_PADDING);
        String indexStr = "";
        if (index != -1) {
            indexStr = String.format("%d:", index);
        }
        builder.append(indexStr);
        for(int i = indexStr.length(); i < INDEX_PADDING; i++) {
            builder.append(" ");
        }
        write(builder.toString());
        return this;
    }

    public CodeWriter writeLabel(Label label) {
        return write(label);
    }

    public CodeWriter writeKeyword(String keyword) {
        return write(keyword);
    }

    public abstract CodeWriter writeLt();

    public abstract CodeWriter writeGt();

    public abstract CodeWriter writeAmpersand();

    public CodeWriter writeQuotedString(String str) {
        write("\"");
        write(str);
        write("\"");
        return this;
    }

    public CodeWriter write(Object obj) {
        if (obj == null) {
            return write("null");
        } else {
            return write(obj.toString());
        }
    }

    public CodeWriter write(String str) {
        if (indentNeeded) {
            indentNeeded = false;
            indent();
        }

        try {
            writer.write(str);
        } catch(IOException exc) {
            LOGGER.log(Level.SEVERE, exc.toString(), exc);
        }
        return this;
    }

    public abstract String removeSpecialCharacters(String str);
}
