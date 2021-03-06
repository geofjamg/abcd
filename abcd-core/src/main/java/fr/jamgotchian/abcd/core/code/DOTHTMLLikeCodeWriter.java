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
package fr.jamgotchian.abcd.core.code;

import fr.jamgotchian.abcd.core.util.Colors;
import java.awt.Color;
import java.io.IOException;
import java.io.Writer;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DOTHTMLLikeCodeWriter extends HTMLCodeWriter {

    public DOTHTMLLikeCodeWriter(Writer writer) {
        super(writer);
    }

    @Override
    public void before(List<ColoredString> infos) {
        try {
            writer.write("<table border=\"0\">");
            if (infos != null) {
                for (ColoredString info : infos) {
                    writer.write("<tr><td align=\"left\" balign=\"left\" bgcolor=\"");
                    writer.write(Colors.toString(info.getColor()));
                    writer.write("\">");
                    writer.write(info.getValue());
                    writer.write("</td></tr>");
                }
            }
            writer.write("<tr><td align=\"left\" balign=\"left\">");
        } catch(IOException exc) {
            LOGGER.error(exc.toString(), exc);
        }
    }

    @Override
    public void after(List<ColoredString> infos) {
        try {
            writer.write("</td></tr>");
            if (infos != null) {
                for (ColoredString info : infos) {
                    writer.write("<tr><td align=\"left\" balign=\"left\" bgcolor=\"");
                    writer.write(Colors.toString(info.getColor()));
                    writer.write("\">");
                    writer.write(info.getValue());
                    writer.write("</td></tr>");
                }
            }
            writer.write("</table>");
        } catch(IOException exc) {
            LOGGER.error(exc.toString(), exc);
        }
    }

    @Override
    protected void writeEol() throws IOException {
        writer.write("<br/>");
    }

    @Override
    public CodeWriter writeSpace() {
        try {
            writer.write(" ");
        } catch(IOException exc) {
            LOGGER.error(exc.toString(), exc);
        }
        return this;
    }

    @Override
    public CodeWriter writeKeyword(String keyword) {
        try {
            writer.write("<font color=\"");
            writer.write(getColorStr(Color.BLUE));
            writer.write("\">");
        } catch(IOException exc) {
            LOGGER.error(exc.toString(), exc);
        }
        write(keyword);
        try {
            writer.write("</font>");
        } catch(IOException exc) {
            LOGGER.error(exc.toString(), exc);
        }
        return this;
    }

}
