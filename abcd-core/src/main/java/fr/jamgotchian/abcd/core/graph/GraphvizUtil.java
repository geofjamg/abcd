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
package fr.jamgotchian.abcd.core.graph;

import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class GraphvizUtil {

    private GraphvizUtil() {
    }

    public static void writeIndent(Appendable out, int indentLevel) throws IOException {
        for (int i = 0 ; i < indentLevel; i++) {
            out.append("    ");
        }
    }

    public static <V, E> void writeAttributes(Writer writer, Map<String, String> attributes) throws IOException {
        writer.append(" [");
        for (Iterator<Map.Entry<String, String>> it = attributes.entrySet().iterator();
             it.hasNext();) {
            Map.Entry<String, String> entry = it.next();
            String propName = entry.getKey();
            String propValue = entry.getValue();
            writer.append(propName).append("=").append(propValue);
            if (it.hasNext()) {
                writer.append(", ");
            }
        }
        writer.append("]");
    }
}
