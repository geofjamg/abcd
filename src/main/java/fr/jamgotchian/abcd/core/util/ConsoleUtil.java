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
package fr.jamgotchian.abcd.core.util;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ConsoleUtil {

    private static final int SEPARATOR_WIDTH = 80;

    private ConsoleUtil() {
    }

    public static String printTitledSeparator(String title, char separator) {
        StringBuilder builder = new StringBuilder(SEPARATOR_WIDTH);
        int remaining = SEPARATOR_WIDTH - title.length() - 2;
        remaining = Math.max(remaining, 4);
        int after = remaining / 2;
        int before = after + remaining % 2;
        for (int i = 0; i < before; i++) {
            builder.append(separator);
        }
        builder.append(' ').append(title).append(' ');
        for (int i = 0; i < after; i++) {
            builder.append(separator);
        }
        return builder.toString();
    }

    public static String printTable(List<String>... columns) {
        StringBuilder builder = new StringBuilder();
        printTable(builder, columns);
        return builder.toString();
    }

    public static void printTable(StringBuilder out, List<String>... columns) {
        List<Integer> columnWidths = new ArrayList<Integer>(columns.length);
        for (List<String> column : columns) {
            columnWidths.add(getColumnWidth(column));
        }

        printSeparator(out, columnWidths);
        out.append("\n");
        int rowCount = columns[0].size();
        for (int r = 0; r < rowCount; r++) {
            out.append("|");
            for (int c = 0; c < columns.length; c++) {
                String format = " %1$-" + columnWidths.get(c) + "s |";
                out.append(String.format(format, columns[c].get(r)));
            }
            out.append("\n");
            if (r == 0) {
                printSeparator(out, columnWidths);
                out.append("\n");
            }
        }
        printSeparator(out, columnWidths);
    }

    private static int getColumnWidth(List<String> column) {
        int max = Integer.MIN_VALUE;
        for (String s : column) {
            int length = (s != null ? s.length() : 0);
            if (length > max) {
                max = s.length();
            }
        }
        return max;
    }

    private static void printSeparator(StringBuilder builder, List<Integer> columnWidths) {
        builder.append("+");
        for (int columnWidth : columnWidths) {
            for (int i = 0; i < columnWidth+1; i++) {
                builder.append("-");
            }
            builder.append("-+");
        }
    }
}
