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
package fr.jamgotchian.abcd.core.util.console;


/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ConsoleUtil {

    private static final int SEPARATOR_WIDTH = 80;

    private ConsoleUtil() {
    }

    public static String indentMsg(String msg, int indent) {
        StringBuilder builder = new StringBuilder(indent + msg.length());
        for (int i = 0; i < indent; i++) {
             builder.append(" ");
        }
        builder.append(msg);
        return builder.toString();
    }

    public static String formatTitledSeparator(String title, char separator) {
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

    public static String formatSeparator(char separator) {
        StringBuilder builder = new StringBuilder(SEPARATOR_WIDTH);
        for (int i = 0; i < SEPARATOR_WIDTH; i++) {
            builder.append(separator);
        }
        return builder.toString();
    }

    public static TablePrinter newTablePrinter(String... columnNames) {
        return new TablePrinter(columnNames);
    }
}
