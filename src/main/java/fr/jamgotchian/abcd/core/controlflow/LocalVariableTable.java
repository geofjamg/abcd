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

import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LocalVariableTable {

    public static class Entry {

        private final int index;

        private final int start;

        private final int end;

        private final String name;

        private final String descriptor;

        public Entry(int index, int start, int end, String name, String descriptor) {
            this.index = index;
            this.start = start;
            this.end = end;
            this.name = name;
            this.descriptor = descriptor;
        }

        public int getIndex() {
            return index;
        }

        public int getStart() {
            return start;
        }

        public int getEnd() {
            return end;
        }

        public String getName() {
            return name;
        }

        public String getDescriptor() {
            return descriptor;
        }
    }

    private final List<Entry> entries = new ArrayList<Entry>();

    public LocalVariableTable() {
    }

    public void addEntry(int index, int start, int end, String name, String descriptor) {
        entries.add(new Entry(index, start, end, name, descriptor));
    }

    public Collection<Entry> getEntries() {
        return Collections.unmodifiableList(entries);
    }

    public String getName(int index, int position) {
        for (Entry entry : entries) {
            if (index == entry.getIndex()
                    && position >= entry.getStart() - 1
                    && position < entry.getEnd()) {
                return entry.getName();
            }
        }
        return null;
    }

    public void print(StringBuilder builder) {
        int rowCount = entries.size()+1;
        List<String> indexColumn = new ArrayList<String>(rowCount);
        List<String> startColumn = new ArrayList<String>(rowCount);
        List<String> endColumn = new ArrayList<String>(rowCount);
        List<String> nameColumn = new ArrayList<String>(rowCount);
        List<String> typeColumn = new ArrayList<String>(rowCount);
        indexColumn.add("index");
        startColumn.add("start");
        endColumn.add("end");
        nameColumn.add("name");
        typeColumn.add("type");
        for (int i = 0; i < entries.size(); i++) {
            Entry entry = entries.get(i);
            indexColumn.add(Integer.toString(entry.getIndex()));
            startColumn.add(Integer.toString(entry.getStart()));
            endColumn.add(Integer.toString(entry.getEnd()));
            nameColumn.add(entry.getName());
            typeColumn.add(entry.getDescriptor());
        }
        ConsoleUtil.printTable(builder, indexColumn, startColumn, endColumn,
                               nameColumn, typeColumn);
    }
}
