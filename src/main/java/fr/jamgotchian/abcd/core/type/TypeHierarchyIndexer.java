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
package fr.jamgotchian.abcd.core.type;

import fr.jamgotchian.abcd.core.common.ABCDException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TypeHierarchyIndexer {

    private final Map<JavaType, Integer> type2index = new HashMap<JavaType, Integer>();

    private final Map<Integer, JavaType> index2type = new HashMap<Integer, JavaType>();

    public TypeHierarchyIndexer() {
        addType(JavaType.BOOLEAN, 0);
        addType(JavaType.BYTE, 1);
        addType(JavaType.CHAR, 2);
        addType(JavaType.DOUBLE, 3);
        addType(JavaType.FLOAT, 4);
        addType(JavaType.INT, 5);
        addType(JavaType.LONG, 6);
        addType(JavaType.SHORT, 7);
    }

    public int getFirstIndex() {
        return 0;
    }

    public int getLastIndex() {
        return index2type.size()-1;
    }

    public int getFirstPrimitiveTypeIndex() {
        return 0;
    }

    public int getLastPrimitiveTypeIndex() {
        return 7;
    }

    private void addType(JavaType type, int index) {
        type2index.put(type, index);
        index2type.put(index, type);
    }

    public int getIndex(JavaType type) {
        Integer index = type2index.get(type);
        if (index == null) {
            throw new ABCDException("Type " + type + " not indexed");
        }
        return index;
    }

    public JavaType getType(int index) {
        JavaType type = index2type.get(index);
        if (type == null) {
            throw new ABCDException("Index error : " + index);
        }
        return type;
    }

    public Collection<JavaType> getTypes(int[] indexes) {
        List<JavaType> types = new ArrayList<JavaType>(indexes.length);
        for (int index : indexes) {
            types.add(getType(index));
        }
        return types;
    }
}
