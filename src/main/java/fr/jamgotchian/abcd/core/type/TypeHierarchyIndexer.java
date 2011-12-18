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

import com.google.common.collect.LinkedHashMultimap;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import fr.jamgotchian.abcd.core.util.TablePrinter;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TypeHierarchyIndexer {

    private static class TypeNode {

        private final List<TypeNode> parents = new ArrayList<TypeNode>();

        private final List<TypeNode> children = new ArrayList<TypeNode>();

        private final PrimitiveType primitiveType;

        private final String className;

        private final int index;

        private TypeNode(String className, int index) {
            this.primitiveType = null;
            this.className = className;
            this.index = index;
        }

        private TypeNode(PrimitiveType primitiveType, int index) {
            this.primitiveType = primitiveType;
            this.className = null;
            this.index = index;
        }

        public PrimitiveType getPrimitiveType() {
            return primitiveType;
        }

        public String getClassName() {
            return className;
        }

        public String getTypeStr() {
            return primitiveType != null ? primitiveType.toString() : className;
        }

        int getIndex() {
            return index;
        }

        void addChild(TypeNode child) {
            children.add(child);
        }

        void addParent(TypeNode parent) {
            parents.add(parent);
            parent.addChild(this);
        }

        List<TypeNode> getParents() {
            return parents;
        }

        public List<TypeNode> getChildren() {
            return children;
        }
    }

    private int nextIndex = 0;

    private final Set<Integer> primitiveTypeIndexes = new TreeSet<Integer>();

    private final Set<Integer> referenceTypeIndexes = new TreeSet<Integer>();

    private final Map<PrimitiveType, TypeNode> primitiveTypeNodes
            = new EnumMap<PrimitiveType, TypeNode>(PrimitiveType.class);

    private final Map<String, TypeNode> referenceTypeNodes = new HashMap<String, TypeNode>();

    private final Map<Integer, TypeNode> index2node = new TreeMap<Integer, TypeNode>();

    public TypeHierarchyIndexer() {
        addIndex(JavaType.BOOLEAN);
        addIndex(JavaType.BYTE);
        addIndex(JavaType.CHAR);
        addIndex(JavaType.DOUBLE);
        addIndex(JavaType.FLOAT);
        addIndex(JavaType.INT);
        addIndex(JavaType.LONG);
        addIndex(JavaType.SHORT);
    }

    public int getFirstIndex() {
        return 0;
    }

    public int getLastIndex() {
        return nextIndex-1;
    }

    public Set<Integer> getPrimitiveTypeIndexes() {
        return primitiveTypeIndexes;
    }

    public Set<Integer> getReferenceTypeIndexes() {
        return referenceTypeIndexes;
    }

    private TypeNode createNode(PrimitiveType primitiveType) {
        TypeNode node = primitiveTypeNodes.get(primitiveType);
        if (node == null) {
            int index = nextIndex++;
            node = new TypeNode(primitiveType, index);
            primitiveTypeNodes.put(primitiveType, node);
            index2node.put(node.getIndex(), node);
            primitiveTypeIndexes.add(index);
        }
        return node;
    }

    private TypeNode createNode(Class<?> clazz) {
        String className = clazz.getName();
        TypeNode node = referenceTypeNodes.get(className);
        if (node == null) {
            int index = nextIndex++;
            node = new TypeNode(className, index);
            referenceTypeNodes.put(className, node);
            index2node.put(node.getIndex(), node);
            referenceTypeIndexes.add(index);
        }
        return node;
    }

    private TypeNode getNode(JavaType type) {
        TypeNode node = null;
        if (type.getKind() == TypeKind.PRIMITIVE) {
            node = primitiveTypeNodes.get(type.getPrimitiveType());
        } else {
            node = referenceTypeNodes.get(type.getClassName().getQualifiedName());
        }
        if (node == null) {
            throw new ABCDException("Type " + type + " not indexed");
        }
        return node;
    }

    private void getAncestors(Class<?> clazz, Class<?> parent, LinkedHashMultimap<Class<?>, Class<?>> ancestors) {
        if (clazz.getSuperclass() != null) {
            getAncestors(clazz.getSuperclass(), clazz, ancestors);
        } else {
            if (clazz.isInterface()) {
                ancestors.put(Object.class, clazz);
            }
        }
        for (Class<?> _interface : clazz.getInterfaces()) {
            getAncestors(_interface, clazz, ancestors);
        }
        ancestors.put(clazz, parent);
    }

    private void addIndex(String className) {
        try {
            Class<?> clazz = Class.forName(className);
            LinkedHashMultimap<Class<?>, Class<?>> ancestors = LinkedHashMultimap.create();
            getAncestors(clazz, null, ancestors);
            for (Map.Entry<Class<?>, Class<?>> entry : ancestors.entries()) {
                createNode(entry.getKey());
            }
            for (Map.Entry<Class<?>, Class<?>> entry : ancestors.entries()) {
                TypeNode superNode = createNode(entry.getKey());
                if (entry.getValue() != null) {
                    TypeNode node = createNode(entry.getValue());
                    node.addParent(superNode);
                }
            }
        } catch (ClassNotFoundException e) {
            throw new ABCDException(e);
        }
    }

    public void addIndex(JavaType type) {
        if (type.getKind() == TypeKind.PRIMITIVE) {
            createNode(type.getPrimitiveType());
        } else { // reference
            if (type.isArray()) {
                // TODO
                throw new ABCDException("TODO");
            } else {
                addIndex(type.getClassName().getQualifiedName());
            }
        }
    }

    private void getParentIndexes(TypeNode node, Set<Integer> indexes) {
        indexes.add(node.getIndex());
        for (TypeNode parent : node.getParents()) {
            getParentIndexes(parent, indexes);
        }
    }

    private Set<Integer> getParentIndexes(TypeNode node) {
        Set<Integer> indexes = new HashSet<Integer>();
        getParentIndexes(node, indexes);
        return indexes;
    }

    public Set<Integer> getParentIndexes(JavaType type) {
        TypeNode node = getNode(type);
        return getParentIndexes(node);
    }

    private void getChildIndexes(TypeNode node, Set<Integer> indexes) {
        indexes.add(node.getIndex());
        for (TypeNode parent : node.getChildren()) {
            getChildIndexes(parent, indexes);
        }
    }

    private Set<Integer> getChildIndexes(TypeNode node) {
        Set<Integer> indexes = new HashSet<Integer>();
        getChildIndexes(node, indexes);
        return indexes;
    }

    public Set<Integer> getChildIndexes(JavaType type) {
        TypeNode node = getNode(type);
        return getChildIndexes(node);
    }

    public JavaType resolveType(int[] indexes, ClassNameManager classNameManager) {
        if (indexes.length == 0) {
            throw new IllegalArgumentException("Empty index array");
        }
        Set<Integer> sortedIndexes = new TreeSet<Integer>();
        for (int index: indexes) {
            sortedIndexes.add(index);
        }
        List<JavaType> types = new ArrayList<JavaType>(indexes.length);
        int primitiveTypeCount = 0;
        int referenceTypeCount = 0;
        for (int index : sortedIndexes) {
            TypeNode node = index2node.get(index);
            if (node == null) {
                throw new ABCDException("Index " + index + " not found");
            }
            if (node.getPrimitiveType() != null) {
                types.add(JavaType.newPrimitiveType(node.getPrimitiveType()));
                primitiveTypeCount++;
            } else {
                types.add(JavaType.newRefType(classNameManager.newClassName(node.getClassName())));
                referenceTypeCount++;
            }
        }
        assert !(primitiveTypeCount > 0 && referenceTypeCount > 0);
        if (primitiveTypeCount > 0) {
            return types.get(0);
        } else {
            return types.get(types.size()-1);
        }
    }

    public String indexesToString() {
        TablePrinter printer = ConsoleUtil.newTablePrinter("Index", "Type", "Ancestors");
        for (TypeNode node : index2node.values()) {
            printer.addRow(node.getIndex(), node.getTypeStr(), getParentIndexes(node).toString());
        }
        return printer.toString();
    }
}
