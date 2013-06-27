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

import com.google.common.base.Objects;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.util.console.ConsoleUtil;
import fr.jamgotchian.abcd.core.util.console.TablePrinter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
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

    private static class Type {

        private final PrimitiveType primitiveType;

        private final String className;

        private int arrayDimension;

        private Type(JavaType type) {
            primitiveType = type.getPrimitiveType();
            if (type.getClassName() != null) {
                className = type.getClassName().getQualifiedName();
            } else {
                className = null;
            }
            arrayDimension = type.getArrayDimension();
        }

        private Type(PrimitiveType primitiveType, String className, int arrayDimension) {
            assert primitiveType != null ^ className != null;
            this.primitiveType = primitiveType;
            this.className = className;
            this.arrayDimension = arrayDimension;
        }

        private PrimitiveType getPrimitiveType() {
            return primitiveType;
        }

        private String getClassName() {
            return className;
        }

        private int getArrayDimension() {
            return arrayDimension;
        }

        private JavaType toJavaType(ClassNameManager classNameManager) {
            return new JavaType(primitiveType,
                                className != null ? classNameManager.newClassName(className) : null,
                                arrayDimension);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Type) {
                Type other = (Type) obj;
                return Objects.equal(primitiveType, other.primitiveType)
                        && Objects.equal(className, other.className)
                        && arrayDimension == other.arrayDimension;
            }
            return false;
        }

        @Override
        public int hashCode() {
            return Objects.hashCode(primitiveType, className, arrayDimension);
        }

        @Override
        public String toString() {
            StringBuilder builder = new StringBuilder();
            if (primitiveType != null) {
                builder.append(primitiveType);
            } else {
                builder.append(className);
            }
            for (int i = 0; i < arrayDimension; i++) {
                builder.append("[]");
            }
            return builder.toString();
        }
    }

    private static class TypeNode {

        private final Set<TypeNode> parents = new LinkedHashSet<>();

        private final Set<TypeNode> children = new LinkedHashSet<>();

        private final Type type;

        private final int index;

        private TypeNode(Type type, int index) {
            this.type = type;
            this.index = index;
        }

        public Type getType() {
            return type;
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

        Collection<TypeNode> getParents() {
            return parents;
        }

        public Collection<TypeNode> getChildren() {
            return children;
        }
    }

    private final ClassLoader classLoader;

    private int nextIndex = 0;

    private final Set<Integer> primitiveTypeIndexes = new TreeSet<>();

    private final Set<Integer> referenceTypeIndexes = new TreeSet<>();

    private final Map<Type, TypeNode> type2Node = new HashMap<>();

    private final Map<Integer, TypeNode> index2node = new TreeMap<>();

    public TypeHierarchyIndexer(ClassLoader classLoader) {
        this.classLoader = classLoader;
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

    private TypeNode createNode(Type type) {
        TypeNode node = type2Node.get(type);
        if (node == null) {
            int index = nextIndex++;
            node = new TypeNode(type, index);
            type2Node.put(type, node);
            index2node.put(index, node);
        }
        return node;
    }

    private TypeNode createNode(PrimitiveType primitiveType) {
        TypeNode node = createNode(new Type(primitiveType, null, 0));
        primitiveTypeIndexes.add(node.getIndex());
        return node;
    }

    private TypeNode createNode(PrimitiveType primitiveType, int arrayDimension) {
        TypeNode node = createNode(new Type(primitiveType, null, arrayDimension));
        referenceTypeIndexes.add(node.getIndex());
        return node;
    }

    private TypeNode createNode(String className) {
        TypeNode node = createNode(new Type(null, className, 0));
        referenceTypeIndexes.add(node.getIndex());
        return node;
    }

    private TypeNode createNode(String className, int arrayDimension) {
        TypeNode node = createNode(new Type(null, className, arrayDimension));
        referenceTypeIndexes.add(node.getIndex());
        return node;
    }

    private TypeNode getNode(JavaType type) {
        TypeNode node = type2Node.get(new Type(type));
        if (node == null) {
            throw new ABCDException("Type " + type + " not indexed");
        }
        return node;
    }

    private void getAncestors(Class<?> clazz, Class<?> parent, LinkedHashMultimap<String, String> ancestors) {
        if (clazz.getSuperclass() != null) {
            getAncestors(clazz.getSuperclass(), clazz, ancestors);
        } else {
            if (clazz.isInterface()) {
                ancestors.put(Object.class.getName(), clazz.getName());
            }
        }
        for (Class<?> _interface : clazz.getInterfaces()) {
            getAncestors(_interface, clazz, ancestors);
        }
        ancestors.put(clazz.getName(), parent == null ? null : parent.getName());
    }

    private Multimap<String, String> getAncestors(String className) throws ClassNotFoundException {
        Class<?> clazz = Class.forName(className, true, classLoader);
        LinkedHashMultimap<String, String> ancestors = LinkedHashMultimap.create();
        getAncestors(clazz, null, ancestors);
        return ancestors;
    }

    private void addIndex(String className) {
        try {
            Multimap<String, String> ancestors = getAncestors(className);
            for (Map.Entry<String, String> entry : ancestors.entries()) {
                createNode(entry.getKey());
            }
            for (Map.Entry<String, String> entry : ancestors.entries()) {
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

    private void addIndex(PrimitiveType primitiveType, int arrayDimension) {
        TypeNode node = createNode(primitiveType, arrayDimension);
        // java.lang.Object is a super class of primitive arrays (int[], float[], ...)
        TypeNode javaLangObjectNode = createNode(Object.class.getName());
        node.addParent(javaLangObjectNode);
    }

    private void addIndex(String className, int arrayDimension) {
        try {
            Multimap<String, String> ancestors = getAncestors(className);
            for (Map.Entry<String, String> entry : ancestors.entries()) {
                createNode(entry.getKey(), arrayDimension);
            }
            for (Map.Entry<String, String> entry : ancestors.entries()) {
                TypeNode superNode = createNode(entry.getKey(), arrayDimension);
                if (entry.getValue() != null) {
                    TypeNode node = createNode(entry.getValue(), arrayDimension);
                    node.addParent(superNode);
                }
            }
            // java.lang.Object is a super class of java.lang.Object[]
            TypeNode javaLangObjectArrayNode = createNode(Object.class.getName(), arrayDimension);
            TypeNode javaLangObjectNode = createNode(Object.class.getName());
            javaLangObjectArrayNode.addParent(javaLangObjectNode);
        } catch (ClassNotFoundException e) {
            throw new ABCDException(e);
        }
    }

    public void addIndex(JavaType type) {
        if (type.getKind() == TypeKind.PRIMITIVE) {
            createNode(type.getPrimitiveType());
        } else { // reference
            if (type.isArray()) {
                if (type.getElementTypeKind() == TypeKind.PRIMITIVE) {
                    addIndex(type.getPrimitiveType(), type.getArrayDimension());
                } else {
                    addIndex(type.getClassName().getQualifiedName(), type.getArrayDimension());
                }
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
        Set<Integer> indexes = new HashSet<>();
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
        Set<Integer> indexes = new HashSet<>();
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
        Set<Integer> sortedIndexes = new TreeSet<>();
        for (int index: indexes) {
            sortedIndexes.add(index);
        }
        List<JavaType> types = new ArrayList<>(indexes.length);
        int primitiveTypeCount = 0;
        int referenceTypeCount = 0;
        for (int index : sortedIndexes) {
            TypeNode node = index2node.get(index);
            if (node == null) {
                throw new ABCDException("Index " + index + " not found");
            }
            JavaType type = node.getType().toJavaType(classNameManager);
            if (type.getKind() == TypeKind.PRIMITIVE) {
                primitiveTypeCount++;
            } else {
                referenceTypeCount++;
            }
            types.add(type);
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
            printer.addRow(node.getIndex(), node.getType(), getParentIndexes(node).toString());
        }
        return printer.toString();
    }
}
