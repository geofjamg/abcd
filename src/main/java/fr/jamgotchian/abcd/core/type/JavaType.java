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
import fr.jamgotchian.abcd.core.common.ABCDException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaType {

    public enum PrimitiveType {
        VOID("void"),
        INTEGER("int"),
        LONG("long"),
        CHAR("char"),
        BYTE("byte"),
        SHORT("short"),
        BOOLEAN("boolean"),
        FLOAT("float"),
        DOUBLE("double");

        private String str;

        private PrimitiveType(String str) {
            this.str = str;
        }

        @Override
        public String toString() {
            return str;
        }
    }

    public static final String UNDEFINED_TYPE = "???";

    public static final JavaType VOID = new JavaType(PrimitiveType.VOID, null, null, 0);
    public static final JavaType INT = new JavaType(PrimitiveType.INTEGER, null, null, 0);
    public static final JavaType LONG = new JavaType(PrimitiveType.LONG, null, null, 0);
    public static final JavaType CHAR = new JavaType(PrimitiveType.CHAR, null, null, 0);
    public static final JavaType BYTE = new JavaType(PrimitiveType.BYTE, null, null, 0);
    public static final JavaType SHORT = new JavaType(PrimitiveType.SHORT, null, null, 0);
    public static final JavaType BOOLEAN = new JavaType(PrimitiveType.BOOLEAN, null, null, 0);
    public static final JavaType FLOAT = new JavaType(PrimitiveType.FLOAT, null, null, 0);
    public static final JavaType DOUBLE = new JavaType(PrimitiveType.DOUBLE, null, null, 0);

    public static final Set<PrimitiveType> ARITHMETIC_TYPES
            = EnumSet.of(PrimitiveType.INTEGER, PrimitiveType.LONG, PrimitiveType.BYTE,
                         PrimitiveType.SHORT, PrimitiveType.FLOAT, PrimitiveType.DOUBLE);

    private static final Map<JavaType, List<JavaType>> WIDENING_PRIMITIVE_CONVERSION;

    static {
        Map<JavaType, List<JavaType>> conversion = new HashMap<JavaType, List<JavaType>>();
        conversion.put(JavaType.BYTE,
                Collections.unmodifiableList(Arrays.asList(JavaType.BYTE, JavaType.SHORT,
                                                           JavaType.INT, JavaType.LONG,
                                                           JavaType.FLOAT, JavaType.DOUBLE)));
        conversion.put(JavaType.SHORT,
                Collections.unmodifiableList(Arrays.asList(JavaType.SHORT, JavaType.INT,
                                                           JavaType.LONG, JavaType.FLOAT,
                                                           JavaType.DOUBLE)));
        conversion.put(JavaType.CHAR,
                Collections.unmodifiableList(Arrays.asList(JavaType.CHAR, JavaType.INT,
                                                           JavaType.LONG, JavaType.FLOAT,
                                                           JavaType.DOUBLE)));
        conversion.put(JavaType.INT,
                Collections.unmodifiableList(Arrays.asList(JavaType.INT, JavaType.LONG,
                                                           JavaType.FLOAT, JavaType.DOUBLE)));
        conversion.put(JavaType.LONG,
                Collections.unmodifiableList(Arrays.asList(JavaType.LONG, JavaType.FLOAT,
                                                           JavaType.DOUBLE)));
        conversion.put(JavaType.FLOAT,
                Collections.unmodifiableList(Arrays.asList(JavaType.FLOAT, JavaType.DOUBLE)));

        conversion.put(JavaType.DOUBLE,
                Collections.unmodifiableList(Arrays.asList(JavaType.DOUBLE)));

        conversion.put(JavaType.BOOLEAN,
                Collections.unmodifiableList(Arrays.asList(JavaType.BOOLEAN)));

        conversion.put(JavaType.VOID,
                Collections.unmodifiableList(Arrays.asList(JavaType.VOID)));

        WIDENING_PRIMITIVE_CONVERSION = Collections.unmodifiableMap(conversion);
    }

    public static JavaType newPrimitiveType(PrimitiveType primitiveType) {
        return new JavaType(primitiveType, null, null, 0);
    }

    public static JavaType newRefType(ClassName className) {
        return new JavaType(null, className, null, 0);
    }

    public static JavaType newArrayType(JavaType arrayElementType, int arrayDimension) {
        return new JavaType(null, null, arrayElementType, arrayDimension);
    }

    private final PrimitiveType primitiveType;

    private final ClassName className;

    private final JavaType arrayElementType;

    private final int arrayDimension;

    private JavaType(PrimitiveType primitiveType, ClassName className,
                     JavaType arrayElementType, int arrayDimension) {
        assert (primitiveType != null && className == null && arrayElementType == null)
                || (primitiveType == null && (className != null || arrayElementType != null));
        this.primitiveType = primitiveType;
        this.className = className;
        this.arrayElementType = arrayElementType;
        this.arrayDimension = arrayDimension;
    }

    public TypeKind getKind() {
        return primitiveType != null ? TypeKind.PRIMITIVE : TypeKind.REFERENCE;
    }

    public boolean isArray() {
        return arrayElementType != null;
    }

    public PrimitiveType getPrimitiveType() {
        return primitiveType;
    }

    public ClassName getClassName() {
        return className;
    }

    public JavaType getArrayElementType() {
        return arrayElementType;
    }

    public int getArrayDimension() {
        return arrayDimension;
    }

    public ComputationalType getComputationalType() {
        if (getKind() == TypeKind.REFERENCE) {
            return ComputationalType.REFERENCE;
        } else {
            switch (primitiveType) {
                case BOOLEAN:
                case BYTE:
                case CHAR:
                case SHORT:
                case INTEGER:
                    return ComputationalType.INT;

                case LONG:
                    return ComputationalType.LONG;

                case FLOAT:
                    return ComputationalType.FLOAT;

                case DOUBLE:
                    return ComputationalType.DOUBLE;

                default:
                    throw new ABCDException("Cannot convert " + this + " to computational type");
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof JavaType)) {
            return false;
        }
        JavaType other = (JavaType) obj;

        return Objects.equal(className, other.className)
                && Objects.equal(primitiveType, other.primitiveType)
                && Objects.equal(arrayElementType, other.arrayElementType)
                && Objects.equal(arrayDimension, other.arrayDimension);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(className, primitiveType, arrayElementType, arrayDimension);
    }

    public String getName(boolean qualifiedName) {
        if (isArray()) {
            StringBuilder builder = new StringBuilder(arrayElementType.getName(qualifiedName));
            for (int i = 0; i < arrayDimension; i++) {
                builder.append("[]");
            }
            return builder.toString();
        } else {
            if (getKind() == TypeKind.PRIMITIVE) {
                return primitiveType.toString();
            } else { // reference
                if (qualifiedName) {
                    return className.getQualifiedName();
                } else {
                    return className.getName();
                }
            }
        }
    }

    public String getQualifiedName() {
        return getName(true);
    }

    public static Set<JavaType> widen(Set<JavaType> types1, Set<JavaType> types2,
                                      ClassNameFactory factory) {
        if (types2.isEmpty()) {
            return types1;
        }
        Set<JavaType> newTypes = new HashSet<JavaType>();
        for (JavaType type1 : types1) {
            newTypes.addAll(type1.widen(types2, factory));
        }
        return newTypes;
    }

    public Set<JavaType> widen(Set<JavaType> otherTypes, ClassNameFactory factory) {
        if (otherTypes.isEmpty()) {
            throw new ABCDException("otherTypes.isEmpty()");
        }
        Set<JavaType> newTypes = new HashSet<JavaType>();
        for (JavaType otherType : otherTypes) {
            JavaType newType = widen(otherType, factory);
            if (newType != null) {
                newTypes.add(newType);
            }
        }
        return newTypes;
    }

    public JavaType widen(JavaType otherType, ClassNameFactory factory) {
        if (otherType == null) {
            throw new ABCDException("otherType == null");
        }
        // types are equals
        if (otherType.equals(this)) {
            return this;
        }
        if (getKind() == TypeKind.PRIMITIVE) {
            // cannot widen primitive to reference type
            if (otherType.getKind() == TypeKind.REFERENCE) {
                return null;
            }
            // try to find a common widening conversion type
            List<JavaType> possibleTypes
                    = new ArrayList<JavaType>(WIDENING_PRIMITIVE_CONVERSION.get(this));
            possibleTypes.retainAll(WIDENING_PRIMITIVE_CONVERSION.get(otherType));
            if (possibleTypes.isEmpty()) {
                return null;
            }
            JavaType newType = possibleTypes.get(0);
            if (newType.equals(this)) {
                return this;
            } else {
                return newType;
            }
        }
        if (getKind() == TypeKind.REFERENCE) {
            if (otherType.getKind() == TypeKind.PRIMITIVE) {
                // cannot widen reference to primitive type
                return null;
            }

            ClassName javaLangObjectClassName = factory.newClassName(Object.class.getName());
            if (getArrayDimension() == 0 && otherType.getArrayDimension() > 0) {
                if (javaLangObjectClassName.equals(getClassName())) {
                    return JavaType.newRefType(javaLangObjectClassName);
                } else {
                    return null;
                }
            } else if (getArrayDimension() > 0 && otherType.getArrayDimension() == 0) {
                if (javaLangObjectClassName.equals(otherType.getClassName())) {
                    return JavaType.newRefType(javaLangObjectClassName);
                } else {
                    return null;
                }
            } else if (getArrayDimension() > 0 && otherType.getArrayDimension() > 0) {
                if (getArrayDimension() != otherType.getArrayDimension()) {
                    return null;
                } else {
                    if (getArrayElementType().getKind() == TypeKind.PRIMITIVE
                            || otherType.getArrayElementType().getKind() == TypeKind.PRIMITIVE) {
                        // equality test have already be done
                        return null;
                    }
                    JavaType commonArrayEltType
                            = getFirstCommonAncestor(getArrayElementType(),
                                                     otherType.getArrayElementType(),
                                                     factory);
                    return JavaType.newArrayType(commonArrayEltType, getArrayDimension());
                }
            } else {
                // find first common ancestor
                return getFirstCommonAncestor(this, otherType, factory);
            }
        }
        throw new InternalError();
    }

    private static JavaType getFirstCommonAncestor(JavaType type1, JavaType type2, ClassNameFactory factory) {
        try {
            Class<?> clazz1 = Class.forName(type1.getClassName().getQualifiedName());
            Collection<Class<?>> ancestors1 = getAncestors(clazz1);

            Class<?> clazz2 = Class.forName(type2.getClassName().getQualifiedName());
            Collection<Class<?>> ancestors2 = getAncestors(clazz2);

            ancestors1.retainAll(ancestors2);
            // should remain at least java.lang.Object
            Class<?> firstCommonAncestor = ancestors1.iterator().next();

            return JavaType.newRefType(factory.newClassName(firstCommonAncestor.getName()));
        } catch (ClassNotFoundException e) {
            throw new ABCDException(e);
        }
    }

    private static void addAncestors(Set<Class<?>> ancestors, Class<?> clazz) {
        ancestors.add(clazz);
        for (Class<?> i : clazz.getInterfaces()) {
            addAncestors(ancestors, i);
        }
        Class<?> sc = clazz.getSuperclass();
        if (sc != null) {
            addAncestors(ancestors, sc);
        }
    }

    private static Collection<Class<?>> getAncestors(Class<?> clazz) {
        Set<Class<?>> ancestors = new LinkedHashSet<Class<?>>();
        addAncestors(ancestors, clazz);
        return ancestors;
    }

    @Override
    public String toString() {
        return getName(false);
    }
}
