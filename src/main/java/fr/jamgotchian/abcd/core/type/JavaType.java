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
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaType {

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

    public static final Map<JavaType, List<JavaType>> WIDENING_PRIMITIVE_CONVERSION;

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

    @Override
    public String toString() {
        return getName(false);
    }
}
