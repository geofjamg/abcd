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
import org.objectweb.asm.Type;

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

    public static final JavaType VOID = new JavaType(PrimitiveType.VOID, null, null, 0);
    public static final JavaType INT = new JavaType(PrimitiveType.INTEGER, null, null, 0);
    public static final JavaType LONG = new JavaType(PrimitiveType.LONG, null, null, 0);
    public static final JavaType CHAR = new JavaType(PrimitiveType.CHAR, null, null, 0);
    public static final JavaType BYTE = new JavaType(PrimitiveType.BYTE, null, null, 0);
    public static final JavaType SHORT = new JavaType(PrimitiveType.SHORT, null, null, 0);
    public static final JavaType BOOLEAN = new JavaType(PrimitiveType.BOOLEAN, null, null, 0);
    public static final JavaType FLOAT = new JavaType(PrimitiveType.FLOAT, null, null, 0);
    public static final JavaType DOUBLE = new JavaType(PrimitiveType.DOUBLE, null, null, 0);

    public static JavaType newRefType(ClassName className) {
        return new JavaType(null, className, null, 0);
    }

    public static JavaType newArrayType(JavaType arrayElementType, int arrayDimension) {
        return new JavaType(null, null, arrayElementType, arrayDimension);
    }

    /**
     * Convert from ASM type to ABCD type
     * @param type ASM type
     * @param factory <code>ClassName</code> factory
     * @return ABCD type
     */
    public static JavaType newType(Type type, ClassNameFactory factory) {
        switch (type.getSort()) {
            case Type.VOID:
                return VOID;
            case Type.BOOLEAN:
                return BOOLEAN;
            case Type.CHAR:
                return CHAR;
            case Type.BYTE:
                return BYTE;
            case Type.SHORT:
                return SHORT;
            case Type.INT:
                return INT;
            case Type.FLOAT:
                return FLOAT;
            case Type.LONG:
                return LONG;
            case Type.DOUBLE:
                return DOUBLE;
            case Type.ARRAY:
                return newArrayType(newType(type.getElementType(), factory), type.getDimensions());
            case Type.OBJECT: {
                ClassName argClassName = factory.newClassName(type.getClassName());
                return newRefType(argClassName);
            }
            default:
                throw new InternalError();
        }
    }

    private final PrimitiveType primitiveType;

    private final ClassName className;

    private final JavaType arrayElementType;

    private final int arrayDimension;

    private JavaType(PrimitiveType primitiveType, ClassName className,
                     JavaType arrayElementType, int arrayDimension) {
        this.primitiveType = primitiveType;
        this.className = className;
        this.arrayElementType = arrayElementType;
        this.arrayDimension = arrayDimension;
    }

    public boolean isPrimitive() {
        return primitiveType != null;
    }

    public boolean isReference() {
        return className != null || arrayElementType != null;
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
            if (isPrimitive()) {
                return primitiveType.toString();
            } else { // reference
                return className.getQualifiedName();
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
