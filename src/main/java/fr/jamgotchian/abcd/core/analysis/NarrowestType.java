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
package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NarrowestType {

    private static final Map<JavaType, List<JavaType>> WIDENING_PRIMITIVE_CONVERSION;

    static {
        Map<JavaType, List<JavaType>> conversion = new HashMap<JavaType, List<JavaType>>();
        conversion.put(JavaType.BYTE,
                Collections.unmodifiableList(Arrays.asList(JavaType.CHAR, JavaType.BYTE, JavaType.SHORT,
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
        WIDENING_PRIMITIVE_CONVERSION = Collections.unmodifiableMap(conversion);
    }

    private JavaType type;

    public NarrowestType() {
    }

    public boolean narrow(JavaType otherType, ClassNameFactory factory) {
        if (otherType == null) {
            return false;
        }
        if (type == null) {
            type = otherType;
            return true;
        }
        if (type.isPrimitive()) {
            if (otherType.isReference()) {
                throw new ABCDException("Cannot narrow primitive to reference type");
            }
            if (otherType.equals(type)) {
                return false;
            } else {
                // find compatible type
                List<JavaType> possibleTypes
                        = new ArrayList<JavaType>(WIDENING_PRIMITIVE_CONVERSION.get(type));
                possibleTypes.retainAll(WIDENING_PRIMITIVE_CONVERSION.get(otherType));
                if (possibleTypes.isEmpty()) {
                    throw new ABCDException("Cannot narrow " + type + " with "
                            + otherType);
                }
                JavaType newType = possibleTypes.get(0);
                if (type.equals(newType)) {
                    return false;
                } else {
                    type = newType;
                    return true;
                }
            }
        }
        if (type.isReference()) {
            if (otherType.isPrimitive()) {
                throw new ABCDException("Cannot narrow reference to primitive type");
            }
            if (otherType.equals(type)) {
              return false;
            } else {
                // find first common ancestor
                try {
                    Class<?> clazz = Class.forName(type.getClassName().getQualifiedName());
                    Collection<Class<?>> ancestors = getAncestors(clazz);

                    Class<?> otherClazz = Class.forName(otherType.getClassName().getQualifiedName());
                    Collection<Class<?>> otherAncestors = getAncestors(otherClazz);

                    ancestors.retainAll(otherAncestors);
                    // should remain at least java.lang.Object
                    Class<?> firstCommonAncestor = ancestors.iterator().next();

                    JavaType newType = JavaType.newRefType(factory.newClassName(firstCommonAncestor.getName()));
                    if (type.equals(newType)) {
                        return false;
                    } else {
                        type = newType;
                        return true;
                    }
                } catch (ClassNotFoundException e) {
                    throw new ABCDException(e);
                }
            }
        }
        throw new InternalError();
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


    public JavaType get() {
        return type;
    }
}
