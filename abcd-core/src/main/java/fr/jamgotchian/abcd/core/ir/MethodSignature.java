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
package fr.jamgotchian.abcd.core.ir;

import com.google.common.base.Objects;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class MethodSignature {

    private final String methodName;

    private final JavaType returnType;

    private final List<JavaType> argumentTypes;

    public MethodSignature(String methodName, JavaType returnType, List<JavaType> argumentTypes) {
        this.methodName = methodName;
        this.returnType = returnType;
        this.argumentTypes = argumentTypes;
    }

    public String getMethodName() {
        return methodName;
    }

    public JavaType getReturnType() {
        return returnType;
    }

    public List<JavaType> getArgumentTypes() {
        return argumentTypes;
    }

    public boolean isConstructor() {
        return "<init>".equals(methodName) && returnType.equals(JavaType.VOID);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof MethodSignature)) {
            return false;
        }
        MethodSignature other = (MethodSignature) obj;
        if (!returnType.equals(other.returnType)
                || !methodName.equals(other.methodName)
                || argumentTypes.size() != other.argumentTypes.size()) {
            return false;
        }
        for (int i = 0; i < argumentTypes.size(); i++) {
            if (!argumentTypes.get(i).equals(other.argumentTypes.get(i))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(returnType, methodName, argumentTypes);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append(returnType).append("-").append(methodName);
        if (argumentTypes.size() > 0) {
            builder.append("-");
            for (Iterator<JavaType> it = argumentTypes.iterator(); it.hasNext();) {
                builder.append(it.next());
                if (it.hasNext()) {
                    builder.append("-");
                }
            }
        }
        return builder.toString();
    }
}
