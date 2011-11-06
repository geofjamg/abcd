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
package fr.jamgotchian.abcd.core.bytecode.dalvik;

import fr.jamgotchian.abcd.core.bytecode.java.JavaBytecodeUtil;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.EnumSet;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.jf.dexlib.TypeIdItem;
import org.jf.dexlib.Util.AccessFlags;
import org.objectweb.asm.Type;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DalvikBytecodeUtil {

    private DalvikBytecodeUtil() {
    }

    public static Set<Modifier> getModifiers(AccessFlags[] accessFlags) {
        Set<Modifier> modifiers = EnumSet.noneOf(Modifier.class);
        for (AccessFlags f : accessFlags) {
            switch (f) {
                case ABSTRACT:
                    modifiers.add(Modifier.ABSTRACT);
                    break;
                case ANNOTATION:
                    // TODO
                    break;
                case BRIDGE:
                    // TODO
                    break;
                case CONSTRUCTOR:
                    // TODO
                    break;
                case DECLARED_SYNCHRONIZED:
                    // TODO
                    break;
                case ENUM:
                    // TODO
                    break;
                case FINAL:
                    modifiers.add(Modifier.FINAL);
                    break;
                case INTERFACE:
                    // TODO
                    break;
                case NATIVE:
                    modifiers.add(Modifier.NATIVE);
                    break;
                case PRIVATE:
                    modifiers.add(Modifier.PRIVATE);
                    break;
                case PROTECTED:
                    modifiers.add(Modifier.PROTECTED);
                    break;
                case PUBLIC:
                    modifiers.add(Modifier.PUBLIC);
                    break;
                case STATIC:
                    modifiers.add(Modifier.STATIC);
                    break;
                case STRICTFP:
                    modifiers.add(Modifier.STRICTFP);
                    break;
                case SYNCHRONIZED:
                    modifiers.add(Modifier.SYNCHRONIZED);
                    break;
                case SYNTHETIC:
                    // TODO
                    break;
                case TRANSIENT:
                    // TODO
                    break;
                case VARARGS:
                    // TODO
                    break;
                case VOLATILE:
                    // TODO
                    break;
                default:
                    throw new InternalError();
            }
        }
        return modifiers;
    }

    public static JavaType newType(TypeIdItem type, ClassNameFactory factory) {
        Type asmType = Type.getType(type.getTypeDescriptor());
        return JavaBytecodeUtil.newType(asmType, factory);
    }
}
