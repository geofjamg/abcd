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

import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.Field;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.Package;
import fr.jamgotchian.abcd.core.bytecode.ClassFactory;
import fr.jamgotchian.abcd.core.bytecode.MethodFactory;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.jf.dexlib.ClassDataItem.EncodedField;
import org.jf.dexlib.ClassDataItem.EncodedMethod;
import org.jf.dexlib.ClassDefItem;
import org.jf.dexlib.TypeIdItem;
import org.jf.dexlib.Util.AccessFlags;
import org.objectweb.asm.Type;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DalvikBytecodeClassFactory implements ClassFactory {

    private final ClassDefItem item;

    public DalvikBytecodeClassFactory(ClassDefItem item) {
        this.item = item;
    }

    private static Field createField(EncodedField encodedField, ImportManager importManager) {
        AccessFlags[] fieldAccessFlags = AccessFlags.getAccessFlagsForClass(encodedField.accessFlags);
        Set<Modifier> fieldModifiers = DalvikBytecodeUtil.getModifiers(fieldAccessFlags);
        String fieldName = encodedField.field.getFieldName().getStringValue();
        JavaType fieldType = DalvikBytecodeUtil.newType(encodedField.field.getFieldType(), importManager);
        return new Field(fieldModifiers, fieldName, fieldType, null);
    }

    @Override
    public Class createClass(ImportManager importManager) {
        String className = Type.getType(item.getClassType().getTypeDescriptor())
                .getClassName();
        String packageName = null;
        String simpleClassName = null;
        int lastDotIndex = className.lastIndexOf('.');
        if (lastDotIndex != -1) {
            packageName = className.substring(0, lastDotIndex);
            simpleClassName = className.substring(lastDotIndex + 1);
        } else { // class is in default package
            packageName = "";
            simpleClassName = className;
        }
        Package _package = new Package(packageName);
        ClassName superClassName = null;
        if (item.getSuperclass() != null) {
            superClassName
                    = importManager.newClassName(Type.getType(item.getSuperclass().getTypeDescriptor()).getClassName());
        }
        AccessFlags[] accessFlags = AccessFlags.getAccessFlagsForClass(item.getAccessFlags());
        Set<Modifier> classModifiers = DalvikBytecodeUtil.getModifiers(accessFlags);
        List<ClassName> interfaceNames = new ArrayList<ClassName>();
        if (item.getInterfaces() != null) {
            for (TypeIdItem interfaceType : item.getInterfaces().getTypes()) {
                ClassName interfaceName
                        = importManager.newClassName(Type.getType(interfaceType.getTypeDescriptor()).getClassName());
                interfaceNames.add(interfaceName);
            }
        }

        Class _class = new Class(_package, simpleClassName, superClassName,
                                 interfaceNames, classModifiers);

        for (EncodedField encodedField : item.getClassData().getInstanceFields()) {
            _class.addField(createField(encodedField, importManager));
        }
        for (EncodedField encodedField : item.getClassData().getStaticFields()) {
            _class.addField(createField(encodedField, importManager));
        }
        return _class;
    }

    public Collection<MethodFactory> createMethodFactories() {
        List<MethodFactory> factories = new ArrayList<MethodFactory>();
        for (EncodedMethod encodedMethod  : item.getClassData().getDirectMethods()) {
            factories.add(new DalvikMethodFactory(encodedMethod));
        }
        for (EncodedMethod encodedMethod  : item.getClassData().getVirtualMethods()) {
            factories.add(new DalvikMethodFactory(encodedMethod));
        }
        return factories;
    }

}
