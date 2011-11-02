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
package fr.jamgotchian.abcd.core.bytecode;

import fr.jamgotchian.abcd.core.ClassFactory;
import fr.jamgotchian.abcd.core.DataSource;
import fr.jamgotchian.abcd.core.common.ABCDException;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.EmptyVisitor;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ClassFileDataSource implements DataSource {

    private static final Logger logger
            = Logger.getLogger(ClassFileDataSource.class.getName());

    private final File classDir;

    private final String className;

    private class InnerClassChecker extends EmptyVisitor {

        private final String outerClassName;

        private String innerClassName;

        private boolean innerClass = false;

        private InnerClassChecker(String outerClassName) {
            this.outerClassName = outerClassName;
        }

        public boolean isInnerClass() {
            return innerClass;
        }

        @Override
        public void visit(int version, int access, String name, String signature,
                String superName, String[] interfaces) {
            innerClassName = name.replace('/', '.');
        }

        @Override
        public FieldVisitor visitField(int access, String name, String desc,
                String signature, Object value) {
            if ((access & Opcodes.ACC_SYNTHETIC) != 0) {
                Type type = Type.getType(desc);
                if (type.getSort() == Type.OBJECT
                        && outerClassName.equals(type.getClassName())) {
                    innerClass = true;
                    return null;
                }
            }
            return super.visitField(access, name, desc, signature, value);
        }
    }

    public ClassFileDataSource(File classDir, String className) {
        this.classDir = classDir;
        this.className = className;
    }

    public Collection<ClassFactory> createClassFactories() throws IOException {
        // check that the class exists
        File classFile = new File(classDir.getCanonicalPath() + File.separator
                + className.replace('.', File.separatorChar) + ".class");
        if (!classFile.exists()) {
            throw new ABCDException(classFile + " does not exist");
        }
        File packageDir = classFile.getParentFile();

        // list inner classes
        File[] innerClassFiles = packageDir.listFiles(new FileFilter() {

            public boolean accept(File pathname) {
                if (pathname.getName().endsWith(".class")) {
                    try {
                        ClassReader cr = new ClassReader(new FileInputStream(pathname));
                        InnerClassChecker checker = new InnerClassChecker(className);
                        cr.accept(checker, 0);
                        return checker.isInnerClass();
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, e.toString(), e);
                    }
                }
                return false;
            }
        });

        List<ClassFactory> factories
                = new ArrayList<ClassFactory>(innerClassFiles.length+1);
        factories.add(new JavaBytecodeClassFactory(new FileInputStream(classFile)));
        for (File innerClassFile : innerClassFiles) {
            factories.add(new JavaBytecodeClassFactory(new FileInputStream(innerClassFile)));
        }
        return factories;
    }

}
