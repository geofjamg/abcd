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
package fr.jamgotchian.abcd.core.bytecode.java;

import fr.jamgotchian.abcd.core.bytecode.ClassFactory;
import fr.jamgotchian.abcd.core.bytecode.ABCDDataSource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JarFileDataSource implements ABCDDataSource {

    private final JarFile jarFile;

    public JarFileDataSource(JarFile jarFile) {
        this.jarFile = jarFile;
    }

    @Override
    public Collection<ClassFactory> createClassFactories() throws IOException {
        List<ClassFactory> factories = new ArrayList<>();
        for (JarEntry entry : Collections.list(jarFile.entries())) {
            if (entry.getName().endsWith(".class")) {
                factories.add(new JavaBytecodeClassFactory(jarFile.getInputStream(entry)));
            }
        }
        return factories;
    }
}
