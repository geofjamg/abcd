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

import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.type.ClassNameFactory;
import fr.jamgotchian.abcd.core.type.JavaType;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NarrowestTypeTest {

    private ClassNameFactory classNameFactory;

    private NarrowestType instance;

    public NarrowestTypeTest() {
    }

    @Before
    public void setUp() {
        classNameFactory = new ImportManager();
        instance = new NarrowestType();
    }

    @After
    public void tearDown() {
        classNameFactory = null;
        instance = null;
    }

    @Test
    public void testNarrow() {
        instance.narrow(JavaType.INT, classNameFactory);
        instance.narrow(JavaType.INT, classNameFactory);
        assertTrue(instance.get().equals(JavaType.INT));
    }

    @Test
    public void testNarrow2() {
        instance.narrow(JavaType.INT, classNameFactory);
        instance.narrow(JavaType.FLOAT, classNameFactory);
        assertTrue(instance.get().equals(JavaType.FLOAT));
    }

    @Test
    public void testNarrow3() {
        instance.narrow(JavaType.BYTE, classNameFactory);
        instance.narrow(JavaType.CHAR, classNameFactory);
        assertTrue(instance.get().equals(JavaType.CHAR));
    }

    private static class A {
    }

    private static class B {
    }

    private static class C extends A {
    }

    private static class D extends A {
    }

    @Test
    public void testNarrow4() {
        instance.narrow(JavaType.newRefType(classNameFactory.newClassName(A.class.getName())), classNameFactory);
        instance.narrow(JavaType.newRefType(classNameFactory.newClassName(B.class.getName())), classNameFactory);
        assertTrue(instance.get().equals(JavaType.newRefType(classNameFactory.newClassName(Object.class.getName()))));
    }

    @Test
    public void testNarrow5() {
        instance.narrow(JavaType.newRefType(classNameFactory.newClassName(C.class.getName())), classNameFactory);
        instance.narrow(JavaType.newRefType(classNameFactory.newClassName(D.class.getName())), classNameFactory);
        assertTrue(instance.get().equals(JavaType.newRefType(classNameFactory.newClassName(A.class.getName()))));
    }
}
