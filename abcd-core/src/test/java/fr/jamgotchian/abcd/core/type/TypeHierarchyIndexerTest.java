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

import fr.jamgotchian.abcd.core.util.Collections3;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TypeHierarchyIndexerTest {

    private TypeHierarchyIndexer indexer;

    public TypeHierarchyIndexerTest() {
    }

    @Before
    public void setUp() {
        indexer = new TypeHierarchyIndexer(TypeHierarchyIndexerTest.class.getClassLoader());
    }

    @After
    public void tearDown() {
    }

    @Test
    public void test1() {
        assertTrue(indexer.getFirstIndex() == 0);
        assertTrue(indexer.getLastIndex() == 7);
        assertArrayEquals(Collections3.toIntArray(indexer.getPrimitiveTypeIndexes()),
                          new int[] {0, 1, 2, 3, 4, 5, 6, 7});
    }

    @Test
    public void test2() {
        JavaType javaLangStringType = JavaType.newRefType(new ClassNameImpl(String.class.getName(), null));
        indexer.addIndex(javaLangStringType);
        assertTrue(indexer.getFirstIndex() == 0);
        assertTrue(indexer.getLastIndex() == 12);
        assertArrayEquals(Collections3.toIntArray(indexer.getReferenceTypeIndexes()),
                          new int[] {8, 9, 10, 11, 12});
        assertArrayEquals(Collections3.toIntArray(indexer.getParentIndexes(javaLangStringType)),
                          new int[] {8, 9, 10, 11, 12});
    }
}
