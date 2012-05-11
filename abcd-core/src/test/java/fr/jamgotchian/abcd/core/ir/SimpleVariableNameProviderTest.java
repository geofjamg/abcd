/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

import org.junit.Test;
import org.junit.After;
import org.junit.Before;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SimpleVariableNameProviderTest {

    private SimpleVariableNameProvider provider;

    private VariableFactory varFactory;

    public SimpleVariableNameProviderTest() {
    }

    @Before
    public void setUp() {
        provider = new SimpleVariableNameProvider();
        varFactory = new VariableFactory();
    }

    @After
    public void tearDown() {
        provider = null;
        varFactory = null;
    }

    private void generate(int n, int offset) {
        for (int i = offset; i < offset + n; i++) {
            provider.getName(varFactory.create(i));
        }
    }

    @Test
    public void testGenerate() {
        assertTrue(provider.getName(varFactory.create(0)).equals("a"));
        assertTrue(provider.getName(varFactory.create(1)).equals("b"));
        generate(24,  2);
        assertTrue(provider.getName(varFactory.create(26)).equals("ba"));
        assertTrue(provider.getName(varFactory.create(27)).equals("bb"));
    }

}