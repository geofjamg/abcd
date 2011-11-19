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
public class SimpleNameGeneratorTest {

    private SimpleVariableNameProvider provider;

    public SimpleNameGeneratorTest() {
    }

    @Before
    public void setUp() {
        provider = new SimpleVariableNameProvider();
    }

    @After
    public void tearDown() {
        provider = null;
    }

    private void generate(int n, int offset) {
        for (int i = offset; i < offset + n; i++) {
            provider.getName(new Variable(i), true);
        }
    }

    @Test
    public void testGenerate() {
        assertTrue(provider.getName(new Variable(0), true).equals("a"));
        assertTrue(provider.getName(new Variable(1), true).equals("b"));
        generate(24,  2);
        assertTrue(provider.getName(new Variable(26), true).equals("ba"));
        assertTrue(provider.getName(new Variable(27), true).equals("bb"));
    }

}