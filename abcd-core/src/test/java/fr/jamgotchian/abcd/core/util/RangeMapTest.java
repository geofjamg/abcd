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

package fr.jamgotchian.abcd.core.util;

import java.util.Arrays;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RangeMapTest {
    
    private RangeMap<Range, String> map;
    
    public RangeMapTest() {
    }

    @Before
    public void setUp() {
        map = new RangeMap<>();
    }

    @After
    public void tearDown() {
        map = null;
    }

    @Test
    public void testOneEntry() {
        assertTrue(map.isEmpty());
        assertTrue(map.put(new RangeImpl(0, 10), "v1") == null);
        assertTrue(map.size() == 1);
        assertTrue(map.get(new RangeImpl(0, 10)).equals("v1"));
        assertTrue(map.findBeginningAt(0).equals("v1"));
        assertTrue(map.findBeginningAt(1) == null);
        assertTrue(map.findContaining(1).equals("v1"));
        assertTrue(map.findContaining(11) == null);
        assertTrue(map.remove(new RangeImpl(5, 8)) == null);
        assertTrue(map.size() == 1);
        assertTrue(map.remove(new RangeImpl(0, 10)).equals("v1"));
        assertTrue(map.isEmpty());
    }
    
    @Test
    public void testTwoEntries() {
        map.put(new RangeImpl(0, 10), "v1");
        assertTrue(map.findBeginningAt(0).equals("v1"));
        assertTrue(map.put(new RangeImpl(0, 5), "v2").equals("v1"));
        assertTrue(map.size() == 1);
        assertTrue(map.findBeginningAt(0).equals("v2"));
    }
    
    @Test
    public void testTwoEntriesOverlapping() {
        map.put(new RangeImpl(0, 10), "v1");
        try {
            map.put(new RangeImpl(5, 16), "v2");
            fail();
        } catch(IllegalArgumentException exc) {
        }
    }
    
    @Test
    public void testValues() {
        map.put(new RangeImpl(0, 10), "v1");
        map.put(new RangeImpl(15, 16), "v2");
        assertTrue(map.size() == 2);
        assertArrayEquals(map.values().toArray(), Arrays.asList("v1", "v2").toArray());
    }
}
    