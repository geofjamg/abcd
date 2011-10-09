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
package fr.jamgotchian.abcd.core.controlflow;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionTest {

    public RegionTest() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testInsertParent() {
        Region root = new Region(null, null, ParentType.ROOT);
        Region seq = new Region(null, null, ParentType.SEQUENCE);
        seq.setParent(root);
        Region first = new Region(null, null, ParentType.IF_THEN_ELSE);
        Region second = new Region(null, null, ParentType.BASIC_BLOCK);
        first.setParent(seq);
        second.setParent(seq);
        Region then = new Region(null, null, ParentType.BASIC_BLOCK);
        Region else_ = new Region(null, null, ParentType.BASIC_BLOCK);
        then.setParent(first);
        else_.setParent(first);
        assertTrue(first.getParent() == seq);
        Region inlined = new Region(null, null, ParentType.INLINED_FINALLY);
        first.insertParent(inlined);
        assertTrue(first.getParent() == inlined);
        assertTrue(inlined.getParent() == seq);
        assertTrue(seq.getChildCount() == 2);
        assertTrue(seq.getChildren().contains(inlined));
        assertTrue(seq.getChildren().contains(second));
        assertTrue(inlined.getChildCount() == 1);
        assertTrue(inlined.getChildren().contains(first));
    }
}
