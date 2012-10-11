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

package fr.jamgotchian.abcd.core.math;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class MatrixTest {

    public MatrixTest() {
    }

    @Test
    public void test() {
        Matrix<Integer> m = new Matrix<Integer>(2, 2, 0);
        assertTrue(m.getRowCount() == 2);
        assertTrue(m.getColumnCount() == 2);
        assertTrue(m.getValue(0, 0) == 0);
        assertTrue(m.getValue(1, 0) == 0);
        assertTrue(m.getValue(0, 1) == 0);
        assertTrue(m.getValue(1, 1) == 0);
        m.setValue(0, 0, 1);
        m.setValue(1, 0, 2);
        m.setValue(0, 1, 3);
        m.setValue(1, 1, 4);
        assertTrue(m.getValue(0, 0) == 1);
        assertTrue(m.getValue(1, 0) == 2);
        assertTrue(m.getValue(0, 1) == 3);
        assertTrue(m.getValue(1, 1) == 4);
    }

}