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

import java.awt.Color;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Colors {

    private Colors() {
    }

    public static String toString(Color c) {
		char[] buf = new char[7];
		buf[0] = '#';
		String s = Integer.toHexString(c.getRed());
		if (s.length() == 1) {
			buf[1] = '0';
			buf[2] = s.charAt(0);
		}
		else {
			buf[1] = s.charAt(0);
			buf[2] = s.charAt(1);
		}
		s = Integer.toHexString(c.getGreen());
		if (s.length() == 1) {
			buf[3] = '0';
			buf[4] = s.charAt(0);
		}
		else {
			buf[3] = s.charAt(0);
			buf[4] = s.charAt(1);
		}
		s = Integer.toHexString(c.getBlue());
		if (s.length() == 1) {
			buf[5] = '0';
			buf[6] = s.charAt(0);
		}
		else {
			buf[5] = s.charAt(0);
			buf[6] = s.charAt(1);
		}
		return String.valueOf(buf);
	}
}
