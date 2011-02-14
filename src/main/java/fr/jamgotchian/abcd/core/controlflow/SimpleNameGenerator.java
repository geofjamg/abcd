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
package fr.jamgotchian.abcd.core.controlflow;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class SimpleNameGenerator implements NameGenerator {

    private static final String DIGITS = "abcdefghijklmnopqrstuvwxyz";

    private int counter;

    public SimpleNameGenerator(int counter) {
        this.counter = counter;
    }

    public SimpleNameGenerator() {
        this(0);
    }

    public String generate() {
        StringBuilder builder = new StringBuilder();
        final int base = DIGITS.length();
        // special case
        if (counter == 0) {
            builder.append(DIGITS.charAt(0));
        } else {
            int n = counter;
            while (n > 0) {
                int d = n % base;
                builder.insert(0, DIGITS.charAt(d));
                n = n / base;
            }
        }
        counter++;
        return builder.toString();
    }
}
