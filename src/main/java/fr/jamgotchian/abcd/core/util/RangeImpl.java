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

import fr.jamgotchian.abcd.core.common.ABCDException;
import java.util.Iterator;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RangeImpl implements Range, Comparable<RangeImpl> {

    protected int first;
    protected int last;

    public RangeImpl(int first, int last) {
        if (last < first) {
            throw new IllegalArgumentException("last <= first");
        }
        this.first = first;
        this.last = last;
    }

    public int getFirst() {
        return first;
    }

    public int getLast() {
        return last;
    }

    public void setLast(int last) {
        this.last = last;
    }

    public void setFirst(int first) {
        this.first = first;
    }

    private class RangeIterator implements Iterator<Integer> {

        private int index = Math.max(first, 0);

        public boolean hasNext() {
            return index <= last;
        }

        public Integer next() {
            return index++;
        }

        public void remove() {
            throw new ABCDException("Can't remove instruction");
        }
    }

    public Iterator<Integer> iterator() {
        return new RangeIterator();
    }

    public boolean contains(int instn) {
        return instn >= first && instn <= last;
    }

    public boolean contains(int first, int last) {
        return this.first <= first && this.last >= last;
    }

    public boolean contains(Range other) {
        return contains(other.getFirst(), other.getLast());
    }

    public int size() {
        if (first < 0 || last < 0) {
            return 0;
        } else {
            return last - first;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof RangeImpl)) {
            return false;
        }

        RangeImpl other = (RangeImpl) obj;

        return first == other.first && last == other.last;
    }

    @Override
    public int hashCode() {
        return first + last;
    }

    public int compareTo(RangeImpl other) {
        if (first == other.first) {
            return last - other.last;
        } else {
            return first - other.first;
        }
    }

    @Override
    public String toString() {
        return "[" + (first == Integer.MIN_VALUE ? "-\u221E" : first) + ", "
                + (last == Integer.MAX_VALUE ? "+\u221E" : last) + "]";
    }
}
