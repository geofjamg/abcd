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
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Sets {

    private Sets() {
    }

    public static <E> Set<E> intersection(Set<E> set1, Set<E> set2) {
        return intersection(Arrays.asList(set1, set2));
    }

    public static <E> Set<E> intersection(Collection<Set<E>> sets) {
        if (sets.isEmpty()) {
            return Collections.emptySet();
        } else {
            Iterator<Set<E>> it = sets.iterator();
            Set<E> intersect = new HashSet<>(it.next());
            while (it.hasNext()) {
                intersect.retainAll(it.next());
            }
            return intersect;
        }
    }

    public static <E> boolean isSubset(Set<E> set1, Set<E> set2) {
        if (set1.isEmpty()) {
            return true;
        }
        for (E e : set1) {
            if (!set2.contains(e)) {
                return false;
            }
        }
        return true;
    }
}
