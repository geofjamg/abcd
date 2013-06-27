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
package fr.jamgotchian.abcd.core.ir;

import com.google.common.base.Objects;
import java.util.Collection;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CaseValues implements Comparable<CaseValues> {

    private static final String DEFAULT = "default";

    private final SortedSet<String> values;

    public static CaseValues newValue(String value) {
        if (DEFAULT.equals(value)) {
            throw new IllegalArgumentException("default is a reserved keyword");
        }
        return new CaseValues(value);
    }

    public static CaseValues newDefaultValue() {
        return new CaseValues(DEFAULT);
    }

    public static boolean isDefault(String value) {
        return DEFAULT.equals(value);
    }

    private CaseValues(String value) {
        if (value == null) {
            throw new IllegalArgumentException("value == null");
        }
        values = new TreeSet<>();
        values.add(value);
    }

    public Collection<String> getValues() {
        return values;
    }

    public void merge(CaseValues other) {
        values.addAll(other.values);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof CaseValues)) {
            return false;
        }
        CaseValues caseValue = (CaseValues) obj;
        return Objects.equal(values, caseValue.values);
    }

    @Override
    public int hashCode() {
       return values.hashCode();
    }

    @Override
    public int compareTo(CaseValues o) {
        String first = values.first();
        String otherFirst = o.values.first();
        return first.compareTo(otherFirst);
    }

    @Override
    public String toString() {
        return values.size() == 1 ? values.first() : values.toString();
    }
}
