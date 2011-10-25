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

import java.util.EnumSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class EdgeImpl implements Edge {

    private EdgeCategory category;

    private Object value;

    private final Set<EdgeAttribute> attributes;

    EdgeImpl() {
        attributes = EnumSet.noneOf(EdgeAttribute.class);
    }

    private EdgeImpl(EdgeImpl other) {
        category = other.category;
        value = other.value;
        attributes = EnumSet.copyOf(other.attributes);
    }

    public EdgeCategory getCategory() {
        return category;
    }

    public void setCategory(EdgeCategory category) {
        this.category = category;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public void addAttribute(EdgeAttribute attr) {
        attributes.add(attr);
    }

    public boolean hasAttribute(EdgeAttribute attr) {
        return attributes.contains(attr);
    }

    public void removeAttribute(EdgeAttribute attr) {
        attributes.remove(attr);
    }

    public void resetAttributes() {
        attributes.clear();
    }

    public void resetState() {
        category = null;
        attributes.remove(EdgeAttribute.LOOP_BACK_EDGE);
        attributes.remove(EdgeAttribute.LOOP_EXIT_EDGE);
        attributes.remove(EdgeAttribute.SELF_LOOP_EDGE);
    }

    @Override
    public Edge clone() {
        return new EdgeImpl(this);
    }

    @Override
    public String toString() {
        return "Edge[value=" + value + ", attrs=" + attributes + "]";
    }
}
