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
package fr.jamgotchian.abcd.core.tac.model;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class VariableID {

    public static final int UNDEFINED_VERSION = -1;

    private final int index;

    private int version;

    public VariableID(int index, int version) {
        this.index = index;
        this.version = version;
    }

    public VariableID(int index) {
        this(index, UNDEFINED_VERSION);
    }

    public int getIndex() {
        return index;
    }

    public int getVersion() {
        return version;
    }

    public void setVersion(int version) {
        this.version = version;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof VariableID) {
            VariableID id = (VariableID) obj;
            return index == id.index
                    && version == id.version;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return index + version;
    }

    @Override
    public VariableID clone() {
        return new VariableID(index, version);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        if (index < 0) {
            builder.append("_t").append(-index);
        } else {
            builder.append("v").append(index);
        }
        if (version != VariableID.UNDEFINED_VERSION) {
            builder.append(".").append(version);
        }
        return builder.toString();
    }
}
