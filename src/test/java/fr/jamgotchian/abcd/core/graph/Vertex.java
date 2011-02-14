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

package fr.jamgotchian.abcd.core.graph;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Vertex implements Comparable<Vertex> {

    private String prefix; 
    
    private final int id;

    public Vertex(int id) {
        this(null, id);
    }
    
    public Vertex(String prefix, int id) {
        this.prefix = prefix;
        this.id = id;
    }

    public int getId() {
        return id;
    }

    @Override
    public String toString() {
        return (prefix != null ? prefix : "") + Integer.toString(id);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Vertex)) {
            return false;
        }
        return id == ((Vertex)obj).id;
    }

    @Override
    public int hashCode() {
        return id;
    }

    public int compareTo(Vertex o) {
        return id - o.id;
    }
}
