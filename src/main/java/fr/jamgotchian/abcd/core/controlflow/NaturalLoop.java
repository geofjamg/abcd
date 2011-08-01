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

import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NaturalLoop {

    private final BasicBlock head;
    
    private final Set<BasicBlock> body;

    private final Set<Edge> exits;
    
    public NaturalLoop(BasicBlock head, Set<BasicBlock> body) {
        this.head = head;
        this.body = body;
        this.exits = new HashSet<Edge>();
    }
    
    public BasicBlock getHead() {
        return head;
    }

    public Set<BasicBlock> getBody() {
        return body;
    }

    public Set<Edge> getExits() {
        return exits;
    }
    
    @Override
    public String toString() {
        return getClass().getSimpleName() + "(head=" + head + ", body=" + body + ")";
    }
}
