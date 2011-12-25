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

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NaturalLoopImpl implements NaturalLoop {

    private final Edge backEdge;

    private final BasicBlock head;

    private final BasicBlock tail;

    private final List<BasicBlock> body;

    private final Set<Edge> exits;

    public NaturalLoopImpl(Edge backEdge, BasicBlock head, BasicBlock tail, List<BasicBlock> body) {
        this.backEdge = backEdge;
        this.head = head;
        this.tail = tail;
        this.body = body;
        this.exits = new HashSet<Edge>();
    }

    @Override
    public Edge getBackEdge() {
        return backEdge;
    }

    @Override
    public BasicBlock getHead() {
        return head;
    }

    @Override
    public BasicBlock getTail() {
        return tail;
    }

    @Override
    public List<BasicBlock> getBody() {
        return body;
    }

    @Override
    public Collection<Edge> getExits() {
        return exits;
    }

    @Override
    public void addExit(Edge e) {
        exits.add(e);
    }

    @Override
    public boolean isInfinite() {
        return exits.isEmpty();
    }

    @Override
    public String toString() {
        return  "NaturalLoop(head=" + head + ", tail=" + tail + ")";
    }
}
