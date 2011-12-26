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

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NaturalLoopImpl implements NaturalLoop {

    private final ControlFlowGraph cfg;

    private final Edge backEdge;

    private final List<BasicBlock> body;

    public NaturalLoopImpl(ControlFlowGraph cfg, Edge backEdge, List<BasicBlock> body) {
        this.cfg = cfg;
        this.backEdge = backEdge;
        this.body = body;
    }

    @Override
    public Edge getBackEdge() {
        return backEdge;
    }

    @Override
    public BasicBlock getHead() {
        return cfg.getEdgeTarget(backEdge);
    }

    @Override
    public BasicBlock getTail() {
        return cfg.getEdgeSource(backEdge);
    }

    @Override
    public List<BasicBlock> getBody() {
        return body;
    }

    @Override
    public List<Edge> getExits() {
        List<Edge> exits = new ArrayList<Edge>(1);
        for (BasicBlock bb : body) {
            for (Edge e : cfg.getOutgoingEdgesOf(bb)) {
                BasicBlock t = cfg.getEdgeTarget(e);
                if (!body.contains(t)) {
                    exits.add(e);
                }
            }
        }
        return exits;
    }

    @Override
    public boolean isInfinite() {
        return getExits().isEmpty();
    }

    @Override
    public String toString() {
        return  "NaturalLoop(head=" + getHead() + ", tail=" + getTail() + ")";
    }
}
