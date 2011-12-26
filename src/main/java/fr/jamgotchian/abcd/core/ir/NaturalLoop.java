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
import java.util.Collection;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NaturalLoop {

    private final ControlFlowGraph cfg;

    private final Edge backEdge;

    private final List<BasicBlock> body;

    private NaturalLoop parent;

    private final List<NaturalLoop> children = new ArrayList<NaturalLoop>();

    public NaturalLoop(ControlFlowGraph cfg, Edge backEdge, List<BasicBlock> body) {
        this.cfg = cfg;
        this.backEdge = backEdge;
        this.body = body;
    }

    public Edge getBackEdge() {
        return backEdge;
    }

    public List<BasicBlock> getBody() {
        return body;
    }

    public NaturalLoop getParent() {
        return parent;
    }

    public void setParent(NaturalLoop parent) {
        if (this.parent != null) {
            this.parent.children.remove(this);
        }
        this.parent = parent;
        this.parent.children.add(this);
    }

    public List<NaturalLoop> getChildren() {
        return children;
    }

    public BasicBlock getHead() {
        return cfg.getEdgeTarget(backEdge);
    }

    public BasicBlock getTail() {
        return cfg.getEdgeSource(backEdge);
    }

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

    public boolean isInfinite() {
        return getExits().isEmpty();
    }

    public static String toString(Collection<NaturalLoop> outermostLoops) {
        StringBuilder builder = new StringBuilder();
        print(outermostLoops, builder);
        return builder.toString();
    }

    public static void print(Collection<NaturalLoop> loops, StringBuilder builder) {
        print(loops, builder, 0);
    }

    private static void print(Collection<NaturalLoop> loops, StringBuilder builder, int depth) {
        for (NaturalLoop loop : loops) {
            for (int i = 0; i < depth * 4; i++) {
                builder.append(' ');
            }
            builder.append(loop.toString()).append("\n");
            print(loop.getChildren(), builder, depth+1);
        }
    }

    @Override
    public String toString() {
        return  "NaturalLoop(head=" + getHead() + ", tail=" + getTail() + ")";
    }
}
