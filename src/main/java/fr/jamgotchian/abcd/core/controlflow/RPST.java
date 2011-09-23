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
package fr.jamgotchian.abcd.core.controlflow;

import fr.jamgotchian.abcd.core.graph.GraphvizUtil;
import fr.jamgotchian.abcd.core.util.Sets;
import java.io.IOException;
import java.io.PrintStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Build a refined program structure tree from control flow graph.
 *
 * This is an implementation of the algorithm described in the following paper :
 * The refined program structure tree, Calculate a fine grained PST taking
 * advantage of dominance information, Tobias Grosser, University of Passau,
 * May 19, 2010.
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPST {

    private static final Logger logger = Logger.getLogger(RPST.class.getName());

    private final BasicBlockRangeAttributeFactory vertexAttrFactory
            = new BasicBlockRangeAttributeFactory();

    private final EdgeAttributeFactory edgeAttrFactory
            = new EdgeAttributeFactory(true);

    private final ControlFlowGraph cfg;

    private final DominatorInfo<BasicBlock, Edge> domInfo;

    private final PostDominatorInfo<BasicBlock, Edge> postDomInfo;

    private final List<Region> regions = new ArrayList<Region>();

    private final Region topLevelRegion = new Region();

    public RPST(ControlFlowGraph cfg) {
        this.cfg = cfg;
        domInfo = cfg.getDominatorInfo();
        postDomInfo = cfg.getPostDominatorInfo();
        build();
    }

    public ControlFlowGraph getCFG() {
        return cfg;
    }

    private boolean derivedDomFrontier(BasicBlock bb, BasicBlock entry, BasicBlock exit) {
        for (BasicBlock p : cfg.getPredecessorsOf(bb)) {
            if (!(domInfo.dominate(entry, p) ? domInfo.dominate(exit, p) : true)) {
                return false;
            }
        }
        return true;
    }

    private boolean isTrivial(BasicBlock entry, BasicBlock exit) {
        Collection<Edge> outgoingEdges = cfg.getOutgoingEdgesOf(entry);
        if (outgoingEdges.size() == 1) {
            Edge outgoingEdge = outgoingEdges.iterator().next();
            return cfg.getEdgeTarget(outgoingEdge).equals(exit);
        }
        return false;
    }

    boolean isRegion(BasicBlock entry, BasicBlock exit) {
        if (isTrivial(entry, exit)) {
            // skip trivial regions
            return false;
        } else if (!domInfo.dominate(entry, exit)) {
            if (!Sets.isSubset(domInfo.getDominanceFrontierOf2(entry),
                               Collections.singleton(exit))) {
                return false;
            }
        } else {
            if (!Sets.isSubset(domInfo.getDominanceFrontierOf2(entry),
                               com.google.common.collect.Sets.union(domInfo.getDominanceFrontierOf2(exit),
                                          Collections.singleton(entry)))) {
                return false;
            }
            for (BasicBlock bb : domInfo.getDominanceFrontierOf2(entry)) {
                if (!derivedDomFrontier(bb, entry, exit)) {
                    return false;
                }
            }
            for (BasicBlock bb : domInfo.getDominanceFrontierOf2(exit)) {
                if (domInfo.strictlyDominate(entry, bb)) {
                    return false;
                }
            }
        }
        return true;
    }

    private Region findRegionWithEntry(BasicBlock bb) {
        for (Region region : regions) {
            if (region.getEntry().equals(bb)) {
                return region;
            }
        }
        return null;
    }

    private BasicBlock getNextExit(BasicBlock bb) {
        BasicBlock exit = postDomInfo.getImmediatePostDominatorOf(bb);
        if (exit != null) {
            Region region;
            while ((region = findRegionWithEntry(exit)) != null) {
                exit = region.getExit();
            }
        }
        return exit;
    }

    private void addRegion(Region region) {
        logger.log(Level.FINER, "New Region {0}", region);
        regions.add(region);
    }

    private void detectRegionsWithEntry(BasicBlock entry) {
        BasicBlock pd = entry;
        Region lastRegion = null;
        while ((pd = getNextExit(pd)) != null) {
            if (isRegion(entry, pd)) {
                Region region = new Region(entry, pd);
                addRegion(region);
                if (lastRegion != null) {
                    lastRegion.setParent(region);
                    logger.log(Level.FINEST, "Parent of region {0} is {1}",
                            new Object[] {lastRegion, region});
                } else {
                    entry.setParent(region);
                    logger.log(Level.FINEST, "Parent of BB {0} is {1}",
                            new Object[] {entry, region});
                }
                lastRegion = region;
            }
        }
    }

    private void detectRegions() {
        for (BasicBlock node : domInfo.getDominatorsTree().getNodesPostOrder()) {
            detectRegionsWithEntry(node);
        }
    }

    private Region findRegion2(BasicBlock bb) {
        for (Region region : regions) {
            if (region.getEntry().equals(bb) && region.getParent() == null) {
                return region;
            }
        }
        return null;
    }

    private void build(BasicBlock bb, Region region) {
        while (bb.equals(region.getExit())) {
            region = region.getParent();
        }
        Region child = findRegion2(bb);
        if (child != null) {
            logger.log(Level.FINEST, "Parent of region {0} is {1}",
                    new Object[] {child, region});
            child.setParent(region);
            region = bb.getParent();
        } else {
            logger.log(Level.FINEST, "Parent of BB {0} is {1}",
                    new Object[] {bb, region});
            bb.setParent(region);
        }
        for (BasicBlock c : domInfo.getDominatorsTree().getChildren(bb)) {
            build(c, region);
        }
    }

    private void build() {
        detectRegions();
        build(domInfo.getDominatorsTree().getRoot(), topLevelRegion);
    }

    public void print(PrintStream out) {
        print(out, topLevelRegion, 0);
    }

    private void printIndent(PrintStream out, int indentLevel) {
        for (int i = 0 ; i < indentLevel; i++) {
            out.print("  ");
        }
    }

    public void print(PrintStream out, Region region, int indentLevel) {
        printIndent(out, indentLevel);
        out.println(region);
        for (BasicBlock bb : region.getBasicBlocks()) {
            printIndent(out, indentLevel+1);
            out.println(bb);
        }
        for (Region child : region.getChildren()) {
            print(out, child, indentLevel+1);
        }
    }

    private void writeIndent(Writer writer, int indentLevel) throws IOException {
        for (int i = 0 ; i < indentLevel; i++) {
            writer.append("  ");
        }
    }

    private void exportRegion(Writer writer, Region region, int indentLevel) throws IOException {
        String clusterName = "cluster_" + Integer.toString(System.identityHashCode(region));
        writeIndent(writer, indentLevel);
        writer.append("subgraph ").append(clusterName).append(" {\n");
        for (BasicBlock bb : region.getBasicBlocks()) {
            writeIndent(writer, indentLevel);
            writer.append("  ")
                    .append(Integer.toString(System.identityHashCode(bb)))
                    .append(" ");
            Map<String, String> attrs = vertexAttrFactory.getAttributes(bb);
            GraphvizUtil.writeAttributes(writer, attrs);
            writeIndent(writer, indentLevel);
            writer.append("\n");
        }
        for (Region child : region.getChildren()) {
            exportRegion(writer, child, indentLevel+1);
        }
        writeIndent(writer, indentLevel);
        writer.append("}\n");
    }

    public void export(Writer writer) throws IOException {
        writer.append("digraph ").append("RPST").append(" {\n");
        exportRegion(writer, topLevelRegion, 1);
        for (Edge edge : cfg.getEdges()) {
            BasicBlock source = cfg.getEdgeSource(edge);
            BasicBlock target = cfg.getEdgeTarget(edge);
            writer.append("  ")
                    .append(Integer.toString(System.identityHashCode(source)))
                    .append(" -> ")
                    .append(Integer.toString(System.identityHashCode(target)));
            GraphvizUtil.writeAttributes(writer, edgeAttrFactory.getAttributes(edge));
            writer.append("\n");
        }
        writer.append("}\n");
    }
}
