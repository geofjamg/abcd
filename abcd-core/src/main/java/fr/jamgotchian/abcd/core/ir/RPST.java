/*
 * Copyright (C) 2012 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

import fr.jamgotchian.abcd.core.graph.Filter;
import fr.jamgotchian.abcd.core.graph.MutableTree;
import fr.jamgotchian.abcd.core.graph.NoTextGraphvizRenderer;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.graph.Trees;
import static fr.jamgotchian.abcd.core.graph.GraphvizUtil.*;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPST {

    private static final Logger LOGGER = LoggerFactory.getLogger(RPST.class);

    public enum ExportType {
        GRAPH,
        TREE
    }

    private static final EdgeGraphvizRenderer EDGE_GRAPHVIZ_RENDERER
            = new EdgeGraphvizRenderer(true);

    private static final RangeGraphvizRenderer RANGE_GRAPHIZ_RENDERER
            = new RangeGraphvizRenderer();

    private static final RegionGraphvizRenderer REGION_GRAPHIZ_RENDERER
            = new RegionGraphvizRenderer();

    private static final NoTextGraphvizRenderer OBJECT_GRAPHIZ_RENDERER
            = new NoTextGraphvizRenderer();

    private ControlFlowGraph cfg;

    private MutableTree<Region, Object> tree;

    public RPST(ControlFlowGraph cfg, Region rootRegion) {
        this.cfg = cfg;
        this.tree = Trees.newTree(rootRegion);
    }

    public ControlFlowGraph getCfg() {
        return cfg;
    }

    public Tree<Region, Object> getTree() {
        return Trees.unmodifiableTree(tree);
    }

    public Region getRootRegion() {
        return tree.getRoot();
    }

    public boolean containsRegion(Region region) {
        return tree.containsNode(region);
    }

    public Region getParent(Region region) {
        return tree.getParent(region);
    }

    public void setNewParent(Region region, Region newParent) {
        tree.setNewParent(region, newParent);
    }

    public void addRegion(Region region, Region parent) {
        tree.addNode(parent, region, new Object());
    }

    public void insertRegion(Region region, Region child) {
        tree.insertNode(region, child, new Object());
        region.setChildType(child.getChildType());
        child.setChildType(ChildType.UNDEFINED);
    }

    public void addRPST(RPST rpst, Region root, Region parent) {
        tree.addTree(rpst.getTree().getSubTree(root), parent, new Object());
    }

    public Set<Region> getChildren(Region region) {
        return tree.getChildren(region);
    }

    public Region getEntryChild(final Region region) {
        return getChildWithEntry(region, region.getEntry());
    }

    public Region getChildWithEntry(Region region, final BasicBlock entry) {
        return tree.getFirstChild(region, new Filter<Region>() {
            @Override
            public boolean accept(Region child) {
                return child.getEntry().equals(entry);
            }
        });
    }

    public Set<Region> getChildrenWithExit(Region region, final BasicBlock exit) {
        return tree.getChildren(region, new Filter<Region>() {
            @Override
            public boolean accept(Region child) {
                return child.getExit().equals(exit);
            }
        });
    }

    public Region getFirstChild(Region region) {
        return tree.getFirstChild(region);
    }

    public Region getFirstChild(Region region, final ChildType childType) {
        return tree.getFirstChild(region, new Filter<Region>() {
            @Override
            public boolean accept(Region child) {
                return child.getChildType() == childType;
            }
        });
    }

    public Collection<Region> getChildren(Region region, final ChildType childType) {
        return tree.getChildren(region, new Filter<Region>() {
            @Override
            public boolean accept(Region child) {
                return child.getChildType() == childType;
            }
        });
    }

    public int getChildCount(Region region) {
        return tree.getChildrenCount(region);
    }

    public void removeRegion(Region region) {
        tree.removeNode(region);
    }

    public void removeSubtree(Region subtreeRootRegion) {
        tree.removeSubtree(subtreeRootRegion);
    }

    public void removeChildren(Region region) {
        for (Region child : new ArrayList<Region>(tree.getChildren(region))) {
            tree.removeSubtree(child);
        }
    }

    public List<Region> getSubRegions(Region region) {
        return tree.getSubTree(region).getNodesPreOrder();
    }

    public List<Region> getRegionsPostOrder() {
        return tree.getNodesPostOrder();
    }

    public List<Region> getRegionsPreOrder() {
        return tree.getNodesPreOrder();
    }

    public Set<BasicBlock> getBasicBlocks(Region region) {
        Set<BasicBlock> bbs = new LinkedHashSet<BasicBlock>();
        addBasicBlocks(region, bbs);
        return bbs;
    }

    private void addBasicBlocks(Region region, Set<BasicBlock> bbs) {
        if (region.getParentType() == ParentType.BASIC_BLOCK) {
            bbs.add((BasicBlock) region);
        }
        for (Region child : getChildren(region)) {
            addBasicBlocks(child, bbs);
        }
    }

    public void print(Appendable out) {
        try {
            print(out, getRootRegion(), 0);
        } catch (IOException e) {
            LOGGER.error(e.toString(), e);
        }
    }

    public void print(Appendable out, Region region, int indentLevel) throws IOException {
        writeIndent(out, indentLevel);
        out.append(region.getChildType().toString()).append("\n");
        writeIndent(out, indentLevel+1);
        out.append("+").append(region.getParentType().toString()).append(" ")
                .append(region.toString()).append("\n");
        for (Region child : getChildren(region)) {
            print(out, child, indentLevel+2);
        }
    }

    private void exportRegion(Writer writer, int paneId, Region region, int indentLevel) throws IOException {
        if (region.getParentType() == ParentType.BASIC_BLOCK) {
            BasicBlock bb = (BasicBlock) region;
            writeIndent(writer, indentLevel);
            writer.append(Integer.toString(System.identityHashCode(bb)))
                    .append(Integer.toString(paneId))
                    .append(" ");
            Map<String, String> attrs = RANGE_GRAPHIZ_RENDERER.getAttributes(bb);
            writeAttributes(writer, attrs);
            writeIndent(writer, indentLevel);
            writer.append("\n");
        } else {
            String clusterName = "cluster_" + Integer.toString(System.identityHashCode(region))
                    + Integer.toString(paneId);
            writeIndent(writer, indentLevel);
            writer.append("subgraph ").append(clusterName).append(" {\n");
            writeIndent(writer, indentLevel+1);
            writer.append("fontsize=\"10\";\n");
            writeIndent(writer, indentLevel+1);
            writer.append("labeljust=\"left\";\n");
            if (region.getParentType() != null) {
                writeIndent(writer, indentLevel+1);
                writer.append("label=\"").append(region.getParentType().toString())
                        .append(" ").append(region.toString()).append("\";\n");
            }
            for (Region child : getChildren(region)) {
                exportRegion(writer, paneId, child, indentLevel+1);
            }
            writeIndent(writer, indentLevel);
            writer.append("}\n");
        }
    }

    private void exportGraph(Writer writer, String title, boolean error, int paneId, int indentLevel) throws IOException {
        writeIndent(writer, indentLevel);
        writer.append("subgraph cluster_title_").append(Integer.toString(paneId)).append(" {\n");
        writeIndent(writer, indentLevel+1);
        writer.append("color=\"").append(error ? "red" : "black").append("\";\n");
        writeIndent(writer, indentLevel+1);
        writer.append("fontsize=\"18\";\n");
        writeIndent(writer, indentLevel+1);
        writer.append("labeljust=\"left\";\n");
        writeIndent(writer, indentLevel+1);
        writer.append("label=<<font color=\"").append(error ? "red" : "black")
                .append("\">").append(title).append("</font>>;\n");

        // export regions recursively
        exportRegion(writer, paneId, getRootRegion(), indentLevel+1);

        // then export isolated basic blocks
        for (BasicBlock bb : cfg.getBasicBlocks()) {
            if (!tree.containsNode(bb)) {
                writeIndent(writer, indentLevel+1);
                writer.append(Integer.toString(System.identityHashCode(bb)))
                        .append(Integer.toString(paneId))
                        .append(" ");
                Map<String, String> attrs = RANGE_GRAPHIZ_RENDERER.getAttributes(bb);
                writeAttributes(writer, attrs);
                writer.append("\n");
            }
        }

        // and finally export edges
        for (Edge edge : cfg.getEdges()) {
            BasicBlock source = cfg.getEdgeSource(edge);
            BasicBlock target = cfg.getEdgeTarget(edge);
            writeIndent(writer, indentLevel+1);
            writer.append(Integer.toString(System.identityHashCode(source)))
                    .append(Integer.toString(paneId))
                    .append(" -> ")
                    .append(Integer.toString(System.identityHashCode(target)))
                    .append(Integer.toString(paneId));
            writeAttributes(writer, EDGE_GRAPHVIZ_RENDERER.getAttributes(edge));
            writer.append("\n");
        }

        writeIndent(writer, indentLevel);
        writer.append("}\n");
    }

    public void exportPane(Writer writer, ExportType type, String title, int paneId, int indentLevel) throws IOException {
        boolean error = false;
        for (Region region : tree.getNodes()) {
            if (region.getParentType() == ParentType.UNDEFINED) {
                error = true;
                break;
            }
        }
        switch (type) {
            case GRAPH:
                exportGraph(writer, title, error, paneId, indentLevel);
                break;

            case TREE:
                tree.exportPane(writer, title, paneId, indentLevel,
                        REGION_GRAPHIZ_RENDERER, OBJECT_GRAPHIZ_RENDERER);
                break;

            default:
                throw new InternalError();
        }
    }

    public void export(Writer writer, ExportType type, String title) throws IOException {
        writer.append("digraph RPST {\n");
        exportPane(writer, type, title, 0, 1);
        writer.append("}\n");
    }

    public void export(Writer writer, ExportType type) throws IOException {
        export(writer, type, cfg.getName());
    }

    public void export(String fileName, ExportType type) {
        Writer writer = null;
        try {
            writer = new FileWriter(fileName);
            export(writer, type);
        } catch (IOException e) {
            LOGGER.error(e.toString(), e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    LOGGER.error(e.toString(), e);
                }
            }
        }
    }
}
