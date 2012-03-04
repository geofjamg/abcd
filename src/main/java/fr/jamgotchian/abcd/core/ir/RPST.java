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

import fr.jamgotchian.abcd.core.graph.GraphvizUtil;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPST {

    private static final Logger LOGGER = Logger.getLogger(RPST.class.getName());

    private static final EdgeGraphvizRenderer EDGE_GRAPHVIZ_RENDERER
            = new EdgeGraphvizRenderer(true);

    private static final RangeGraphvizRenderer RANGE_GRAPHIZ_RENDERER
            = new RangeGraphvizRenderer();

    private ControlFlowGraph cfg;

    private Region rootRegion;

    private boolean typed = false;

    public RPST(ControlFlowGraph cfg, Region rootRegion) {
        this.cfg = cfg;
        this.rootRegion = rootRegion;
    }

    public ControlFlowGraph getCfg() {
        return cfg;
    }

    public void setCfg(ControlFlowGraph cfg) {
        this.cfg = cfg;
    }

    public Region getRootRegion() {
        return rootRegion;
    }

    public boolean isTyped() {
        return typed;
    }

    public void setTyped(boolean typed) {
        this.typed = typed;
    }

    private void visitRegionPostOrder(Region region, List<Region> regionsPostOrder) {
        for (Region child : region.getChildren()) {
            visitRegionPostOrder(child, regionsPostOrder);
        }
        regionsPostOrder.add(region);
    }

    public List<Region> getRegionsPostOrder() {
        List<Region> regionsPostOrder = new ArrayList<Region>();
        visitRegionPostOrder(rootRegion, regionsPostOrder);
        return regionsPostOrder;
    }

    public void print(Appendable out) {
        try {
            print(out, rootRegion, 0);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
        }
    }

    private void printSpace(Appendable out, int indentLevel) throws IOException {
        for (int i = 0 ; i < indentLevel; i++) {
            out.append("    ");
        }
    }

    public void print(Appendable out, Region region, int indentLevel) throws IOException {
        printSpace(out, indentLevel);
        out.append(region.getChildType().toString()).append("\n");
        printSpace(out, indentLevel+1);
        out.append("+").append(region.getParentType().toString()).append(" ")
                .append(region.toString()).append("\n");
        for (Region child : region.getChildren()) {
            print(out, child, indentLevel+2);
        }
    }

    private void writeSpace(Writer writer, int indentLevel) throws IOException {
        for (int i = 0 ; i < indentLevel; i++) {
            writer.append("  ");
        }
    }

    private void exportRegion(Writer writer, Region region, int indentLevel) throws IOException {
        if (region.isBasicBlock()) {
            BasicBlock bb = region.getEntry();
            writeSpace(writer, indentLevel);
            writer.append("  ")
                    .append(Integer.toString(System.identityHashCode(bb)))
                    .append(" ");
            Map<String, String> attrs = RANGE_GRAPHIZ_RENDERER.getAttributes(bb);
            GraphvizUtil.writeAttributes(writer, attrs);
            writeSpace(writer, indentLevel);
            writer.append("\n");
        } else {
            String clusterName = "cluster_" + Integer.toString(System.identityHashCode(region));
            writeSpace(writer, indentLevel);
            writer.append("subgraph ").append(clusterName).append(" {\n");
            writeSpace(writer, indentLevel+1);
            writer.append("fontsize=\"10\";\n");
            writeSpace(writer, indentLevel+1);
            writer.append("labeljust=\"left\";\n");
            if (region.getParentType() != null) {
                writeSpace(writer, indentLevel+1);
                writer.append("label=\"").append(region.getParentType().toString())
                        .append(" ").append(region.toString()).append("\";\n");
            }
            for (Region child : region.getChildren()) {
                exportRegion(writer, child, indentLevel+1);
            }
            writeSpace(writer, indentLevel);
            writer.append("}\n");
        }
    }

    public void export(Writer writer) throws IOException {
        writer.append("digraph ").append("RPST").append(" {\n");
        exportRegion(writer, rootRegion, 1);
        for (Edge edge : cfg.getEdges()) {
            BasicBlock source = cfg.getEdgeSource(edge);
            BasicBlock target = cfg.getEdgeTarget(edge);
            writer.append("  ")
                    .append(Integer.toString(System.identityHashCode(source)))
                    .append(" -> ")
                    .append(Integer.toString(System.identityHashCode(target)));
            GraphvizUtil.writeAttributes(writer, EDGE_GRAPHVIZ_RENDERER.getAttributes(edge));
            writer.append("\n");
        }
        writer.append("}\n");
    }

    public void export(String fileName) {
        Writer writer = null;
        try {
            writer = new FileWriter(fileName);
            export(writer);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
        } finally {
            if (writer != null) {
                try {
                    writer.close();
                } catch (IOException e) {
                    LOGGER.log(Level.SEVERE, e.toString(), e);
                }
            }
        }
    }
}
