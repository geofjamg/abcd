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

package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockType;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.graph.VertexToString;
import fr.jamgotchian.abcd.core.output.OutputUtil;
import fr.jamgotchian.abcd.core.region.BasicBlockRegion;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.RegionType;
import java.io.IOException;
import java.io.Writer;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DOTUtil {

    private DOTUtil() {
    }

    private static class DefaultVertexToString<V> implements VertexToString<V> {
        public String toString(V vertex) {
            return vertex.toString();
        }
    }

    public enum BasicBlockWritingMode {
        INSTN_RANGE,
        BYTECODE,
        STATEMENTS
    }

    private static void writeBasicBlock(BasicBlock block, Writer writer,
                                        BasicBlockWritingMode mode) throws IOException {
        switch (mode) {
            case INSTN_RANGE:
                writeBasicBlockInstnRange(block, writer);
                break;

            case BYTECODE:
                writeBasicBlockBytecode(block, writer);
                break;

            case STATEMENTS:
                writeBasicBlockStatements(block, writer);
                break;

            default:
                throw new InternalError();
        }
    }

    private static void writeBasicBlockInstnRange(BasicBlock block, Writer writer) throws IOException {
        writer.append("\"").append(block.toString()).append("\" [");
        if (block.getType() != null) {
            switch (block.getType()) {
                case ENTRY:
                case EXIT:
                    writer.append("shape=ellipse, style=filled, color=lightgrey");
                    break;

                case JUMP_IF:
                    writer.append("shape=diamond, style=filled, color=cornflowerblue");
                    break;

                case SWITCH:
                    writer.append("shape=hexagon, style=filled, color=chocolate");
                    break;

                case RETURN:
                    writer.append("shape=invhouse, style=filled, color=orange");
                    break;

                default:
                    writer.append("shape=box, color=black");
                    break;
            }
        } else {
            writer.append("shape=box, color=black");
        }
        writer.append("];\n");
    }

    private static void writeBasicBlockBytecode(BasicBlock block, Writer writer) throws IOException {
        writer.append("\"").append(block.toString())
              .append("\" [shape=box, color=black, label=< ")
              .append(OutputUtil.toDOTHTMLLike(block))
              .append(" >];\n");
    }

    private static void writeBasicBlockStatements(BasicBlock block, Writer writer) throws IOException {
        BasicBlockAnalysisDataImpl data = (BasicBlockAnalysisDataImpl) block.getData();
        writer.append("\"").append(block.toString())
              .append("\" [shape=box, color=black, label=< ");
        if (block.getType() == BasicBlockType.ENTRY
                || block.getType() == BasicBlockType.EXIT) {
            writer.append("<font color=\"black\">").append(block.getType().toString())
                  .append("</font>");
        } else {
            writer.append(OutputUtil.toDOTHTMLLike(data.getStatements()));
        }
        writer.append(" >];\n");
    }

    private static <V, E> void writeEdge(DirectedGraph<V, Edge> graph, Edge edge,
            Writer writer, VertexToString<V> toStr) throws IOException {
        V source = graph.getEdgeSource(edge);
        V target = graph.getEdgeTarget(edge);
        writer.append("  \"").append(toStr.toString(source)).append("\" -> \"")
                .append(toStr.toString(target)).append("\" [");
        if (edge.isLoopBack()) {
            writer.append("color=red");
        } else if (edge.isLoopExit()) {
            writer.append("color=green");
        } else {
            writer.append("color=black");
        }
        if (edge.isExceptional()) {
            writer.append(", style=dotted");
        }
        if (edge.getValue() != null) {
            writer.append(", label=\"").append(edge.getValue().toString()).append("\"");
        }
        writer.append("];\n");
    }

    private static void writeIndent(Writer writer, int indent) throws IOException {
        for (int i = 0; i < indent; i++) {
            writer.append("  ");
        }
    }

    private static void writeRegion(Region region, Writer writer, BasicBlockWritingMode mode,
                                    int indent) throws IOException {
        if (region.getType() == RegionType.BASIC_BLOCK) {
            writeIndent(writer, indent);
            writeBasicBlock(((BasicBlockRegion) region).getBasicBlock(), writer, mode);
        } else {
            writeIndent(writer, indent);
            writer.append("subgraph \"cluster_").append(region.getName().toString()).append("\" {\n");
            writeIndent(writer, indent+1);
            writer.append("label=\"").append(region.getName().toString()).append(" (")
                    .append(region.getTypeName()).append(")\";\n");
            writeIndent(writer, indent+1);
            writer.append("fontcolor=blue;\n");
            writeIndent(writer, indent+1);
            writer.append("labeljust=l;\n");
            writeIndent(writer, indent+1);
            writer.append("color=blue\n");
            for (Region childRegion : region.getChildRegions()) {
                writeRegion(childRegion, writer, mode, indent+1);
            }
            writeIndent(writer, indent);
            writer.append("}\n");
        }
    }

    public static void writeCFG(ControlFlowGraph graph, Set<Region> rootRegions,
                                Writer writer, BasicBlockWritingMode mode) throws IOException {
        writer.append("digraph CFG {\n");
        VertexToString<BasicBlock> toStr = new DefaultVertexToString<BasicBlock>();
        for (Edge edge : graph.getEdges()) {
            writeEdge(graph.getGraph(), edge, writer, toStr);
        }
        if (rootRegions == null) {
            for (BasicBlock block : graph.getBasicBlocks()) {
                writeBasicBlock(block, writer, mode);
            }
        } else {
            for (Region rootRegion : rootRegions) {
                writeRegion(rootRegion, writer, mode, 1);
            }
        }
        writer.append("}");
    }

    public static <V> void writeGraph(DirectedGraph<V, Edge> graph, String name,
                                      Writer writer, VertexToString<V> toStr)
            throws IOException {
        writer.append("digraph ").append(name).append(" {\n");
        for (Edge edge : graph.getEdges()) {
            writeEdge(graph, edge, writer, toStr);
        }
        writer.append("}");
    }
}
