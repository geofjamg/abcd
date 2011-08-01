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
import fr.jamgotchian.abcd.core.controlflow.EdgeAttribute;
import fr.jamgotchian.abcd.core.controlflow.util.TACInstWriter;
import fr.jamgotchian.abcd.core.graph.AttributeProvider;
import fr.jamgotchian.abcd.core.graph.DirectedGraphs;
import fr.jamgotchian.abcd.core.output.OutputUtil;
import fr.jamgotchian.abcd.core.region.BasicBlockRegion;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.RegionType;
import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DOTUtil {

    private DOTUtil() {
    }

    private static class RegionAttributeProvider implements AttributeProvider<Region> {

        public Map<String, String> getAttributes(Region region) {
            Map<String, String> attrs = new HashMap<String, String>(1);
            attrs.put("label", "\"" + region + " (" + region.getTypeName()+ ")\"");
            return attrs;
        }
    }

    private static class RangeAttributeProvider implements AttributeProvider<BasicBlock> {

        public Map<String, String> getAttributes(BasicBlock block) {
            Map<String, String> attrs = new HashMap<String, String>(4);
            attrs.put("shape", "box");
            attrs.put("color", "black");
            if (block.getType() != null) {
                switch (block.getType()) {
                    case ENTRY:
                    case EXIT:
                        attrs.put("shape", "ellipse");
                        attrs.put("color", "lightgrey");
                        attrs.put("style", "filled");
                        break;

                    case JUMP_IF:
                        attrs.put("shape", "diamond");
                        attrs.put("color", "cornflowerblue");
                        attrs.put("style", "filled");
                        break;

                    case SWITCH:
                        attrs.put("shape", "hexagon");
                        attrs.put("color", "chocolate");
                        attrs.put("style", "filled");
                        break;

                    case RETURN:
                        attrs.put("shape", "invhouse");
                        attrs.put("color", "orange");
                        attrs.put("style", "filled");
                        break;
                }
            }
            attrs.put("label", "\"" + block +"\"");
            return attrs;
        }
    }

    private static class BytecodeAttributeProvider implements AttributeProvider<BasicBlock> {

        public Map<String, String> getAttributes(BasicBlock block) {
            Map<String, String> attrs = new HashMap<String, String>(3);
            attrs.put("shape", "box");
            attrs.put("color", "black");
            attrs.put("label", "< " + OutputUtil.toDOTHTMLLike(block) + " >");
            return attrs;
        }
    }

    private static class TACAttributeProvider implements AttributeProvider<BasicBlock> {

        public Map<String, String> getAttributes(BasicBlock block) {
            Map<String, String> attrs = new HashMap<String, String>(3);
            attrs.put("shape", "box");
            attrs.put("color", "black");
            StringBuilder builder = new StringBuilder();
            builder.append("< ");
            if (block.getType() == BasicBlockType.ENTRY
                    || block.getType() == BasicBlockType.EXIT) {
                builder.append("<font color=\"black\">").append(block.getType())
                      .append("</font>");
            } else {
                builder.append(TACInstWriter.toDOTHTMLLike(block.getRange(),
                                                           block.getInstructions(),
                                                           block.getInputStack(),
                                                           block.getOutputStack()));
            }
            builder.append(" >");
            attrs.put("label", builder.toString());
            return attrs;
        }
    }

    private static class EdgeAttributeProvider implements AttributeProvider<Edge> {

        public Map<String, String> getAttributes(Edge edge) {
            Map<String, String> attrs = new HashMap<String, String>(3);
            if (edge.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
                attrs.put("color", "red");
            } else if (edge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                attrs.put("color", "green");
            } else {
                attrs.put("color", "black");
            }
            if (edge.isExceptional()) {
                attrs.put("style", "dotted");
            }
            if (edge.getValue() != null) {
                attrs.put("label", "\"" + edge.getValue() + "\"");
            }
            return attrs;
        }
    }

    public enum DisplayMode {
        RANGE,
        BYTECODE,
        TAC
    }

    private static final Map<DisplayMode, AttributeProvider<BasicBlock>>
            BASIC_BLOCK_ATTRIBUTE_PROVIDERS;

    public static final AttributeProvider<Edge> EDGE_ATTRIBUTE_PROVIDER
            = new EdgeAttributeProvider();

    public static final AttributeProvider<Region> REGION_ATTRIBUTE_PROVIDER
            = new RegionAttributeProvider();

    public static final AttributeProvider<BasicBlock> RANGE_ATTRIBUTE_PROVIDER
            = new RangeAttributeProvider();

    public static final AttributeProvider<BasicBlock> BYTECODE_ATTRIBUTE_PROVIDER
            = new BytecodeAttributeProvider();

    public static final AttributeProvider<BasicBlock> TAC_ATTRIBUTE_PROVIDER
            = new TACAttributeProvider();

    static {
        Map<DisplayMode, AttributeProvider<BasicBlock>> providers
                = new EnumMap<DisplayMode, AttributeProvider<BasicBlock>>(DisplayMode.class);
        providers.put(DisplayMode.RANGE, RANGE_ATTRIBUTE_PROVIDER);
        providers.put(DisplayMode.BYTECODE, BYTECODE_ATTRIBUTE_PROVIDER);
        providers.put(DisplayMode.TAC, TAC_ATTRIBUTE_PROVIDER);
        BASIC_BLOCK_ATTRIBUTE_PROVIDERS = Collections.unmodifiableMap(providers);
    }

    private static void writeBasicBlock(Writer writer, BasicBlock block,
                                        DisplayMode mode) throws IOException {
        DirectedGraphs.writeVertexDOT(writer, block, BASIC_BLOCK_ATTRIBUTE_PROVIDERS.get(mode));
    }

    private static void writeEdge(Writer writer, Edge edge, BasicBlock source,
                                  BasicBlock target) throws IOException {
        DirectedGraphs.writeEdgeDOT(writer, edge, source, target, EDGE_ATTRIBUTE_PROVIDER);
    }

    private static void writeIndent(Writer writer, int indent) throws IOException {
        for (int i = 0; i < indent; i++) {
            writer.append("  ");
        }
    }

    private static void writeRegion(Region region, Writer writer, DisplayMode mode,
                                    int indent) throws IOException {
        if (region.getType() == RegionType.BASIC_BLOCK) {
            writeIndent(writer, indent);
            writeBasicBlock(writer, ((BasicBlockRegion) region).getBasicBlock(), mode);
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
                                Writer writer, DisplayMode mode) throws IOException {
        writer.append("digraph CFG {\n");
        for (Edge edge : graph.getEdges()) {
            BasicBlock source = graph.getEdgeSource(edge);
            BasicBlock target = graph.getEdgeTarget(edge);
            writeEdge(writer, edge, source, target);
        }
        if (rootRegions == null) {
            for (BasicBlock block : graph.getBasicBlocks()) {
                writeBasicBlock(writer, block, mode);
            }
        } else {
            for (Region rootRegion : rootRegions) {
                writeRegion(rootRegion, writer, mode, 1);
            }
        }
        writer.append("}");
    }
}
