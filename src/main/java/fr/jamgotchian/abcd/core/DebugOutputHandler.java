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
package fr.jamgotchian.abcd.core;

import fr.jamgotchian.abcd.core.analysis.DOTUtil;
import static fr.jamgotchian.abcd.core.analysis.DOTUtil.DisplayMode.*;
import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.controlflow.Edge;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.region.Region;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Set;
import java.util.logging.Level;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DebugOutputHandler extends DefaultOutputHandler {

    private final File outputDir;

    private final boolean drawRegions;

    private String baseName;

    private ControlFlowGraph graph;

    public DebugOutputHandler(boolean debug, OutputStream os, File outputDir,
                              boolean drawRegions) {
        super(debug, os);
        if (!outputDir.exists()) {
            throw new ABCDException(outputDir + " does not exist");
        }
        if (!outputDir.isDirectory()) {
            throw new ABCDException(outputDir + " is not a directory");
        }
        this.outputDir = outputDir;
        this.drawRegions = drawRegions;
    }

    @Override
    public void controlFlowGraphBuilt(ControlFlowGraph graph) {
        this.graph = graph;

        baseName = outputDir.getPath() + "/" + graph.getName();

        try {
            if (!drawRegions) {
                Writer writer = new FileWriter(baseName + "_CFG.dot");
                try {
                    DOTUtil.writeCFG(graph, null, writer, RANGE);
                } finally {
                    writer.close();
                }

                writer = new FileWriter(baseName + "_BC.dot");
                try {
                    DOTUtil.writeCFG(graph, null, writer, BYTECODE);
                } finally {
                    writer.close();
                }
            }

            Writer writer = new FileWriter(baseName + "_DT.dot");
            try {
                graph.getDominatorInfo().getDominatorsTree()
                        .writeDOT(writer, "DT", DOTUtil.RANGE_ATTRIBUTE_PROVIDER,
                                                DOTUtil.EDGE_ATTRIBUTE_PROVIDER);
            } finally {
                writer.close();
            }

            writer = new FileWriter(baseName + "_PDT.dot");
            try {
                graph.getDominatorInfo().getPostDominatorsTree()
                        .writeDOT(writer, "PDT", DOTUtil.RANGE_ATTRIBUTE_PROVIDER,
                                                 DOTUtil.EDGE_ATTRIBUTE_PROVIDER);
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.toString(), e);
        }
    }

    @Override
    public void treeAddressCodeBuilt(ControlFlowGraph graph) {
        try {
            Writer writer = new FileWriter(baseName + "_TAC.dot");
            try {
                DOTUtil.writeCFG(graph, null, writer, TAC);
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.toString(), e);
        }
    }

    @Override
    public void regionGraphBuilt(DirectedGraph<Region, Edge> regionGraph) {
        try {
            if (drawRegions) {
                Set<Region> rootRegions = regionGraph.getVertices();

                Writer writer = new FileWriter(baseName + "_CFG.dot");
                try {
                    DOTUtil.writeCFG(graph, rootRegions, writer, RANGE);
                } finally {
                    writer.close();
                }

                writer = new FileWriter(baseName + "_BC.dot");
                try {
                    DOTUtil.writeCFG(graph, rootRegions, writer, BYTECODE);
                } finally {
                    writer.close();
                }

                writer = new FileWriter(baseName + "_TAC.dot");
                try {
                    DOTUtil.writeCFG(graph, rootRegions, writer, TAC);
                } finally {
                    writer.close();
                }
            }

            Writer writer = new FileWriter(baseName + "_RG.dot");
            try {
                regionGraph.writeDOT(writer, "RG", DOTUtil.REGION_ATTRIBUTE_PROVIDER,
                                                   DOTUtil.EDGE_ATTRIBUTE_PROVIDER);
            } finally {
                writer.close();
            }
        } catch (IOException e) {
            logger.log(Level.SEVERE, e.toString(), e);
        }
    }
}