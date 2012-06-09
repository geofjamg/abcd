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

import fr.jamgotchian.abcd.core.util.Counter;
import static fr.jamgotchian.abcd.core.graph.GraphvizUtil.*;
import fr.jamgotchian.abcd.core.ir.RPST.ExportType;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPSTFlightRecorder {

    public static class Record {

        private final String name;

        private ControlFlowGraph abruptCfg;

        private ControlFlowGraph prunedCfg;

        private RPST prunedRpst;

        private ControlFlowGraph smoothCfg;

        private RPST smoothRpst;

        public Record(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public ControlFlowGraph getAbruptCfg() {
            return abruptCfg;
        }

        public void setAbruptCfg(ControlFlowGraph abruptCfg) {
            this.abruptCfg = abruptCfg;
        }

        public ControlFlowGraph getPrunedCfg() {
            return prunedCfg;
        }

        public void setPrunedCfg(ControlFlowGraph prunedCfg) {
            this.prunedCfg = prunedCfg;
        }

        public RPST getPrunedRpst() {
            return prunedRpst;
        }

        public void setPrunedRpst(RPST prunedRpst) {
            this.prunedRpst = prunedRpst;
        }

        public ControlFlowGraph getSmoothCfg() {
            return smoothCfg;
        }

        public void setSmoothCfg(ControlFlowGraph smoothCfg) {
            this.smoothCfg = smoothCfg;
        }

        public RPST getSmoothRpst() {
            return smoothRpst;
        }

        public void setSmoothRpst(RPST smoothRpst) {
            this.smoothRpst = smoothRpst;
        }

        public void export(Writer writer, Counter paneId) throws IOException {
            writeIndent(writer, 2);
            writer.append("subgraph cluster_title_")
                    .append(Integer.toString(paneId.getCountAndIncrement())).append(" {\n");
            writeIndent(writer, 3);
            writer.append("fontsize=\"18\";\n");
            writeIndent(writer, 3);
            writer.append("labeljust=\"left\";\n");
            writeIndent(writer, 3);
            writer.append("label=\"").append(name).append("\";\n");
            if (abruptCfg != null) {
                abruptCfg.exportPane(writer, "Abrupt CFG", paneId.getCountAndIncrement(), 3);
            }
            if (prunedCfg != null) {
                prunedCfg.exportPane(writer, "Pruned CFG", paneId.getCountAndIncrement(), 3);
            }
            if (prunedRpst != null) {
                prunedRpst.exportPane(writer, ExportType.GRAPH, "Pruned RPST graph view",
                                      paneId.getCountAndIncrement(), 3);
                prunedRpst.exportPane(writer, ExportType.TREE, "Pruned RPST control tree view",
                                      paneId.getCountAndIncrement(), 3);
            }
            if (smoothCfg != null) {
                smoothCfg.exportPane(writer, "Smooth CFG", paneId.getCountAndIncrement(), 3);
            }
            if (smoothRpst != null) {
                smoothRpst.exportPane(writer, ExportType.GRAPH, "Smooth RPST graph view",
                                      paneId.getCountAndIncrement(), 3);
                smoothRpst.exportPane(writer, ExportType.TREE, "Smooth RPST control tree view",
                                      paneId.getCountAndIncrement(), 3);
            }
            writeIndent(writer, 2);
            writer.append("}\n");
        }
    }

    private final String name;

    private final List<Record> records = new ArrayList<Record>();

    public RPSTFlightRecorder(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public List<Record> getRecords() {
        return records;
    }

    public void export(Writer writer) throws IOException {
        writer.append("digraph ").append("RPST").append(" {\n");

        Counter paneId = new Counter(0);

        writeIndent(writer, 1);
        writer.append("subgraph cluster_title_")
                .append(Integer.toString(paneId.getCountAndIncrement())).append(" {\n");
        writeIndent(writer, 2);
        writer.append("fontsize=\"18\";\n");
        writeIndent(writer, 2);
        writer.append("labeljust=\"left\";\n");
        writeIndent(writer, 2);
        writer.append("label=\"").append(name).append("\";\n");

        for (Record record : records) {
            record.export(writer, paneId);
        }

        writeIndent(writer, 1);
        writer.append("}\n");

        writer.append("}\n");
    }
}

