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
import static fr.jamgotchian.abcd.graph.GraphvizUtil.*;
import fr.jamgotchian.abcd.core.ir.RPST.ExportType;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RPSTLogger {

    public static class Log {

        private final String name;

        private ControlFlowGraph cfg;

        private RPST rpst;

        private Log(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public ControlFlowGraph getCfg() {
            return cfg;
        }

        public void setCfg(ControlFlowGraph cfg) {
            this.cfg = cfg;
        }

        public RPST getRpst() {
            return rpst;
        }

        public void setRpst(RPST rpst) {
            this.rpst = rpst;
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
            if (cfg != null) {
                cfg.exportPane(writer, "CFG", paneId.getCountAndIncrement(), 3);
            }
            if (rpst != null) {
                rpst.exportPane(writer, ExportType.GRAPH, "RPST graph view",
                                      paneId.getCountAndIncrement(), 3);
                rpst.exportPane(writer, ExportType.TREE, "RPST control tree view",
                                      paneId.getCountAndIncrement(), 3);
            }
            writeIndent(writer, 2);
            writer.append("}\n");
        }
    }

    private final String name;

    private final List<Log> logs = new ArrayList<>();

    public RPSTLogger(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public Log newLog(String name) {
        Log log = new Log(name);
        logs.add(log);
        return log;
    }

    public List<Log> getLogs() {
        return Collections.unmodifiableList(logs);
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

        for (Log log : logs) {
            log.export(writer, paneId);
        }

        writeIndent(writer, 1);
        writer.append("}\n");

        writer.append("}\n");
    }
}

