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

import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.util.JavaCompilationUnitWriter;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.RPST;
import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.output.TextCodeWriter;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DefaultOutputHandler implements OutputHandler {

    protected final static Logger logger
            = Logger.getLogger(DefaultOutputHandler.class.getName());

    private final boolean debug;

    private final OutputStream os;

    public DefaultOutputHandler(boolean debug, OutputStream os) {
        this.debug = debug;
        this.os = os;
    }

    public void writeRawCFG(ControlFlowGraph cfg, GraphvizRenderer<BasicBlock> bbRenderer) {
    }

    public void writeCFG(ControlFlowGraph cfg) {
    }

    public void writeRPST(RPST rpst) {
    }

    public void writeAST(CompilationUnit compilUnit) {
        Writer writer = new OutputStreamWriter(new BufferedOutputStream(os));
        try {
            compilUnit.accept(new JavaCompilationUnitWriter(new TextCodeWriter(writer, 4), debug), null);
        } finally {
            try  {
                writer.close();
            } catch (IOException e) {
                logger.log(Level.SEVERE, e.toString(), e);
            }
        }
    }
}
