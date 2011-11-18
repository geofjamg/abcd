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

import fr.jamgotchian.abcd.core.common.ABCDWriter;
import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.util.JavaCompilationUnitWriter;
import fr.jamgotchian.abcd.core.ir.BasicBlock;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.RPST;
import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import fr.jamgotchian.abcd.core.code.TextCodeWriter;
import fr.jamgotchian.abcd.core.common.ABCDException;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DefaultABCDWriter implements ABCDWriter {

    protected final static Logger logger
            = Logger.getLogger(DefaultABCDWriter.class.getName());

    private final boolean debug;

    private final File outDir;

    public DefaultABCDWriter(boolean debug, File outDir) {
        this.debug = debug;
        this.outDir = outDir;
        if (!outDir.exists()) {
            throw new ABCDException(outDir + " does not exist");
        }
        if (!outDir.isDirectory()) {
            throw new ABCDException(outDir + " is not a directory");
        }
    }

    public void writeRawCFG(ControlFlowGraph cfg, GraphvizRenderer<BasicBlock> bytecodeRenderer) {
    }

    public void writeCFG(ControlFlowGraph cfg) {
    }

    public void writeRPST(RPST rpst, int level) {
    }

    public void writeAST(CompilationUnit compilUnit) {
        assert compilUnit != null;
        try {
            File packageDir = new File(outDir.getAbsolutePath() + File.separator
                    + compilUnit.getPackage().getName().replace('.', File.separatorChar));
            packageDir.mkdirs();
            File srcDir = new File(packageDir.getAbsolutePath() + File.separator
                    + compilUnit.getClasses().get(0).getName() + ".java");
            FileOutputStream os = new FileOutputStream(srcDir);
            Writer writer = new OutputStreamWriter(new BufferedOutputStream(os));
            try {
                compilUnit.accept(new JavaCompilationUnitWriter(new TextCodeWriter(writer, 4), debug), null);
            } finally {
                writer.close();
            }
        } catch (Exception e) {
            logger.log(Level.SEVERE, e.toString(), e);
        }
    }
}
