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
package fr.jamgotchian.abcd.core;

import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.util.JavaCompilationUnitWriter;
import fr.jamgotchian.abcd.core.code.CodeWriter;
import fr.jamgotchian.abcd.core.code.TextCodeWriter;
import fr.jamgotchian.abcd.core.common.Configuration;
import fr.jamgotchian.abcd.core.common.DecompilationObserver;
import fr.jamgotchian.abcd.core.ir.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ir.RPSTLogger;
import java.io.Writer;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TestDecompilationObserver implements DecompilationObserver {

    private final Writer writer;
    
    private final Configuration config;

    public TestDecompilationObserver(Writer writer, Configuration config) {
        this.writer = writer;
        this.config = config;
    }
    
    @Override
    public void doneRawCFG(ControlFlowGraph cfg) {
    }

    @Override
    public void doneCFG(ControlFlowGraph cfg, boolean failure) {
    }

    @Override
    public void doneRPST(RPSTLogger logger) {
    }

    @Override
    public void doneAST(CompilationUnit compilUnit) {
        CodeWriter codeWriter = new TextCodeWriter(writer, 4);
        compilUnit.accept(new JavaCompilationUnitWriter(codeWriter, config), null);
    }
    
}
