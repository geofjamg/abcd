/*
 * Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
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

package fr.jamgotchian.abcd.core.ast.util;

import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.CompilationUnitVisitor;
import fr.jamgotchian.abcd.core.ast.Package;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.code.CodeWriter;
import fr.jamgotchian.abcd.core.common.ABCDUtil;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class JavaCompilationUnitWriter implements CompilationUnitVisitor<Void, Void> {

    private final CodeWriter writer;

    private final JavaClassWriter classVisitor;

    public JavaCompilationUnitWriter(CodeWriter writer) {
        this.writer = writer;
        classVisitor = new JavaClassWriter(writer);
    }

    public Void visit(CompilationUnit compilUnit, Void arg) {

        // write ABCD banner
        writer.write("/**").newLine()
                .writeSpace().write("*").writeSpace().write("Decompiled by ABCD v")
                .write(ABCDUtil.VERSION).newLine()
                .writeSpace().write("*").writeSpace().write("ABCD home page : ")
                .write(ABCDUtil.HOME_PAGE).newLine()
                .writeSpace().write("*/").newLine().newLine();

        if (compilUnit.getPackage() != null) {
            compilUnit.getPackage().accept(this, null);
        }
        if (compilUnit.getImport().getImports().size() > 0) {
            writer.newLine();
            compilUnit.getImport().accept(this, null);
        }
        writer.newLine();
        for (Class _class : compilUnit.getClasses()) {
            _class.accept(classVisitor, null);
            writer.newLine();
        }
        return null;
    }

    public Void visit(Package _package, Void arg) {
        writer.writeKeyword("package").writeSpace().write(_package.getName()).write(";").newLine();
        return null;
    }

    public Void visit(ImportManager importManager, Void arg) {
        for (String className : importManager.getImports()) {
            writer.writeKeyword("import").writeSpace().write(className)
                    .write(";").newLine();
        }
        return null;
    }

}
