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

package fr.jamgotchian.abcd.core.ast;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CompilationUnit {

    private final Package _package;

    private final ImportManager _import;

    private final List<Class> classes;

    public CompilationUnit(Package _package, ImportManager _import) {
        this._package = _package;
        this._import = _import;
        classes = new ArrayList<>();
    }

    public Package getPackage() {
        return _package;
    }

    public ImportManager getImport() {
        return _import;
    }

    public List<Class> getClasses() {
        return classes;
    }

    public <R, A> R accept(CompilationUnitVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }

    public String getFilePath() {
        return _package.getName().replace('.', File.separatorChar)
                + File.separator + classes.get(0).getName().getSimpleName() + ".java";
    }
}
