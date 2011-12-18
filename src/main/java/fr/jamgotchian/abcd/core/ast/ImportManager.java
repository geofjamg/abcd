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
package fr.jamgotchian.abcd.core.ast;

import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.ClassNameManager;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ImportManager implements ClassNameManager {

    private final List<ClassName> classNames = new ArrayList<ClassName>();

    public ImportManager() {
    }

    @Override
    public ClassName newClassName(String className) {
        ClassName cn = new ImportableClassName(className, this);
        classNames.add(cn);
        return cn;
    }

    @Override
    public Collection<ClassName> getAllClassNames() {
        return classNames;
    }

    public boolean isImported(ClassName className) {
        String packageName = className.getPackageName();
        return packageName != null
                && packageName.startsWith("java.lang");
    }

    public Set<String> getImports() {
        // TODO
        return Collections.emptySet();
    }

    public <R, A> R accept(CompilationUnitVisitor<R, A> visitor, A arg) {
      return visitor.visit(this, arg);
    }
}
