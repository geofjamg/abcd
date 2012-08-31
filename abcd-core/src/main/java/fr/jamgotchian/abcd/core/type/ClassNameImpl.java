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
package fr.jamgotchian.abcd.core.type;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ClassNameImpl implements ClassName {

    private final String qualifiedName;

    private final String packageName;

    private final String simpleName;

    private final ImportStrategy importStrategy;

    public ClassNameImpl(String qualifiedName, ImportStrategy importStrategy) {
        this.qualifiedName = qualifiedName;
        this.importStrategy = importStrategy;
        int lastDotIndex = qualifiedName.lastIndexOf('.');
        if (lastDotIndex == -1)  {
            packageName = null;
            simpleName = qualifiedName;
        } else {
            packageName = qualifiedName.substring(0, lastDotIndex);
            simpleName = qualifiedName.substring(lastDotIndex+1);
        }
    }

    @Override
    public String getPackageName() {
        return packageName;
    }

    @Override
    public String getSimpleName() {
        return simpleName;
    }

    @Override
    public String getQualifiedName() {
        return qualifiedName;
    }

    @Override
    public String getCompilationUnitName() {
        if (importStrategy != null) {
            return importStrategy.getCompilationUnitName(this);
        } else {
            return qualifiedName;
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ClassNameImpl)) {
            return false;
        }
        return ((ClassNameImpl) obj).qualifiedName.equals(qualifiedName);
    }

    @Override
    public int hashCode() {
        return qualifiedName.hashCode();
    }

    @Override
    public String toString() {
        return getCompilationUnitName();
    }
}

