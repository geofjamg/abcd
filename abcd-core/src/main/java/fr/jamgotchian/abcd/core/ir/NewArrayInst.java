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
package fr.jamgotchian.abcd.core.ir;

import fr.jamgotchian.abcd.core.type.JavaType;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NewArrayInst extends DefInst {

    private final JavaType type;

    private final List<Variable> dimensions;

    NewArrayInst(int defID, Variable result, JavaType type, List<Variable> dimensions) {
        super(defID, result);
        this.type = type;
        this.dimensions = dimensions;
    }

    public JavaType getElementType() {
        return type;
    }

    public List<Variable> getDimensions() {
        return dimensions;
    }

    public int getDimensionCount() {
        return dimensions.size();
    }

    @Override
    public Set<Variable> getUses() {
        return new HashSet<>(dimensions);
    }

    @Override
    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
