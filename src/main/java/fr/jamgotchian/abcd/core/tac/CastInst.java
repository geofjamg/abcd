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
package fr.jamgotchian.abcd.core.tac;

import fr.jamgotchian.abcd.core.type.JavaType;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CastInst implements TACInst {

    private final TemporaryVariable result;

    private final TemporaryVariable var;

    private final JavaType type;

    public CastInst(TemporaryVariable result, TemporaryVariable var, JavaType type) {
        this.result = result;
        this.var = var;
        this.type = type;
    }

    public TemporaryVariable getResult() {
        return result;
    }

    public TemporaryVariable getVar() {
        return var;
    }

    public JavaType getType() {
        return type;
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
