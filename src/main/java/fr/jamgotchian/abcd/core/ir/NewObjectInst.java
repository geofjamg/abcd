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

import fr.jamgotchian.abcd.core.type.ClassName;
import java.util.Collections;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class NewObjectInst implements IRInst {
    
    private final TemporaryVariable result;
    
    private final ClassName className;
    
    private List<Variable> args;

    public NewObjectInst(TemporaryVariable result, ClassName className) {
        this.result = result;
        this.className = className;
        this.args = Collections.emptyList();
    }

    public TemporaryVariable getResult() {
        return result;
    }

    public ClassName getClassName() {
        return className;
    }

    public List<Variable> getArgs() {
        return args;
    }

    public void setParams(List<Variable> params) {
        this.args = params;
    }
    
    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
