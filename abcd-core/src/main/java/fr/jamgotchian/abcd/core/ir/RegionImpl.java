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
package fr.jamgotchian.abcd.core.ir;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class RegionImpl implements Region {

    private final BasicBlock entry;

    private final BasicBlock exit;

    private ParentType parentType;

    private ChildType childType;

    private Object data;

    public RegionImpl(BasicBlock entry, BasicBlock exit,ParentType parentType) {
        this.entry = entry;
        this.exit = exit;
        this.parentType = parentType;
        this.childType = ChildType.UNDEFINED;
    }

    @Override
    public BasicBlock getEntry() {
        return entry;
    }

    @Override
    public BasicBlock getExit() {
        return exit;
    }

    @Override
    public ParentType getParentType() {
        return parentType;
    }

    @Override
    public void setParentType(ParentType parentType) {
        this.parentType = parentType;
    }

    @Override
    public ChildType getChildType() {
        return childType;
    }

    @Override
    public void setChildType(ChildType childType) {
        this.childType = childType;
    }

    @Override
    public boolean isBasicBlock() {
        return parentType == ParentType.BASIC_BLOCK;
    }

    @Override
    public Object getData() {
        return data;
    }

    @Override
    public void setData(Object data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return "(" + entry + ", " + exit + ")";
    }
}
