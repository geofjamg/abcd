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
package fr.jamgotchian.abcd.core.controlflow;

import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Region {

    private BasicBlock entry;

    private BasicBlock exit;

    private Region parent;

    private final Set<Region> children = new HashSet<Region>();

    private ParentType parentType;

    private ChildType childType;

    public Region(BasicBlock entry, BasicBlock exit, ParentType parentType) {
        this.entry = entry;
        this.exit = exit;
        this.parentType = parentType;
        childType = ChildType.UNDEFINED;
    }

    public BasicBlock getEntry() {
        return entry;
    }

    public BasicBlock getExit() {
        return exit;
    }

    public void setEntry(BasicBlock entry) {
        this.entry = entry;
    }

    public void setExit(BasicBlock exit) {
        this.exit = exit;
    }

    public Region getParent() {
        return parent;
    }

    public void setParent(Region parent) {
        this.parent = parent;
        parent.addChild(this);
    }

    public Set<Region> getChildren() {
        return children;
    }

    public int getChildCount() {
        return children.size();
    }

    public void addChild(Region child) {
        children.add(child);
    }

    public boolean hasChild() {
        return children.size() > 0;
    }

    public ParentType getParentType() {
        return parentType;
    }

    public void setParentType(ParentType parentType) {
        this.parentType = parentType;
    }

    public ChildType getChildType() {
        return childType;
    }

    public void setChildType(ChildType childType) {
        this.childType = childType;
    }

    @Override
    public String toString() {
        return childType + " " + parentType + " (" + entry + ", " + exit + ")";
    }
}
