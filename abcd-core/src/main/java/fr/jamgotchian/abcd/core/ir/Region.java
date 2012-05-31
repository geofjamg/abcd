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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Region {

    private BasicBlock entry;

    private BasicBlock exit;

    private Region parent;

    private final Set<Region> children = new LinkedHashSet<Region>();

    private ParentType parentType;

    private ChildType childType;

    private Object data;

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
        if (this.parent != null) {
            this.parent.children.remove(this);
        }
        this.parent = parent;
        if (parent != null) {
            parent.addChild(this);
        }
    }

    public void insertParent(Region parent) {
        Region oldParent = this.parent;
        setParent(parent);
        if (oldParent != null) {
            parent.setParent(oldParent);
            parent.setChildType(childType);
            childType = ChildType.UNDEFINED;
        }
    }

    public Set<Region> getChildren() {
        return children;
    }

    public Region getEntryChild() {
        for (Region child : children) {
            if (child.getEntry().equals(entry)) {
                return child;
            }
        }
        throw new IllegalStateException("Should not happen");
    }

    public Region getFirstChild() {
        if (children.isEmpty()) {
            return null;
        } else {
            return children.iterator().next();
        }
    }

    public Region getSecondChild() {
        if (children.size() < 2) {
            return null;
        } else {
            Iterator<Region> it = children.iterator();
            it.next();
            return it.next();
        }
    }

    public Region getFirstChild(ChildType childType) {
        for (Region child : children) {
            if (child.getChildType() == childType) {
                return child;
            }
        }
        return null;
    }

    public Collection<Region> getChildren(ChildType childType) {
        List<Region> children2 = new ArrayList<Region>();
        for (Region child : children) {
            if (child.getChildType() == childType) {
                children2.add(child);
            }
        }
        return children2;
    }

    public int getChildCount() {
        return children.size();
    }

    private void addChild(Region child) {
        children.add(child);
    }

    public void removeChildren() {
        for (Region child : new ArrayList<Region>(children)) {
            child.setParent(null);
        }
    }

    public boolean hasChild() {
        return children.size() > 0;
    }

    private void getSubRegion(Set<Region> regions) {
        regions.addAll(children);
        for (Region child : children) {
            child.getSubRegion(regions);
        }
    }

    public Set<Region> getSubRegions() {
        Set<Region> regions = new HashSet<Region>();
        getSubRegion(regions);
        return regions;
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

    public boolean isBasicBlock() {
        return parentType == ParentType.BASIC_BLOCK;
    }

    public Set<BasicBlock> getBasicBlocks() {
        Set<BasicBlock> bbs = new LinkedHashSet<BasicBlock>();
        addBasicBlocks(bbs);
        return bbs;
    }

    private void addBasicBlocks(Set<BasicBlock> bbs) {
        if (entry != null) {
            bbs.add(entry);
        }
        for (Region child : children) {
            child.addBasicBlocks(bbs);
        }
    }

    public Object getData() {
        return data;
    }

    public void setData(Object data) {
        this.data = data;
    }

    public boolean deepEquals(Region other) {
        return deepEquals(other, new VariableMapping());
    }

    private boolean deepEquals(Region other, VariableMapping mapping) {
        if (parentType == other.getParentType()
                && childType == other.getChildType()) {
            if (childType == ChildType.CATCH || childType == ChildType.FINALLY) {
                ExceptionHandlerInfo info = (ExceptionHandlerInfo) entry.getProperty(BasicBlockPropertyName.EXCEPTION_HANDLER_ENTRY);
                ExceptionHandlerInfo otherInfo = (ExceptionHandlerInfo) other.getEntry().getProperty(BasicBlockPropertyName.EXCEPTION_HANDLER_ENTRY);
                mapping.defEqual(info.getVariable(), otherInfo.getVariable());
            }
            if (isBasicBlock()) {
                BasicBlock bb = entry;
                BasicBlock otherBb = other.getEntry();
                return IRInstComparator.equal(bb.getInstructions(),
                                              otherBb.getInstructions(),
                                              mapping);
            } else {
                if (getChildCount() == other.getChildCount()) {
                    for (Region child : children) {
                        boolean found = false;
                        for (Region otherChild : other.getChildren()) {
                            if (child.deepEquals(otherChild, mapping)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            return false;
                        }
                    }
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return "(" + entry + ", " + exit + ")";
    }
}
