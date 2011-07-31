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

package fr.jamgotchian.abcd.core.controlflow;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class EdgeImpl implements Edge {

    private EdgeCategory category;

    private final boolean exceptional;

    private Object value;

    private boolean loopExit;

    private boolean selfLoop; 
    
    public EdgeImpl() {
        this(false);
    }
    
    public EdgeImpl(boolean exceptional) {
        this(exceptional, null, false);
    }

    public EdgeImpl(Object value, boolean loopExit) {
        this(false, value, loopExit);
    }
    
    public EdgeImpl(boolean exceptional, Object value, boolean loopExit) {
        this.exceptional = exceptional;
        this.value = value;
        this.loopExit = loopExit;
        selfLoop = false;
    }
    
    private EdgeImpl(EdgeImpl other) {
        category = other.category;
        exceptional = other.exceptional;
        value = other.value;
        loopExit = other.loopExit;
    }

    public EdgeCategory getCategory() {
        return category;
    }

    public boolean isLoopBack() {
        return selfLoop || (category != null && category == EdgeCategory.BACK);
    }
    
    public void setCategory(EdgeCategory category) {
        this.category = category;
    }

    public boolean isExceptional() {
        return exceptional;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public boolean isLoopExit() {
        return loopExit;
    }

    public void setLoopExit(boolean loopExit) {
        this.loopExit = loopExit;
    }

    public boolean isSelfLoop() {
        return selfLoop;
    }

    public void setSelfLoop(boolean selfLoop) {
        this.selfLoop = selfLoop;
    }

    public void resetState() {
        category = null;
        loopExit = false;
        selfLoop = false;
    }
    
    @Override
    public Edge clone() {
        return new EdgeImpl(this);
    }

    @Override
    public String toString() {
        return  "Edge[value=" + value + ", category=" + category + 
                ", isLoopExit=" + loopExit + ", isExceptional=" + exceptional + "]";
    }
}
