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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BasicBlockSplit {

    private final BasicBlock blockBefore;

    private final BasicBlock blockAfter;

    private Edge edge;

    BasicBlockSplit(BasicBlock blockBefore, BasicBlock blockAfter, Edge edge) {
        this.blockBefore = blockBefore;
        this.blockAfter = blockAfter;
        this.edge = edge;
    }

    public BasicBlock getBlockBefore() {
        return blockBefore;
    }

    public BasicBlock getBlockAfter() {
        return blockAfter;
    }

    public Edge getEdge() {
        return edge;
    }

    void setEdge(Edge edge) {
        this.edge = edge;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + blockBefore + ", " + blockAfter + ", " + edge + ")";
    }
}
