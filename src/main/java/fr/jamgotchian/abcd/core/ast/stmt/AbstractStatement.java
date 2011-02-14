/*
 *  Copyright (C) 2010 Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.jamgotchian.abcd.core.ast.stmt;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public abstract class AbstractStatement implements Statement {

    private BlockStatement block;

    private Statement previous;

    private Statement next;

    public AbstractStatement() {
    }

    public void setBlock(BlockStatement block) {
        this.block = block;
    }

    public BlockStatement getBlock() {
        return block;
    }

    public Statement getNext() {
        return next;
    }

    public void setNext(Statement next) {
        this.next = next;
    }

    public Statement getPrevious() {
        return previous;
    }

    public void setPrevious(Statement previous) {
        this.previous = previous;
    }

    public void remove() {
        if (block != null) {
            block.remove(this);
        }
    }

    public void insertBefore(Statement stmt) {
        if (block != null) {
            block.insertBefore(this, stmt);
        }
    }

    public void insertAfter(Statement stmt) {
        if (block != null) {
            block.insertAfter(this, stmt);
        }
    }

    public boolean isFirst() {
        assert block != null;
        return block.getFirst() == this;
    }

    public boolean isLast() {
        assert block != null;
        return block.getLast() == this;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName();
    }
}
