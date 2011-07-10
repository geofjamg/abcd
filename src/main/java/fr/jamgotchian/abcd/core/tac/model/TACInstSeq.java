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
package fr.jamgotchian.abcd.core.tac.model;

import com.google.common.base.Predicate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TACInstSeq implements Iterable<TACInst> {

    private final List<TACInst> insts;

    public TACInstSeq() {
        this(new ArrayList<TACInst>());
    }

    public TACInstSeq(List<TACInst> insts) {
        this.insts = insts;
    }

    public TACInst get(int index) {
        return insts.get(index);
    }

    public void add(TACInst inst) {
        insts.add(inst);
    }

    public void addAll(int index, Collection<TACInst> insts) {
        this.insts.addAll(index, insts);
    }

    public TACInst remove(int index) {
        return insts.remove(index);
    }

    public boolean removeIf(Predicate<TACInst> predicate) {
        boolean done = false;
        for (Iterator<TACInst> it = insts.iterator(); it.hasNext();) {
            TACInst inst = it.next();
            if (predicate.apply(inst)) {
                it.remove();
                done = true;
            }
        }
        return done;
    }

    public void insertAt(int index, TACInst inst) {
        insts.add(index, inst);
    }

    public int size() {
        return insts.size();
    }

    public boolean isEmpty() {
        return insts.isEmpty();
    }

    public TACInst getLast() {
        if (insts.isEmpty()) {
            return null;
        } else {
            return insts.get(insts.size()-1);
        }
    }

    public Iterator<TACInst> iterator() {
        return insts.iterator();
    }

    public <R, A> R accept(TACInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
