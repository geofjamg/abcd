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

import com.google.common.base.Predicate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class IRInstSeq implements Iterable<IRInst> {

    private static final Logger LOGGER = LoggerFactory.getLogger(IRInstSeq.class);

    private final List<IRInst> insts;

    public IRInstSeq() {
        this(new ArrayList<IRInst>());
    }

    public IRInstSeq(List<IRInst> insts) {
        this.insts = insts;
    }

    public IRInst get(int index) {
        return insts.get(index);
    }

    public void add(IRInst inst) {
        LOGGER.debug("Add inst : {}", IRInstWriter.toText(inst));
        insts.add(inst);
    }

    public void addAll(int index, Collection<IRInst> insts) {
        this.insts.addAll(index, insts);
    }

    public void addAll(IRInstSeq other) {
        this.insts.addAll(other.insts);
    }

    public IRInst remove(int index) {
        return insts.remove(index);
    }

    public IRInst removeLast() {
        if (insts.isEmpty()) {
            return null;
        } else {
            return insts.remove(insts.size()-1);
        }
    }

    public void clear() {
        insts.clear();
    }

    public boolean removeIf(Predicate<IRInst> predicate) {
        boolean done = false;
        for (Iterator<IRInst> it = insts.iterator(); it.hasNext();) {
            IRInst inst = it.next();
            if (predicate.apply(inst)) {
                it.remove();
                done = true;
            }
        }
        return done;
    }

    public IRInstSeq copyIf(Predicate<IRInst> predicate) {
        List<IRInst> copy = new ArrayList<IRInst>(insts.size());
        for (IRInst inst : insts) {
            if (predicate.apply(inst)) {
                copy.add(inst);
            }
        }
        return new IRInstSeq(copy);
    }

    public void insertAt(int index, IRInst inst) {
        insts.add(index, inst);
    }

    public int size() {
        return insts.size();
    }

    public boolean isEmpty() {
        return insts.isEmpty();
    }

    public IRInst getLast() {
        if (insts.isEmpty()) {
            return null;
        } else {
            return insts.get(insts.size()-1);
        }
    }

    public Iterator<IRInst> iterator() {
        return insts.iterator();
    }

    public <R, A> R accept(IRInstVisitor<R, A> visitor, A arg) {
        return visitor.visit(this, arg);
    }
}
