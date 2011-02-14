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

import fr.jamgotchian.abcd.core.util.Collections3;
import fr.jamgotchian.abcd.core.util.Sets;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
class DominatorsFinder extends ForwardDataFlowAnalysis<Set<BasicBlock>>  {

    DominatorsFinder(ControlFlowGraph graph) {
        super(graph);
    }

    @Override
    public Set<BasicBlock> getInitValue(BasicBlock block, boolean isStartBlock) {
        if (isStartBlock) {
            return new HashSet<BasicBlock>(Arrays.asList(block));
        } else {
            return new HashSet<BasicBlock>(getGraph().getBasicBlocks());
        }
    }

    @Override
    public Set<BasicBlock> combineValues(Set<BasicBlock> value1, Set<BasicBlock> value2) {
        return Sets.intersection(value1 == null ? Collections.<BasicBlock>emptySet() : value1, 
                                 value2 == null ? Collections.<BasicBlock>emptySet() : value2);
    }

    @Override
    public Set<BasicBlock> applyTranferFunction(BasicBlock block, Set<BasicBlock> inValue) {
        Set<BasicBlock> outValue = new HashSet<BasicBlock>();
        outValue.add(block);
        if (inValue != null) {
            outValue.addAll(inValue);
        }
        return outValue;
    }

    @Override
    public boolean valuesEqual(Set<BasicBlock> value1, Set<BasicBlock> value2) {
        return Collections3.sameContent(value1, value2);
    }    
}
