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

import java.util.Collections;
import java.util.Set;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ForkJoinInfo {

    public static class Branch {
        
        private final Edge forkEdge;
        
        private final Edge joinEdge;

        public Branch(Edge forkEdge, Edge joinEdge) {
            this.forkEdge = forkEdge;
            this.joinEdge = joinEdge;
        }

        public Edge getForkEdge() {
            return forkEdge;
        }

        public Edge getJoinEdge() {
            return joinEdge;
        }
    }
    
    private final BasicBlock forkBlock;
    
    private final BasicBlock joinBlock;
    
    private final Set<Branch> branches;

    public ForkJoinInfo(BasicBlock forkBlock, BasicBlock joinBlock, Set<Branch> branches) {
        this.forkBlock = forkBlock;
        this.joinBlock = joinBlock;
        this.branches = Collections.unmodifiableSet(branches);
    }

    public BasicBlock getForkBlock() {
        return forkBlock;
    }

    public BasicBlock getJoinBlock() {
        return joinBlock;
    }

    public Set<Branch> getBranches() {
        return branches;
    }
}
