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

import fr.jamgotchian.abcd.core.graph.DominatorInfo;
import fr.jamgotchian.abcd.core.graph.Tree;
import fr.jamgotchian.abcd.core.util.Collections3;
import java.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LoopBodyExpander {

    private static final Logger LOGGER = LoggerFactory.getLogger(LoopBodyExpander.class);

    private final ControlFlowGraph cfg;

    public LoopBodyExpander(ControlFlowGraph cfg) {
        this.cfg = cfg;
    }

    private boolean isCompleteExit(BasicBlock bb) {
        if (cfg.getSuccessorCountOf(bb) == 0) {
            return false;
        }
        for (Edge e : cfg.getIncomingEdgesOf(bb)) {
            if (!e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                return false;
            }
        }
        return true;
    }

    public void expand() {
        DominatorInfo<BasicBlock, Edge> domInfo = cfg.getDominatorInfo();
        Tree<BasicBlock, Edge> domTree = domInfo.getDominatorsTree();
        for (NaturalLoop nl : cfg.getNaturalLoops().values()) {
            Collection<BasicBlock> exits = nl.getExitBlocks();
            if (exits.size() <= 1) {
                continue;
            }
            LOGGER.debug("{} has {} exits : {}",
                    new Object[] {nl, exits.size(), exits});
            List<BasicBlock> incompleteExit = new ArrayList<BasicBlock>();
            List<BasicBlock> completeExit = new ArrayList<BasicBlock>();
            for (BasicBlock exit : exits) {
                if (isCompleteExit(exit)) {
                    completeExit.add(exit);
                } else {
                    incompleteExit.add(exit);
                }
            }
            for (BasicBlock bb : incompleteExit) {
                for (Edge e : cfg.getIncomingEdgesOf(bb)) {
                    if (e.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
                        continue;
                    }
                    for (BasicBlock bb2 : completeExit) {
                        Set<Edge> frontier = domInfo.getDominanceFrontierOf(bb2);
                        if (Collections3.equals(frontier, Collections.singleton(e))) {
                            Set<BasicBlock> bodyExt = domTree.getSubTree(bb2).getNodes();
                            LOGGER.debug(">>> Expand {} by adding {}", nl, bodyExt);
                            // expand the loop body
                            nl.getBody().addAll(bodyExt);
                            for (Edge e2 : cfg.getIncomingEdgesOf(bb2)) {
                                e2.removeAttribute(EdgeAttribute.LOOP_EXIT_EDGE);
                            }
                            e.addAttribute(EdgeAttribute.LOOP_EXIT_EDGE);
                        }
                    }
                }
            }
            Collection<BasicBlock> newExits = nl.getExitBlocks();
            if (newExits.size() < exits.size()) {
                LOGGER.debug("{} has now {} exits : {}",
                        new Object[] {nl, newExits.size(), newExits});
            }
            if (newExits.size() > 1) {
                LOGGER.warn("{} has multiple exits!!!", nl);
            }
        }
    }
}
