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
package fr.jamgotchian.abcd.core.analysis;

import fr.jamgotchian.abcd.core.controlflow.BasicBlock;
import fr.jamgotchian.abcd.core.controlflow.BasicBlockType;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class AbruptEdgeRemover {

    private static final Logger logger
            = Logger.getLogger(AbruptEdgeRemover.class.getName());

    private final ControlFlowGraph CFG;

    public AbruptEdgeRemover(ControlFlowGraph CFG) {
        this.CFG = CFG;
    }

    public void remove() {
        for (BasicBlock bb : CFG.getBasicBlocks()) {
            if (bb.getType() == BasicBlockType.JUMP_IF) {
                Set<BasicBlock> frontier
                        = CFG.getDominatorInfo().getDominanceFrontierOf2(bb);
                if (frontier.size() > 1) {
                    logger.log(Level.FINER, "Abrupt CFG {0} : {1}",
                            new Object[] {CFG.getName(), bb});
                }
            }
        }
    }
}
