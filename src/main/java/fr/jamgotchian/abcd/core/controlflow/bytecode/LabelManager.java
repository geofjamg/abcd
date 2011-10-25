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

package fr.jamgotchian.abcd.core.controlflow.bytecode;

import fr.jamgotchian.abcd.core.common.Label;
import java.util.IdentityHashMap;
import java.util.Map;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.JumpInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.LabelNode;
import static org.objectweb.asm.tree.AbstractInsnNode.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class LabelManager {

    private final Map<LabelNode, Label> labels = new IdentityHashMap<LabelNode, Label>();

    public LabelManager() {
    }

    public LabelManager(InsnList instructions) {
        for (int i = 0; i < instructions.size(); i++) {
            AbstractInsnNode abstractNode = instructions.get(i);

            switch (abstractNode.getType()) {
                case JUMP_INSN: {
                    JumpInsnNode node = (JumpInsnNode) abstractNode;
                    getLabel(node.label);
                    break;
                }

                case LABEL: {
                    LabelNode node = (LabelNode) abstractNode;
                    getLabel(node);
                    break;
                }
            }
        }
    }

    final public Label getLabel(LabelNode node) {
        Label label = labels.get(node);
        if (label == null) {
            label = new Label(labels.size());
            labels.put(node, label);
        }
        return label;
    }

    public void clear() {
        labels.clear();
    }
}
