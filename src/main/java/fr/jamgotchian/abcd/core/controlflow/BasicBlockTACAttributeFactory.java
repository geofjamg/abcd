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

import fr.jamgotchian.abcd.core.controlflow.util.TACInstWriter;
import fr.jamgotchian.abcd.core.graph.AttributeFactory;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class BasicBlockTACAttributeFactory implements AttributeFactory<BasicBlock> {

    public Map<String, String> getAttributes(BasicBlock block) {
        Map<String, String> attrs = new HashMap<String, String>(3);
        attrs.put("shape", "box");
        attrs.put("color", "black");
        StringBuilder builder = new StringBuilder();
        builder.append("< ");
        if (block.getType() == BasicBlockType.ENTRY
                || block.getType() == BasicBlockType.EXIT) {
            builder.append("<font color=\"black\">").append(block.getType()).append("</font>");
        } else {
            builder.append(TACInstWriter.toDOTHTMLLike(block.getRange(),
                    block.getInstructions(),
                    block.getInputStack(),
                    block.getOutputStack()));
        }
        builder.append(" >");
        attrs.put("label", builder.toString());
        return attrs;
    }
}
