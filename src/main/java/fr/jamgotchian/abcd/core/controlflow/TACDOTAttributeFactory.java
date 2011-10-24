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

import fr.jamgotchian.abcd.core.graph.DOTAttributeFactory;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TACDOTAttributeFactory implements DOTAttributeFactory<BasicBlock> {

    public static final TACDOTAttributeFactory INSTANCE = new TACDOTAttributeFactory();

    public Map<String, String> getAttributes(BasicBlock bb) {
        Map<String, String> attrs = new HashMap<String, String>(3);
        attrs.put("shape", "box");
        attrs.put("color", "black");
        StringBuilder builder = new StringBuilder();
        builder.append("< ");
        if (bb.getType() == BasicBlockType.ENTRY
                || bb.getType() == BasicBlockType.EXIT) {
            builder.append("<font color=\"black\">").append(bb.getType()).append("</font>");
        } else {
            builder.append(TACInstWriter.toDOTHTMLLike(bb.getRange(),
                    bb.getInstructions(),
                    bb.getInputStack(),
                    bb.getOutputStack()));
        }
        builder.append(" >");
        attrs.put("label", builder.toString());
        return attrs;
    }
}
