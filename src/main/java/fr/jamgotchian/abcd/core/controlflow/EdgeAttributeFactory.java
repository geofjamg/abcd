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

import fr.jamgotchian.abcd.core.graph.AttributeFactory;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class EdgeAttributeFactory implements AttributeFactory<Edge> {

    private final boolean useConstraints;

    public EdgeAttributeFactory(boolean useConstraints) {
        this.useConstraints = useConstraints;
    }

    public EdgeAttributeFactory() {
        this(true);
    }

    public Map<String, String> getAttributes(Edge edge) {
        Map<String, String> attrs = new HashMap<String, String>(3);
        if (edge.hasAttribute(EdgeAttribute.FAKE_EDGE)) {
            attrs.put("color", "yellow");
        } else if (edge.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
            attrs.put("color", "red");
        } else if (edge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            attrs.put("color", "green");
        } else if (edge.hasAttribute(EdgeAttribute.BREAK_LABEL_EDGE)) {
            attrs.put("color", "purple");
        } else if (edge.hasAttribute(EdgeAttribute.RETURN_EDGE)) {
            attrs.put("color", "orange");
        } else {
            attrs.put("color", "black");
        }
        if (edge.isExceptional()) {
            attrs.put("style", "dotted");
        }
        if (edge.getValue() != null) {
            attrs.put("label", "\"" + edge.getValue() + "\"");
        }
        if (useConstraints &&
                edge != null &&
                edge.getCategory() != EdgeCategory.ADVANCING) {
            attrs.put("constraint", "false");
        }
        return attrs;
    }
}
