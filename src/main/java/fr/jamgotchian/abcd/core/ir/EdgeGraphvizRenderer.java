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

import fr.jamgotchian.abcd.core.graph.GraphvizRenderer;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class EdgeGraphvizRenderer implements GraphvizRenderer<Edge> {

    private final boolean useConstraints;

    public EdgeGraphvizRenderer(boolean useConstraints) {
        this.useConstraints = useConstraints;
    }

    public EdgeGraphvizRenderer() {
        this(true);
    }

    public Map<String, String> getAttributes(Edge edge) {
        Map<String, String> attrs = new HashMap<String, String>(3);
        if (edge.hasAttribute(EdgeAttribute.FAKE_EDGE)) {
            attrs.put("color", "gray");
        } else if (edge.hasAttribute(EdgeAttribute.LOOP_BACK_EDGE)) {
            attrs.put("color", "red");
        } else if (edge.hasAttribute(EdgeAttribute.LOOP_EXIT_EDGE)) {
            attrs.put("color", "green");
        } else {
            attrs.put("color", "black");
        }
        if (edge.hasAttribute(EdgeAttribute.EXCEPTIONAL_EDGE)) {
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
