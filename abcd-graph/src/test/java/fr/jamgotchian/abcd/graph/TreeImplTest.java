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

package fr.jamgotchian.abcd.graph;

import com.google.common.collect.ImmutableSet;
import java.util.Arrays;
import java.util.Collections;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class TreeImplTest {

    public TreeImplTest() {
    }

    @Test
    public void testInit() {
        Vertex n1 = new Vertex("n", 1);
        Tree<Vertex, String> tree = new TreeImpl<>(n1);
        assertTrue(ImmutableSet.of(n1).equals(tree.getNodes()));
    }

    @Test
    public void testAddNode() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        assertTrue(ImmutableSet.of(n1, n2, n3, n4).equals(tree.getNodes()));
        assertTrue(tree.getParent(n1) == null);
        assertTrue(ImmutableSet.of(n2).equals(tree.getChildren(n1)));
        assertTrue(tree.getParent(n2).equals(n1));
        assertTrue(ImmutableSet.of(n3, n4).equals(tree.getChildren(n2)));
        assertTrue(tree.getParent(n3).equals(n2));
        assertTrue(tree.getChildren(n3).isEmpty());
        assertTrue(tree.getParent(n4).equals(n2));
        assertTrue(tree.getChildren(n4).isEmpty());
    }

    @Test
    public void testSubTree() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        Vertex n6 = new Vertex("n", 6);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        String e45 = "e45";
        String e46 = "e46";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        tree.addNode(n4, n5, e45);
        tree.addNode(n4, n6, e46);
        Tree<Vertex, String> subtree = tree.getSubTree(n4);
        assertTrue(subtree.getNodes().size() == 3);
        assertTrue(subtree.getRoot() == n4);
        assertTrue(subtree.getChildren(n4).size() == 2);
        assertTrue(ImmutableSet.of(n5, n6).equals(subtree.getChildren(n4)));
        assertTrue(ImmutableSet.of(n4, n5, n6).equals(subtree.getNodes()));
    }

    /**
     *     n1                 n1
     *     |                  |
     *     n2          =>     n2
     *   /  \                /
     *  n3   n4             n3
     *  |   / \            / \
     *  n7 n5  n6         n7  n4
     *                       / \
     *                      n5  n6
     */
    @Test
    public void testSetNewParent() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        Vertex n6 = new Vertex("n", 6);
        Vertex n7 = new Vertex("n", 7);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        String e45 = "e45";
        String e46 = "e46";
        String e37 = "e37";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        tree.addNode(n4, n5, e45);
        tree.addNode(n4, n6, e46);
        tree.addNode(n3, n7, e37);
        tree.setNewParent(n4, n3);
        assertTrue(tree.getNodes().size() == 7);
        assertTrue(ImmutableSet.of(n1, n2, n3, n4, n5, n6, n7).equals(tree.getNodes()));
        assertTrue(ImmutableSet.of(n3).equals(tree.getChildren(n2)));
        assertTrue(ImmutableSet.of(n4, n7).equals(tree.getChildren(n3)));
        assertTrue(tree.getParent(n4).equals(n3));
        assertTrue(ImmutableSet.of(n5, n6).equals(tree.getChildren(n4)));
    }

    @Test
    public void testDepthFromRoot() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        String e45 = "e45";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        tree.addNode(n4, n5, e45);
        assertTrue(tree.getDepthFromRoot(n1) == 0);
        assertTrue(tree.getDepthFromRoot(n2) == 1);
        assertTrue(tree.getDepthFromRoot(n3) == 2);
        assertTrue(tree.getDepthFromRoot(n4) == 2);
        assertTrue(tree.getDepthFromRoot(n5) == 3);
    }

    @Test
    public void testFirstCommonAncestor() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        String e12 = "e12";
        String e23 = "e23";
        String e14 = "e14";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n1, n4, e14);
        try {
            tree.getFirstCommonAncestor(Collections.<Vertex>emptyList());
            fail();
        } catch (GraphException e) {
        }
        assertTrue(tree.getFirstCommonAncestor(Arrays.asList(n2)).equals(n2));
        assertTrue(tree.getFirstCommonAncestor(Arrays.asList(n2, n3)).equals(n2));
        assertTrue(tree.getFirstCommonAncestor(Arrays.asList(n3, n4)).equals(n1));
    }

     /**
     *     n1                 n1
     *     |                 /  \
     *     n2          =>   n3   n4
     *   /  \               |   / \
     *  n3   n4            n7 n5  n6
     *  |   / \
     *  n7 n5  n6
     *
     */
    @Test
    public void testRemoveNode() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        Vertex n6 = new Vertex("n", 6);
        Vertex n7 = new Vertex("n", 7);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        String e45 = "e45";
        String e46 = "e46";
        String e37 = "e37";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        tree.addNode(n4, n5, e45);
        tree.addNode(n4, n6, e46);
        tree.addNode(n3, n7, e37);
        tree.removeNode(n2);
        assertTrue(tree.getNodes().size() == 6);
        assertTrue(ImmutableSet.of(n1, n3, n4, n5, n6, n7).equals(tree.getNodes()));
        assertTrue(!tree.containsNode(n2));
        assertTrue(ImmutableSet.of(n3, n4).equals(tree.getChildren(n1)));
        assertTrue(tree.getParent(n3).equals(n1));
        assertTrue(tree.getParent(n4).equals(n1));
    }

    /**
     *     n1                 n1
     *     |                  |
     *     n2          =>     n2
     *   /  \               /
     *  n3   n4            n3
     *  |   / \            |
     *  n7 n5  n6          n7
     *
     */
    @Test
    public void testRemoveSubtree() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        Vertex n6 = new Vertex("n", 6);
        Vertex n7 = new Vertex("n", 7);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        String e45 = "e45";
        String e46 = "e46";
        String e37 = "e37";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        tree.addNode(n4, n5, e45);
        tree.addNode(n4, n6, e46);
        tree.addNode(n3, n7, e37);
        tree.removeSubtree(n4);
        assertTrue(tree.getNodes().size() == 4);
        assertTrue(ImmutableSet.of(n1, n2, n3, n7).equals(tree.getNodes()));
        assertTrue(ImmutableSet.of(n3).equals(tree.getChildren(n2)));
    }

    /**
     *     n1                 n1
     *     |                  |
     *     n2          =>     n2
     *   /  \                /  \
     *  n3   n4             n3   n8
     *  |   / \             |    |
     *  n7 n5  n6           n7   n4
     *                          / \
     *                         n5  n6
     */
    @Test
    public void testInsertNode() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        Vertex n6 = new Vertex("n", 6);
        Vertex n7 = new Vertex("n", 7);
        Vertex n8 = new Vertex("n", 8);
        String e12 = "e12";
        String e23 = "e23";
        String e24 = "e24";
        String e45 = "e45";
        String e46 = "e46";
        String e37 = "e37";
        String eNew = "eNew";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);
        tree.addNode(n2, n3, e23);
        tree.addNode(n2, n4, e24);
        tree.addNode(n4, n5, e45);
        tree.addNode(n4, n6, e46);
        tree.addNode(n3, n7, e37);
        tree.insertNode(n8, n4, eNew);
        assertTrue(tree.getNodes().size() == 8);
        assertTrue(ImmutableSet.of(n1, n2, n3, n4, n5, n6, n7, n8).equals(tree.getNodes()));
        assertTrue(tree.getParent(n8).equals(n2));
        assertTrue(ImmutableSet.of(n3, n8).equals(tree.getChildren(n2)));
        assertTrue(tree.getParent(n4).equals(n8));
        assertTrue(ImmutableSet.of(n4).equals(tree.getChildren(n8)));
    }

    /**
     *     n1                 n1
     *     |                  |
     *     n2          =>     n2
     *                        |
     *     +                  n3
     *                       /  \
     *     n3               n4   n5
     *    /  \
     *   n4   n5
     *
     */
    @Test
    public void testAddTree() {
        Vertex n1 = new Vertex("n", 1);
        Vertex n2 = new Vertex("n", 2);
        String e12 = "e12";
        MutableTree<Vertex, String> tree = new TreeImpl<>(n1);
        tree.addNode(n1, n2, e12);

        Vertex n3 = new Vertex("n", 3);
        Vertex n4 = new Vertex("n", 4);
        Vertex n5 = new Vertex("n", 5);
        String e34 = "e34";
        String e35 = "e35";
        MutableTree<Vertex, String> tree2 = new TreeImpl<>(n3);
        tree2.addNode(n3, n4, e34);
        tree2.addNode(n3, n5, e35);

        String e23 = "e23";
        tree.addTree(tree2, n2, e23);

        assertTrue(tree.getNodes().size() == 5);
        assertTrue(ImmutableSet.of(n1, n2, n3, n4, n5).equals(tree.getNodes()));
        assertTrue(tree.getParent(n3).equals(n2));
        assertTrue(tree.getParent(n4).equals(n3));
        assertTrue(tree.getParent(n5).equals(n3));
        assertTrue(ImmutableSet.of(n3).equals(tree.getChildren(n2)));
        assertTrue(ImmutableSet.of(n4, n5).equals(tree.getChildren(n3)));
        assertTrue(tree.getIncomingEdge(n3).equals(e23));
    }

}
