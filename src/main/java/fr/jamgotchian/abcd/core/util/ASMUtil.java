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

package fr.jamgotchian.abcd.core.util;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.InsnList;
import org.objectweb.asm.tree.LabelNode;
import org.objectweb.asm.tree.LocalVariableNode;
import org.objectweb.asm.tree.MethodNode;
import org.objectweb.asm.tree.TryCatchBlockNode;


/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ASMUtil implements Opcodes {

    private ASMUtil() {
    }

    public static Set<Modifier> getModifiers(int access) {
        Set<Modifier> modifiers = EnumSet.noneOf(Modifier.class);

        if ((access & ACC_ABSTRACT) != 0) {
            modifiers.add(Modifier.ABSTRACT);
        }
        if ((access & ACC_FINAL) != 0) {
            modifiers.add(Modifier.FINAL);
        }
        if ((access & ACC_NATIVE) != 0) {
            modifiers.add(Modifier.NATIVE);
        }
        if ((access & ACC_PRIVATE) != 0) {
            modifiers.add(Modifier.PRIVATE);
        }
        if ((access & ACC_PROTECTED) != 0) {
            modifiers.add(Modifier.PROTECTED);
        }
        if ((access & ACC_PUBLIC) != 0) {
            modifiers.add(Modifier.PUBLIC);
        }
        if ((access & ACC_STATIC) != 0) {
            modifiers.add(Modifier.STATIC);
        }
        if ((access & ACC_SYNCHRONIZED) != 0) {
            modifiers.add(Modifier.SYNCHRONIZED);
        }
        if ((access & ACC_TRANSIENT) != 0) {
            modifiers.add(Modifier.TRANSIENT);
        }
        if ((access & ACC_VOLATILE) != 0) {
            modifiers.add(Modifier.VOLATILE);
        }
        return modifiers;
    }

    public static Map<LabelNode, Integer> getLabelNodeIndexMap(InsnList instructions) {
        Map<LabelNode, Integer> labelNodeIndex = new HashMap<LabelNode, Integer>();
        for (int i = 0; i < instructions.size(); i++) {
            AbstractInsnNode node = instructions.get(i);
            if (node.getType() == AbstractInsnNode.LABEL) {
                labelNodeIndex.put((LabelNode) node, i);
            }
        }
        return labelNodeIndex;
    }

    public static void printLocalVariableTable(MethodNode mn, StringBuilder builder) {
        Map<LabelNode, Integer> labelNodeIndex = getLabelNodeIndexMap(mn.instructions);
        int rowCount = mn.localVariables.size()+1;
        List<String> indexColumn = new ArrayList<String>(rowCount);
        List<String> startColumn = new ArrayList<String>(rowCount);
        List<String> endColumn = new ArrayList<String>(rowCount);
        List<String> nameColumn = new ArrayList<String>(rowCount);
        List<String> typeColumn = new ArrayList<String>(rowCount);
        indexColumn.add("index");
        startColumn.add("start");
        endColumn.add("end");
        nameColumn.add("name");
        typeColumn.add("type");
        for (int i = 0; i < mn.localVariables.size(); i++) {
            LocalVariableNode node = (LocalVariableNode) mn.localVariables.get(i);
            indexColumn.add(Integer.toString(node.index));
            startColumn.add(Integer.toString(labelNodeIndex.get(node.start)));
            endColumn.add(Integer.toString(labelNodeIndex.get(node.end)));
            nameColumn.add(node.name);
            typeColumn.add(node.desc);
        }
        ConsoleUtil.printTable(builder, indexColumn, startColumn, endColumn,
                               nameColumn, typeColumn);
    }

    public static void printTryCatchBlocks(MethodNode mn, StringBuilder builder) {
        Map<LabelNode, Integer> labelNodeIndex = getLabelNodeIndexMap(mn.instructions);
        int rowCount = mn.tryCatchBlocks.size() + 1;
        List<String> tryStartColumn = new ArrayList<String>(rowCount);
        List<String> tryEndColumn = new ArrayList<String>(rowCount);
        List<String> catchStartColumn = new ArrayList<String>(rowCount);
        List<String> typeColumn = new ArrayList<String>(rowCount);
        tryStartColumn.add("tryStart");
        tryEndColumn.add("endStart");
        catchStartColumn.add("catchStart");
        typeColumn.add("type");

        for (int i = 0; i < mn.tryCatchBlocks.size(); i++) {
            TryCatchBlockNode node = (TryCatchBlockNode) mn.tryCatchBlocks.get(i);
            int tryStart = labelNodeIndex.get(node.start);
            int tryEnd = labelNodeIndex.get(node.end);
            int catchStart = labelNodeIndex.get(node.handler);
            String exceptionClassName = node.type;
            if (exceptionClassName != null) {
                exceptionClassName = exceptionClassName.replace('/', '.');
            }
            tryStartColumn.add(Integer.toString(tryStart));
            tryEndColumn.add(Integer.toString(tryEnd));
            catchStartColumn.add(Integer.toString(catchStart));
            typeColumn.add(exceptionClassName);
        }
        ConsoleUtil.printTable(builder, tryStartColumn, tryEndColumn,
                               catchStartColumn, typeColumn);
    }

    public static void printInnerClasses(Map<String, String> innerClasses, StringBuilder builder) {
        int rowCount = innerClasses.size()+1;
        List<String> innerClassColumn = new ArrayList<String>(rowCount);
        List<String> outerClassColumn = new ArrayList<String>(rowCount);
        innerClassColumn.add("Inner class");
        outerClassColumn.add("Outer class");
        for (Map.Entry<String, String> entry : innerClasses.entrySet()) {
            String innerClass = entry.getKey();
            String outerClass = entry.getValue();
            innerClassColumn.add(innerClass);
            outerClassColumn.add(outerClass);
        }
        ConsoleUtil.printTable(builder, innerClassColumn, outerClassColumn);
    }
}
