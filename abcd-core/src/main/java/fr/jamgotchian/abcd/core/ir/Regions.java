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

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Regions {

    private Regions() {
    }

    public static boolean deepEquals(RPST rpst, Region region1, Region region2) {
        return deepEquals(rpst, region1, region2, new VariableMapping());
    }

    private static boolean deepEquals(RPST rpst, Region region1, Region region2, VariableMapping mapping) {
        if (region1.getParentType() == region2.getParentType()
                && region1.getChildType() == region2.getChildType()) {
            if (region1.getChildType() == ChildType.CATCH || region1.getChildType() == ChildType.FINALLY) {
                ExceptionHandlerInfo info = (ExceptionHandlerInfo) region1.getEntry().getProperty(BasicBlockPropertyName.EXCEPTION_HANDLER_ENTRY);
                ExceptionHandlerInfo otherInfo = (ExceptionHandlerInfo) region2.getEntry().getProperty(BasicBlockPropertyName.EXCEPTION_HANDLER_ENTRY);
                mapping.defEqual(info.getVariable(), otherInfo.getVariable());
            }
            if (region1.getParentType() == ParentType.BASIC_BLOCK) {
                BasicBlock bb = region1.getEntry();
                BasicBlock otherBb = region2.getEntry();
                return IRInstComparator.equal(bb.getInstructions(),
                                              otherBb.getInstructions(),
                                              mapping);
            } else {
                if (rpst.getChildCount(region1) == rpst.getChildCount(region2)) {
                    for (Region child : rpst.getChildren(region1)) {
                        boolean found = false;
                        for (Region otherChild : rpst.getChildren(region2)) {
                            if (deepEquals(rpst, child, otherChild, mapping)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            return false;
                        }
                    }
                    return true;
                }
            }
        }
        return false;
    }
}
