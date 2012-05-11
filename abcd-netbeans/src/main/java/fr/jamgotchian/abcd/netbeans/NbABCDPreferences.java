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
package fr.jamgotchian.abcd.netbeans;

import fr.jamgotchian.abcd.core.common.ABCDAbstractPreferences;
import fr.jamgotchian.abcd.core.common.ABCDPreferences;
import org.openide.util.NbPreferences;
import org.openide.util.lookup.ServiceProvider;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
@ServiceProvider(service=ABCDPreferences.class)
public class NbABCDPreferences extends ABCDAbstractPreferences {

    private static final String USE_LOCAL_VARIABLE_TABLE = "USE_LOCAL_VARIABLE_TABLE";

    private static final String ANALYSE_LOCAL_VARIABLE_TYPE = "ANALYSE_LOCAL_VARIABLE_TYPE";

    @Override
    public boolean isUseLocalVariableTable() {
        return NbPreferences.forModule(PreferencesPanel.class)
                .getBoolean(USE_LOCAL_VARIABLE_TABLE, false);
    }

    @Override
    public void setUseLocalVariableTable(boolean useLocalVariableTable) {
        NbPreferences.forModule(PreferencesPanel.class)
                .putBoolean(USE_LOCAL_VARIABLE_TABLE, useLocalVariableTable);
    }

    @Override
    public boolean isAnalyseLocalVariableType() {
        return NbPreferences.forModule(PreferencesPanel.class)
                .getBoolean(ANALYSE_LOCAL_VARIABLE_TYPE, true);
    }

    @Override
    public void setAnalyseLocalVariableType(boolean analyseLocalVariableType) {
        NbPreferences.forModule(PreferencesPanel.class)
                .putBoolean(ANALYSE_LOCAL_VARIABLE_TYPE, analyseLocalVariableType);
    }
}
