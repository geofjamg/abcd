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
package fr.jamgotchian.abcd.core.bytecode.dalvik;

import fr.jamgotchian.abcd.core.bytecode.ClassFactory;
import fr.jamgotchian.abcd.core.bytecode.ABCDDataSource;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.jf.dexlib.ClassDefItem;
import org.jf.dexlib.DexFile;
import org.jf.dexlib.ItemType;
import org.jf.dexlib.Section;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DexFileDataSource implements ABCDDataSource {

    private final File file;

    public DexFileDataSource(File file) {
        this.file = file;
    }

    @Override
    public Collection<ClassFactory> createClassFactories() throws IOException {
        DexFile dexFile = new DexFile(file);
        Section<ClassDefItem> section =
                dexFile.getSectionForType(ItemType.TYPE_CLASS_DEF_ITEM);
        List<ClassFactory> factories = new ArrayList<ClassFactory>();
        for (ClassDefItem item : section.getItems()) {
            factories.add(new DalvikBytecodeClassFactory(item));
        }
        return factories;
    }

}
