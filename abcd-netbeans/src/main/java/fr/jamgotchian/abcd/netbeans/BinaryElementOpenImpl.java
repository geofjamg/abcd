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
package fr.jamgotchian.abcd.netbeans;

import javax.lang.model.element.Element;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.modules.java.BinaryElementOpen;
import org.openide.filesystems.FileObject;
import org.openide.util.lookup.ServiceProvider;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
@ServiceProvider(service = BinaryElementOpen.class, position = Integer.MAX_VALUE-1)
public class BinaryElementOpenImpl implements BinaryElementOpen {

    @Override
    public boolean open(ClasspathInfo cpInfo, ElementHandle<? extends Element> toOpen) {
        FileObject source = CodeGenerator.generateCode(cpInfo, toOpen);
        if (source != null) {
            return open(source, toOpen);
        } else {
            return false;
        }
    }

    @SuppressWarnings("deprecation")
    private boolean open(FileObject source, ElementHandle<? extends Element> toOpen) {
        return org.netbeans.api.java.source.UiUtils.open(source, toOpen);
    }
}
