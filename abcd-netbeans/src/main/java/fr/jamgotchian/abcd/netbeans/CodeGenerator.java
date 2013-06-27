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

import fr.jamgotchian.abcd.core.ABCDContext;
import fr.jamgotchian.abcd.core.bytecode.ABCDDataSource;
import fr.jamgotchian.abcd.core.common.Configuration;
import fr.jamgotchian.abcd.core.common.DecompilationObserver;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.security.MessageDigest;
import java.util.EnumSet;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.TypeElement;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.ClasspathInfo.PathKind;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.java.source.JavaSource.Phase;
import org.netbeans.api.java.source.Task;
import org.netbeans.api.java.source.WorkingCopy;
import org.netbeans.modules.java.source.indexing.JavaIndex;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class CodeGenerator {

    private static final Logger LOG = LoggerFactory.getLogger(CodeGenerator.class);
    private static final Set<ElementKind> UNUSABLE_KINDS = EnumSet.of(ElementKind.PACKAGE);
    private static final String HASH_ATTRIBUTE_NAME = "origin-hash";

    public static FileObject generateCode(final ClasspathInfo cpInfo, final ElementHandle<? extends Element> toOpenHandle) {
	if (UNUSABLE_KINDS.contains(toOpenHandle.getKind())) {
	    return null;
	}

        final ClassPath cp = ClassPathSupport.createProxyClassPath(
                cpInfo.getClassPath(PathKind.BOOT),
                cpInfo.getClassPath(PathKind.COMPILE),
                cpInfo.getClassPath(PathKind.SOURCE));

        try {
            FileObject testFile = FileUtil.createMemoryFileSystem().getRoot().createData("test.java");  //NOI18N

            try (OutputStream testOut = testFile.getOutputStream()) {
                FileUtil.copy(new ByteArrayInputStream("".getBytes("UTF-8")), testOut); //NOI18N
            }

            JavaSource js = JavaSource.create(cpInfo, testFile);
            final FileObject[] resource = new FileObject[1];
            final FileObject[] result = new FileObject[1];
            final boolean[] sourceGenerated = new boolean[1];

            js.runModificationTask(new Task<WorkingCopy>() {
                @Override
                public void run(WorkingCopy wc) throws Exception {
                    wc.toPhase(Phase.PARSED);

                    final Element toOpen = toOpenHandle.resolve(wc);
                    final TypeElement te = toOpen != null ? wc.getElementUtilities().outermostTypeElement(toOpen) : null;

                    if (te == null) {
                        LOG.info("Cannot resolve element: " + toOpenHandle.toString() + " on classpath: " + cp.toString()); //NOI18N
                        return;
                    }

                    final String resourceName = te.getQualifiedName().toString().replace('.', '/') + ".class";  //NOI18N
                    resource[0] = cp.findResource(resourceName);
                    if (resource[0] == null) {
                        LOG.info("Cannot find resource: " + resourceName +" on classpath: " + cp.toString()); //NOI18N
                        return ;
                    }

                    final FileObject root = cp.findOwnerRoot(resource[0]);
                    if (root == null) {
                        LOG.info("Cannot find owner of: " + FileUtil.getFileDisplayName(resource[0]) +" on classpath: " + cp.toString()); //NOI18N
                        return ;
                    }

                    final File  sourceRoot = new File (JavaIndex.getIndex(root.getURL()),"gensrc");     //NOI18N
                    final FileObject sourceRootFO = FileUtil.createFolder(sourceRoot);
                    if (sourceRootFO == null) {
                        LOG.info("Cannot create folder: " + sourceRoot); //NOI18N
                        return ;
                    }

                    final String path = te.getQualifiedName().toString().replace('.', '/') + ".java";   //NOI18N
                    final FileObject source = sourceRootFO.getFileObject(path);

                    MessageDigest md = MessageDigest.getInstance("MD5");
                    byte[] hashBytes = md.digest(resource[0].asBytes());
                    StringBuilder hashBuilder = new StringBuilder();

                    for (byte b : hashBytes) {
                        hashBuilder.append(String.format("%02X", b));
                    }

                    String hash = hashBuilder.toString();

                    if (source != null) {
                        result[0] = source;

                        String existingHash = (String) source.getAttribute(HASH_ATTRIBUTE_NAME);

                        if (hash.equals(existingHash)) {
                            LOG.debug(FileUtil.getFileDisplayName(source) + " is up to date, reusing from cache.");  //NOI18N
                            return;
                        }
                    }

                    if (source == null) {
                        result[0] = FileUtil.createData(sourceRootFO, path);
                        LOG.debug(FileUtil.getFileDisplayName(result[0]) + " does not exist, creating.");  //NOI18N
                    } else {
                        LOG.debug(FileUtil.getFileDisplayName(source) + " is not up to date, regenerating.");  //NOI18N
                    }

                    result[0].setAttribute(HASH_ATTRIBUTE_NAME, hash);

                    sourceGenerated[0] = true;
                }
            });

            if (sourceGenerated[0]) {
                final File resultFile = FileUtil.toFile(result[0]);
                if (resultFile != null && !resultFile.canWrite()) {
                    resultFile.setWritable(true);
                }
                try (OutputStream resultOut = result[0].getOutputStream()) {
                    ABCDDataSource dataSrc = new NbABCDDataSource(resource[0].getInputStream());
                    Configuration config = Lookup.getDefault().lookup(Configuration.class);
                    DecompilationObserver observer = new NbDecompilationObserver(resultOut, config);
                    new ABCDContext().decompile(dataSrc, observer, config, cp.getClassLoader(true));
                }
                if (resultFile != null) {
                    resultFile.setReadOnly();
                }
            }

            return result[0];
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
            return null;
        }
    }

    private CodeGenerator() {
    }
}
