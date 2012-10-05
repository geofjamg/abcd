/*
 * Copyright (C) 2012 geo
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
package fr.jamgotchian.abcd.core;

import fr.jamgotchian.abcd.core.bytecode.ABCDDataSource;
import fr.jamgotchian.abcd.core.common.ABCDPreferences;
import fr.jamgotchian.abcd.core.common.DecompilationObserver;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class DecompilationTest {
        
    private static JavaCompiler compiler;
    
    public DecompilationTest() {
    }
    
    @BeforeClass
    public static void setUpClass() {
        compiler = ToolProvider.getSystemJavaCompiler();
    }
    
    @AfterClass
    public static void tearDownClass() {
        compiler = null;
    }
    
    @Before
    public void setUp() {
    }
    
    @After
    public void tearDown() {
    }

    private byte[] compile(String className, String source) {
        ClassFileManager fileManager = new ClassFileManager(compiler.getStandardFileManager(null, null, null));
        JavaFileObject file = new CharSequenceJavaFileObject(className, source);
        compiler.getTask(null, fileManager, null, null, null, Collections.singleton(file)).call();
        return fileManager.getClassBytes();        
    }
    
    private String decompile(byte[] classFile) throws IOException {
        ABCDDataSource dataSrc = new ByteArrayDataSource(classFile);
        Writer writer = new StringWriter();
        try {
            ABCDPreferences prefs = new ABCDPreferencesImpl();
            DecompilationObserver observer = new TestDecompilationObserver(writer, prefs);
            new ABCDContext().decompile(dataSrc, observer, prefs, getClass().getClassLoader());
        } finally {
            writer.close();
        }
        return writer.toString();
    }

    private boolean test(String className, String source) throws IOException {
        byte[] classFile = compile(className, source);
        String decompiledSource = decompile(classFile);
        System.out.println(decompiledSource);
        return source.equals(decompiledSource);
    }
    
    @Test
    public void testIf() throws Exception {
        String className = "fr.jamgotchian.abcd.test.Test";
        String source = 
                "package fr.jamgotchian.abcd.test;\n" +
                "\n" +
                "public class Test {\n" +
                "    void test(int a) {\n" +
                "        if (a < 1) {\n" +
                "            System.out.println(\"enter if\");\n" +
                "        }\n" +
                "        System.out.println(\"after if\");\n" +
                "    }\n" +
                "}\n";
        // TODO: does not work yet
//        Assert.assertTrue(test(className, source));
    }
}
