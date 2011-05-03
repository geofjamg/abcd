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

package fr.jamgotchian.abcd.core;

import fr.jamgotchian.abcd.core.common.ABCDException;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraphBuilder;
import fr.jamgotchian.abcd.core.controlflow.ControlFlowGraph;
import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.Field;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.Package;
import fr.jamgotchian.abcd.core.ast.stmt.CommentStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.stmt.Statements;
import fr.jamgotchian.abcd.core.output.JavaCompilationUnitWriter;
import fr.jamgotchian.abcd.core.output.TextCodeWriter;
import fr.jamgotchian.abcd.core.analysis.ControlFlowGraphStmtAnalysis;
import fr.jamgotchian.abcd.core.analysis.AbstractSyntaxTreeBuilder;
import fr.jamgotchian.abcd.core.analysis.ConditionalExpressionRefactorer;
import fr.jamgotchian.abcd.core.analysis.DOTUtil;
import static fr.jamgotchian.abcd.core.analysis.DOTUtil.BasicBlockWritingMode.*;
import fr.jamgotchian.abcd.core.analysis.ForLoopRefactorer;
import fr.jamgotchian.abcd.core.analysis.GraphIRBuilder;
import fr.jamgotchian.abcd.core.analysis.Refactorer;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.graph.VertexToString;
import fr.jamgotchian.abcd.core.output.OutputUtil;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.StructuralAnalysis;
import fr.jamgotchian.abcd.core.util.ASMUtil;
import fr.jamgotchian.abcd.core.util.Exceptions;
import fr.jamgotchian.abcd.core.util.SimplestFormatter;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.EmptyVisitor;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class ABCDContext {

    public static final boolean DEBUG = false;

    private static final Logger logger;

    private static List<Refactorer> REFACTORERS = Collections.unmodifiableList(
            Arrays.<Refactorer>asList(new ForLoopRefactorer(),
                                      new ConditionalExpressionRefactorer()));

    static {
        // root logger configuration
        Logger rootLogger = Logger.getLogger(ABCDContext.class.getPackage().getName());
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new SimplestFormatter());
        handler.setLevel(Level.FINEST);
        rootLogger.setLevel(Level.ALL);
        rootLogger.addHandler(handler);
        rootLogger.setUseParentHandlers(false);

        logger = Logger.getLogger(ABCDContext.class.getName());
        logger.setLevel(Level.FINER);
    }

    public static void decompile(File classFile, OutputStream os) throws IOException {
        new ABCDContext().decompileFile(classFile, os);
    }

    public static void analyse(File classFile, File outputDir, boolean drawRegions) throws IOException {
        new ABCDContext().analyseFile(classFile, outputDir, drawRegions);
    }

    private final Map<String, String> innerClasses = new HashMap<String, String>();

    public ABCDContext() {
    }

    private void decompileFile(File classFile, OutputStream os) throws IOException {
        ImportManager importManager = new ImportManager();

        Class _class = decompileClass(classFile, importManager);

        CompilationUnit compilUnit = new CompilationUnit(_class.getPackage(), importManager);
        compilUnit.getClasses().add(_class);

        File classDir = classFile.getParentFile();
        analyseInnerClassesFromSyntheticFields(classDir);

        // find class root directory
        File classRootDir = classDir.getAbsoluteFile();
        int packageDepth = _class.getPackage().getName().split("\\.").length;
        for (int i = 0; i < packageDepth; i++) {
            classRootDir = classRootDir.getParentFile();
        }

        decompileInnerClass(_class, classRootDir, importManager);

        if (os != null) {
            Writer writer = new OutputStreamWriter(new BufferedOutputStream(os));
            try {
                compilUnit.accept(new JavaCompilationUnitWriter(new TextCodeWriter(writer, 4), DEBUG), null);
            } finally {
                writer.close();
            }
        }
    }

    private Class decompileClass(File classFile, ImportManager importManager) throws IOException {
        ClassNode cn = new ClassNode();
        ClassReader cr = new ClassReader(new FileInputStream(classFile));
        cr.accept(cn, 0);

        Package _package = new Package(ASMUtil.getPackageName(cn));

        String className = cn.name.replace('/', '.');

        logger.fine("");
        logger.log(Level.FINE, "########## Decompile class {0} ##########", className);
        logger.fine("");

        String simpleClassName = null;
        int lastDotIndex = className.lastIndexOf('.');
        if (lastDotIndex != -1) {
            simpleClassName = className.substring(lastDotIndex+1);
        } else {
            simpleClassName = className;
        }
        String superClassName = null;
        if (cn.superName != null) {
            superClassName = cn.superName.replace('/', '.');
        }
        Set<Modifier> classModifiers = ASMUtil.getModifiers(cn.access);
        classModifiers.remove(Modifier.SYNCHRONIZED); // ???

        Class _class = new Class(_package, simpleClassName, superClassName, classModifiers);

        for (String _interface : (List<String>) cn.interfaces) {
            _class.addInterface(_interface.replace('/', '.'));
        }

        for (FieldNode fn : (List<FieldNode>) cn.fields) {
            Type fieldType = Type.getType(fn.desc);
            JavaType javaFieldType = JavaType.newType(fieldType, importManager);

            _class.addField(new Field(ASMUtil.getModifiers(fn.access),
                                      fn.name,
                                      javaFieldType));
        }

        for (MethodNode mn : (List<MethodNode>) cn.methods) {
            String methodSignature = ASMUtil.getMethodSignature(cn, mn);
            Type returnType = ASMUtil.getReturnType(mn);
            JavaType javaReturnType = null;
            if (returnType != null) {
                javaReturnType = JavaType.newType(returnType, importManager);
            }
            String methodName = ASMUtil.getMethodName(cn, mn);
            boolean constructor = "<init>".equals(mn.name);
            List<ClassName> exceptions = new ArrayList<ClassName>();
            if (mn.exceptions != null) {
                for (String exception : (List<String>) mn.exceptions) {
                    exceptions.add(importManager.newClassName(exception.replace('/', '.')));
                }
            }

            Set<Modifier> methodModifiers = ASMUtil.getModifiers(mn.access);
            boolean isMethodStatic = methodModifiers.contains(Modifier.STATIC);

            Type[] argTypes = Type.getArgumentTypes(mn.desc);
            List<LocalVariableDeclaration> arguments = new ArrayList<LocalVariableDeclaration>(argTypes.length);
            for(int index = 0; index < argTypes.length; index++) {
                Type argType = argTypes[index];
                int localVarIndex = index;
                // index 0 of local variable table contains this for non static method
                if (!isMethodStatic) {
                    localVarIndex++;
                }
                JavaType javaArgType = JavaType.newType(argType, importManager);
                arguments.add(new LocalVariableDeclaration(localVarIndex, javaArgType));
            }

            Method method = new Method(methodName, methodModifiers,
                                       javaReturnType, arguments, exceptions,
                                       constructor);
            _class.addMethod(method);

            try {
                logger.log(Level.FINE, "");
                logger.log(Level.FINE, "********** Decompile method {0} **********", methodSignature);
                logger.log(Level.FINE, "");

                logger.log(Level.FINE, "////////// Build Control flow graph of {0} //////////", methodSignature);
                ControlFlowGraph graph = new ControlFlowGraphBuilder().build(mn, methodSignature.toString());

                logger.log(Level.FINE, "////////// Analyse Control flow of {0} //////////", methodSignature);
                graph.analyse();

                logger.log(Level.FINE, "////////// Build Statements of {0} //////////", methodSignature);
                new ControlFlowGraphStmtAnalysis().analyse(graph, importManager);

                logger.log(Level.FINE, "////////// Analyse structure of {0} //////////", methodSignature);
                Set<Region> rootRegions = new StructuralAnalysis(graph).analyse();
                if (rootRegions.size() > 1) {
                    throw new ABCDException("Fail to recognize structure");
                }
                Region rootRegion = rootRegions.iterator().next();

                logger.log(Level.FINE, "////////// Build AST of {0} //////////", methodSignature);
                new AbstractSyntaxTreeBuilder(importManager).build(rootRegion, method.getBody());

                for (Refactorer refactorer : REFACTORERS) {
                    refactorer.refactor(method.getBody());
                }

            } catch (Exception exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);

                method.getBody().clear();
                StringBuilder msg = new StringBuilder();
                msg.append(Exceptions.printStackTrace(exc))
                   .append("\n")
                   .append(OutputUtil.toText(mn.instructions));

                method.getBody().add(new CommentStatement("\n" + msg.toString()));
                method.getBody().add(Statements.createThrowErrorStmt(InternalError.class,
                                                                     "Decompilation failed",
                                                                     importManager));
            }
        }
        return _class;
    }

    private void analyseInnerClassesFromSyntheticFields(File dir) throws IOException {
        assert dir.isDirectory();
        logger.log(Level.FINEST, "Analyse inner classes of directory ", dir.getName());

        innerClasses.clear();

        File[] classFiles = dir.listFiles(new FileFilter() {

            public boolean accept(File pathname) {
                return pathname.getName().endsWith(".class");
            }
        });

        for (final File classFile : classFiles) {
            ClassReader cr = new ClassReader(new FileInputStream(classFile));
            cr.accept(new EmptyVisitor() {

                private String innerClassName;

                @Override
                public void visit(int version, int access, String name, String signature,
                                  String superName, String[] interfaces) {
                    innerClassName = name.replace('/', '.');
                }

                @Override
                public FieldVisitor visitField(int access, String name, String desc,
                                               String signature, Object value) {
                    if ((access & Opcodes.ACC_SYNTHETIC) != 0) {
                        Type type = Type.getType(desc);
                        if (type.getSort() == Type.OBJECT) {
                            innerClasses.put(innerClassName, type.getClassName());
                        }
                    }
                    return super.visitField(access, name, desc, signature, value);
                }

            }, 0);
        }
    }

    private void decompileInnerClass(Class outerClass, File classRootDir,
                                     ImportManager importManager) throws IOException {
        String outerClassName = outerClass.getQualifiedName();
        for (Map.Entry<String, String> entry : innerClasses.entrySet()) {
            String innerClassName = entry.getKey();
            if (outerClassName.equals(entry.getValue())) {
                String innerClassFileName = classRootDir.getAbsolutePath() + '/'
                        + innerClassName.replace('.', '/') + ".class";
                File innerClassFile = new File(innerClassFileName);
                Class innerClass = decompileClass(innerClassFile, importManager);
                outerClass.addInnerClass(innerClass);

                decompileInnerClass(innerClass, classRootDir, importManager);
            }
        }
    }

    private void analyseFile(File classFile, File outputDir, boolean drawRegions) throws IOException {
        if (!outputDir.exists()) {
            throw new ABCDException(outputDir + " does not exist");
        }

        if (!outputDir.isDirectory()) {
            throw new ABCDException(outputDir + " is not a directory");
        }

        ClassNode cn = new ClassNode();
        ClassReader cr = new ClassReader(new FileInputStream(classFile));
        cr.accept(cn, 0);

        logger.fine("");
        logger.log(Level.FINE, "########## Analyse class {0} ##########", cn.name.replace('/', '.'));
        logger.fine("");

        File classDir = classFile.getParentFile();
        analyseInnerClassesFromSyntheticFields(classDir);

        StringBuilder builder = new StringBuilder();
        ASMUtil.printInnerClasses(innerClasses, builder);
        logger.log(Level.FINER, "Inner classes :\n{0}", builder.toString());

        List<String> errorMsgs = new ArrayList<String>();

        for (MethodNode mn : (List<MethodNode>) cn.methods) {
            String methodSignature = ASMUtil.getMethodSignature(cn, mn);

            try {
                logger.log(Level.FINE, "********** Analyse method {0} **********", methodSignature);

                logger.log(Level.FINER, "Bytecode :\n{0}", OutputUtil.toText(mn.instructions));

                builder = new StringBuilder();
                ASMUtil.printTryCatchBlocks(mn, builder);
                logger.log(Level.FINER, "Try catch blocks :\n{0}", builder.toString());

                builder = new StringBuilder();
                ASMUtil.printLocalVariableTable(mn, builder);
                logger.log(Level.FINER, "Local variable table :\n{0}", builder.toString());

                logger.log(Level.FINE, "////////// Build Control flow graph of {0} //////////", methodSignature);
                ControlFlowGraph graph = new ControlFlowGraphBuilder().build(mn, methodSignature.toString());

                logger.log(Level.FINE, "////////// Analyse Control flow of {0} //////////", methodSignature);
                graph.analyse();

                String baseName = outputDir.getPath() + "/" + methodSignature;

                if (!drawRegions) {
                    Writer writer = new FileWriter(baseName + "_CFG.dot");
                    try {
                        DOTUtil.writeCFG(graph, null, writer, INSTN_RANGE);
                    } finally {
                        writer.close();
                    }

                    writer = new FileWriter(baseName + "_BC.dot");
                    try {
                        DOTUtil.writeCFG(graph, null, writer, BYTECODE);
                    } finally {
                        writer.close();
                    }
                }

                logger.log(Level.FINE, "////////// Build Statements of {0} //////////", methodSignature);
                new ControlFlowGraphStmtAnalysis().analyse(graph, new ImportManager());

                logger.log(Level.FINE, "////////// Build IR Instructions of {0} //////////", methodSignature);
                new GraphIRBuilder().analyse(graph, new ImportManager());

                Set<Region> rootRegions = null;
                if (drawRegions) {
                    logger.log(Level.FINE, "////////// Analyse structure of {0} //////////", methodSignature);
                    StructuralAnalysis analysis = new StructuralAnalysis(graph);
                    rootRegions = analysis.analyse();

                    Writer writer = new FileWriter(baseName + "_RG.dot");
                    try {
                        DOTUtil.writeGraph(analysis.getRegionGraph(), "RG", writer, new VertexToString<Region>() {

                            public String toString(Region region) {
                                return region + " (" + region.getTypeName() + ")";
                            }
                        });
                    } finally {
                        writer.close();
                    }

                    writer = new FileWriter(baseName + "_CFG.dot");
                    try {
                        DOTUtil.writeCFG(graph, rootRegions, writer, INSTN_RANGE);
                    } finally {
                        writer.close();
                    }

                    writer = new FileWriter(baseName + "_BC.dot");
                    try {
                        DOTUtil.writeCFG(graph, rootRegions, writer, BYTECODE);
                    } finally {
                        writer.close();
                    }
                }

                Writer writer = new FileWriter(baseName + "_STMT.dot");
                try {
                    DOTUtil.writeCFG(graph, rootRegions, writer, STATEMENTS);
                } finally {
                    writer.close();
                }

                writer = new FileWriter(baseName + "_DT.dot");
                try {
                    graph.getDominatorInfo().getDominatorsTree().writeDOT("DT", writer);
                } finally {
                    writer.close();
                }

                writer = new FileWriter(baseName + "_PDT.dot");
                try {
                    graph.getDominatorInfo().getPostDominatorsTree().writeDOT("PDT", writer);
                } finally {
                    writer.close();
                }

                if (rootRegions != null && rootRegions.size() > 1) {
                    throw new ABCDException("Fail to recognize structure");
                }
            } catch (Exception exc) {
                errorMsgs.add(exc.toString() + " (" + methodSignature + ")");
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }

        logger.fine("");
        logger.log(Level.FINE, "########## Summary {0} ##########", cn.name.replace('/', '.'));
        logger.fine("");
        logger.log(Level.FINE, "Succeed : {0}/{1}",
                new Object[]{cn.methods.size() - errorMsgs.size(), cn.methods.size()});
        StringBuilder errorStr = new StringBuilder();
        for (String errorMsg : errorMsgs) {
            errorStr.append("  ").append(errorMsg).append("\n");
        }
        logger.log(Level.FINE, "Failed : {0}/{1}\n{2}",
                new Object[]{errorMsgs.size(), cn.methods.size(),
                    errorStr.toString()});
    }

    private static void printUsage() {
        System.out.println("Usage:");
        System.out.println("    -decompile <class file> <output java file>");
        System.out.println("    -analyse <class file> <output directory> [<-regions>]");
        System.exit(1);
    }

    public static void main(String[] args) {
        if (args.length != 3 && args.length != 4) {
            printUsage();
        }
        String cmd = args[0];
        String classFileName = args[1];
        File classFile = new File(classFileName);
        if ("-decompile".equals(cmd)) {
            if (args.length != 3) {
                printUsage();
            }
            String javaFileName = args[2];
            try {
                OutputStream os = new FileOutputStream(javaFileName);

                ABCDContext.decompile(classFile, os);
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        } else if ("-analyse".equals(cmd)) {
            File outputDir = new File(args[2]);
            boolean drawRegions = false;
            if (args.length == 4) {
                if("-regions".equals(args[3])) {
                    drawRegions = true;
                } else {
                    printUsage();
                }
            }
            try {
                ABCDContext.analyse(classFile, outputDir, drawRegions);
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        } else {
            printUsage();
        }
    }
}
