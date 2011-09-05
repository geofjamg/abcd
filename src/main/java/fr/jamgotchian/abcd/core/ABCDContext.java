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
import fr.jamgotchian.abcd.core.controlflow.LocalVariableTable;
import fr.jamgotchian.abcd.core.controlflow.TACInstFactory;
import fr.jamgotchian.abcd.core.controlflow.TemporaryVariableFactory;
import fr.jamgotchian.abcd.core.controlflow.VariableID;
import fr.jamgotchian.abcd.core.ast.Class;
import fr.jamgotchian.abcd.core.ast.CompilationUnit;
import fr.jamgotchian.abcd.core.ast.Field;
import fr.jamgotchian.abcd.core.ast.Method;
import fr.jamgotchian.abcd.core.ast.Package;
import fr.jamgotchian.abcd.core.ast.ImportManager;
import fr.jamgotchian.abcd.core.ast.stmt.CommentStatement;
import fr.jamgotchian.abcd.core.ast.stmt.LocalVariableDeclaration;
import fr.jamgotchian.abcd.core.ast.stmt.Statements;
import fr.jamgotchian.abcd.core.ast.expr.Expressions;
import fr.jamgotchian.abcd.core.ast.expr.LocalVariable;
import fr.jamgotchian.abcd.core.ast.util.Refactorer;
import fr.jamgotchian.abcd.core.ast.util.ForLoopRefactorer;
import fr.jamgotchian.abcd.core.analysis.AbstractSyntaxTreeBuilder;
import fr.jamgotchian.abcd.core.analysis.LocalVariableTypeAnalyser;
import fr.jamgotchian.abcd.core.analysis.LogicalOperatorBuilder;
import fr.jamgotchian.abcd.core.analysis.TreeAddressCodeBuilder;
import fr.jamgotchian.abcd.core.analysis.TernaryOperatorBuilder;
import fr.jamgotchian.abcd.core.type.ClassName;
import fr.jamgotchian.abcd.core.type.JavaType;
import fr.jamgotchian.abcd.core.controlflow.OutputUtil;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.RegionGraph;
import fr.jamgotchian.abcd.core.region.StructuralAnalysis;
import fr.jamgotchian.abcd.core.util.ASMUtil;
import fr.jamgotchian.abcd.core.util.ConsoleUtil;
import fr.jamgotchian.abcd.core.util.Exceptions;
import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

    private static final Logger logger = Logger.getLogger(ABCDContext.class.getName());

    private static List<Refactorer> REFACTORERS = Collections.unmodifiableList(
            Arrays.<Refactorer>asList(new ForLoopRefactorer()));

    public static void decompile(File classFile, OutputStream os) throws IOException {
        new ABCDContext().decompileFile(classFile, new DefaultOutputHandler(DEBUG, os));
    }

    public static void analyse(File classFile, OutputStream os,
                               File outputDir) throws IOException {
        new ABCDContext().decompileFile(classFile, new DebugOutputHandler(DEBUG, os, outputDir));
    }

    private final Map<String, String> innerClasses = new HashMap<String, String>();

    public ABCDContext() {
    }

    private void decompileFile(File classFile, OutputHandler handler) throws IOException {
        ImportManager importManager = new ImportManager();

        Class _class = decompileClass(classFile, importManager, handler);

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

        decompileInnerClass(_class, classRootDir, importManager, handler);

        handler.abstractSyntaxTreeBuilt(compilUnit);
    }

    private Class createClass(ClassNode cn, ImportManager importManager) {
        // package
        String packageName = "";
        int lastDotIndex = cn.name.lastIndexOf('/');
        if (lastDotIndex != -1) {
            packageName = cn.name.substring(0, lastDotIndex).replace('/', '.');
        }
        Package _package = new Package(packageName);

        // class name
        String className = cn.name.replace('/', '.');
        String simpleClassName = null;
        lastDotIndex = className.lastIndexOf('.');
        if (lastDotIndex != -1) {
            simpleClassName = className.substring(lastDotIndex+1);
        } else { // class is in default package
            simpleClassName = className;
        }

        // super class name
        String superClassName = null;
        if (cn.superName != null) {
            superClassName = cn.superName.replace('/', '.');
        }

        // class modifiers
        Set<Modifier> classModifiers = ASMUtil.getModifiers(cn.access);
        classModifiers.remove(Modifier.SYNCHRONIZED); // ???

        Class _class = new Class(_package, simpleClassName, superClassName, classModifiers);

        // implemented interfaces
        for (String _interface : (List<String>) cn.interfaces) {
            _class.addInterface(_interface.replace('/', '.'));
        }

        // fields
        for (FieldNode fn : (List<FieldNode>) cn.fields) {
            Type fieldType = Type.getType(fn.desc);
            JavaType javaFieldType = JavaType.newType(fieldType, importManager);

            _class.addField(new Field(ASMUtil.getModifiers(fn.access),
                                      fn.name,
                                      javaFieldType));
        }
        return _class;
    }

    private Method createMethod(ClassNode cn, MethodNode mn, ImportManager importManager) {
        // return type
        JavaType javaReturnType = null;
        if (!"<init>".equals(mn.name)) {
            Type returnType = Type.getReturnType(mn.desc);
            javaReturnType = JavaType.newType(returnType, importManager);
        }

        // method name
        String methodName;
        if ("<init>".equals(mn.name)) {
            String className = cn.name.replace('/', '.');
            int lastDotIndex = className.lastIndexOf('.');
            if (lastDotIndex != -1) {
                methodName = className.substring(lastDotIndex+1);
            } else {
                methodName = className;
            }
        } else {
            methodName = mn.name;
        }

        // contructor or method ?
        boolean constructor = "<init>".equals(mn.name);

        // throwable exceptions
        List<ClassName> exceptions = new ArrayList<ClassName>();
        if (mn.exceptions != null) {
            for (String exception : (List<String>) mn.exceptions) {
                exceptions.add(importManager.newClassName(exception.replace('/', '.')));
            }
        }

        // method modifiers
        Set<Modifier> methodModifiers = ASMUtil.getModifiers(mn.access);

        // parameters
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
            LocalVariable var = Expressions.newVarExpr(new VariableID(localVarIndex), "");
            arguments.add(new LocalVariableDeclaration(var, javaArgType));
        }

        Method method = new Method(methodName, methodModifiers,
                                   javaReturnType, arguments, exceptions,
                                   constructor);
        return method;
    }

    private Class decompileClass(File classFile, ImportManager importManager,
                                 OutputHandler handler) throws IOException {
        ClassNode cn = new ClassNode();
        ClassReader cr = new ClassReader(new FileInputStream(classFile));
        cr.accept(cn, 0);

        Class _class = createClass(cn, importManager);

        logger.log(Level.FINE, "\n\n{0}\n",
                ConsoleUtil.printTitledSeparator("Decompile class " + _class.getQualifiedName(), '#'));

        List<String> errorMsgs = new ArrayList<String>();

        for (MethodNode mn : (List<MethodNode>) cn.methods) {
            Method method = createMethod(cn, mn, importManager);
            _class.addMethod(method);

            String methodSignature = method.getSignature();

            try {
                logger.log(Level.FINE, "\n\n{0}\n",
                        ConsoleUtil.printTitledSeparator("Decompile method " + methodSignature, '%'));

                logger.log(Level.FINER, "Bytecode :\n{0}", OutputUtil.toText(mn.instructions));

                logger.log(Level.FINE, "\n{0}",
                        ConsoleUtil.printTitledSeparator("Build Control flow graph of " + methodSignature, '='));
                ControlFlowGraph graph = new ControlFlowGraphBuilder().build(mn, methodSignature.toString());
                graph.compact();
                graph.analyseLoops();

                StringBuilder builder = new StringBuilder();
                graph.getExceptionTable().print(builder);
                logger.log(Level.FINER, "Exception table :\n{0}", builder.toString());

                builder = new StringBuilder();
                graph.getLocalVariableTable().print(builder);
                logger.log(Level.FINER, "Local variable table :\n{0}", builder.toString());

                LocalVariableTable table = graph.getLocalVariableTable();
                for (LocalVariableDeclaration decl : method.getArguments()) {
                    LocalVariable var = decl.getVariable();
                    var.setName(table.getName(var.getID().getIndex(), 0));
                }

                handler.controlFlowGraphBuilt(graph);

                logger.log(Level.FINE, "\n{0}",
                        ConsoleUtil.printTitledSeparator("Build 3AC instructions of " + methodSignature, '='));

                TemporaryVariableFactory tmpVarFactory = new TemporaryVariableFactory();
                TACInstFactory instFactory = new TACInstFactory();

                new TreeAddressCodeBuilder(graph, importManager, tmpVarFactory,
                                           instFactory).build();

                graph.compact();
                graph.analyseLoops();
                graph.addFakeEdges();

                logger.log(Level.FINE, "\n{0}",
                        ConsoleUtil.printTitledSeparator("Build complex logical operators " + methodSignature, '='));

                new LogicalOperatorBuilder(graph, tmpVarFactory, instFactory).builder();

                logger.log(Level.FINE, "\n{0}",
                        ConsoleUtil.printTitledSeparator("Build ternary operators " + methodSignature, '='));

                // must be done after complex logical operators building because
                // of ternary operator with complex condition
                new TernaryOperatorBuilder(graph, tmpVarFactory, instFactory).build();

                // analyse local variables types
                new LocalVariableTypeAnalyser(graph, method, importManager,
                                              instFactory).analyse();

                handler.treeAddressCodeBuilt(graph);

//                new StructuralAnalysis2(graph).analysis();

                logger.log(Level.FINE, "\n{0}",
                        ConsoleUtil.printTitledSeparator("Analyse structure of " + methodSignature, '='));
                RegionGraph regionGraph = new StructuralAnalysis(graph).analyse();

                handler.regionGraphBuilt(regionGraph);

                Set<Region> rootRegions = regionGraph.getRegions();
                if (rootRegions.size() > 1) {
                    throw new ABCDException("Fail to recognize structure");
                }
                Region rootRegion = rootRegions.iterator().next();

                logger.log(Level.FINE, "\n{0}",
                        ConsoleUtil.printTitledSeparator("Build AST of " + methodSignature, '='));
                new AbstractSyntaxTreeBuilder(graph, importManager,
                                              rootRegion, method.getBody()).build();

                // refactor AST
//                for (Refactorer refactorer : REFACTORERS) {
//                    refactorer.refactor(method.getBody());
//                }

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

                errorMsgs.add(exc.toString() + " (" + methodSignature + ")");
            }
        }

        logger.log(Level.FINE, "\n\n{0}\n",
                ConsoleUtil.printTitledSeparator("Summary " + _class.getQualifiedName(), '#'));
        logger.log(Level.FINE, "Succeed : {0}/{1}",
                new Object[]{cn.methods.size() - errorMsgs.size(), cn.methods.size()});
        StringBuilder errorStr = new StringBuilder();
        for (String errorMsg : errorMsgs) {
            errorStr.append("  ").append(errorMsg).append("\n");
        }
        logger.log(Level.FINE, "Failed : {0}/{1}\n{2}",
                new Object[]{errorMsgs.size(), cn.methods.size(),
                    errorStr.toString()});

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
                                     ImportManager importManager,
                                     OutputHandler handler) throws IOException {
        String outerClassName = outerClass.getQualifiedName();
        for (Map.Entry<String, String> entry : innerClasses.entrySet()) {
            String innerClassName = entry.getKey();
            if (outerClassName.equals(entry.getValue())) {
                String innerClassFileName = classRootDir.getAbsolutePath() + '/'
                        + innerClassName.replace('.', '/') + ".class";
                File innerClassFile = new File(innerClassFileName);
                Class innerClass = decompileClass(innerClassFile, importManager, handler);
                outerClass.addInnerClass(innerClass);

                decompileInnerClass(innerClass, classRootDir, importManager, handler);
            }
        }
    }

    private static void printUsage() {
        System.out.println("Usage:");
        System.out.println("    -class <class file> -java <output java file> [-analyse <dir>]");
        System.exit(1);
    }

    public static void main(String[] args) {
        try {
            if ((args.length != 4 && args.length != 6)
                    || !"-class".equals(args[0])
                    || !"-java".equals(args[2])) {
                printUsage();
            }
            File classFile = new File(args[1]);
            OutputStream os = new FileOutputStream(args[3]);
            if (args.length == 6) {
                if (!"-analyse".equals(args[4])) {
                    printUsage();
                }
                File outputDir = new File(args[5]);
                ABCDContext.analyse(classFile, os, outputDir);
            } else {
                ABCDContext.decompile(classFile, os);
            }
        } catch (Throwable exc) {
            logger.log(Level.SEVERE, exc.toString(), exc);
        }
    }
}
