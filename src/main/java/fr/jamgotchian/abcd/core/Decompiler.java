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
import fr.jamgotchian.abcd.core.controlflow.Edge;
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
import fr.jamgotchian.abcd.core.output.BytecodeWriter;
import fr.jamgotchian.abcd.core.analysis.ControlFlowGraphStmtAnalysis;
import fr.jamgotchian.abcd.core.analysis.AbstractSyntaxTreeBuilder;
import fr.jamgotchian.abcd.core.analysis.ConditionalExpressionRefactoring;
import fr.jamgotchian.abcd.core.analysis.ForLoopRefactoring;
import fr.jamgotchian.abcd.core.region.Region;
import fr.jamgotchian.abcd.core.region.StructuralAnalysis;
import fr.jamgotchian.abcd.core.graph.DirectedGraph;
import fr.jamgotchian.abcd.core.util.ASMUtil;
import fr.jamgotchian.abcd.core.util.SimplestFormatter;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.lang.model.element.Modifier;
import org.objectweb.asm.ClassReader;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.ClassNode;
import org.objectweb.asm.tree.FieldNode;
import org.objectweb.asm.tree.MethodNode;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Decompiler {

    public static final boolean DEBUG = false;

    private static final Logger logger;

    static {
        // root logger configuration
        Logger rootLogger = Logger.getLogger(Decompiler.class.getPackage().getName());
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new SimplestFormatter());
        handler.setLevel(Level.FINEST);
        rootLogger.setLevel(Level.ALL);
        rootLogger.addHandler(handler);
        rootLogger.setUseParentHandlers(false);

        logger = Logger.getLogger(Decompiler.class.getName());
        logger.setLevel(Level.FINER);
    }

    private final ClassNode cn;

    private final Map<String, MethodNode> methodNodes;

    public Decompiler(InputStream is) throws IOException {
        cn = new ClassNode();
        ClassReader cr = new ClassReader(is);
        cr.accept(cn, 0);

        methodNodes = new LinkedHashMap<String, MethodNode>();
        for (MethodNode mn : (List<MethodNode>) cn.methods) {
            String methodSignature = ASMUtil.getMethodSignature(cn, mn);
            methodNodes.put(methodSignature, mn);
        }
    }

    public Set<String> getMethodSignatures() {
        return methodNodes.keySet();
    }
    
    public void decompile(OutputStream os) throws IOException {
        String name = cn.name.replace('/', '.');

        Package _package = null;
        String simpleName = null;
        int lastDotIndex = name.lastIndexOf('.');
        if (lastDotIndex != -1) {
            String packageName = name.substring(0, lastDotIndex);
            _package = new Package(packageName);
            simpleName = name.substring(lastDotIndex+1);
        } else {
            simpleName = name;
        }

        CompilationUnit compilUnit = new CompilationUnit(_package);

        String superName = null;
        if (cn.superName != null) {
            superName = cn.superName.replace('/', '.');
        }
        Set<Modifier> classModifiers = ASMUtil.getModifiers(cn.access);
        classModifiers.remove(Modifier.SYNCHRONIZED); // ???

        Class _class = new Class(simpleName, superName, classModifiers);
        compilUnit.getClasses().add(_class);

        for (String _interface : (List<String>) cn.interfaces) {
            _class.addInterface(_interface.replace('/', '.'));
        }

        for (FieldNode fn : (List<FieldNode>) cn.fields) {
            Type fieldType = Type.getType(fn.desc);
            _class.addField(new Field(ASMUtil.getModifiers(fn.access),
                                        fn.name,
                                        fieldType.getClassName()));
        }

        for (Map.Entry<String, MethodNode> entry : methodNodes.entrySet()) {
            String methodSignature = entry.getKey();
            MethodNode mn = entry.getValue();

            String returnTypeName = ASMUtil.getReturnTypeName(mn);
            String methodName = ASMUtil.getMethodName(cn, mn);
            boolean constructor = "<init>".equals(mn.name);
            List<String> exceptions = new ArrayList<String>();
            if (mn.exceptions != null) {
                for (String exception : (List<String>) mn.exceptions) {
                    exceptions.add(exception.replace('/', '.'));
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
                arguments.add(new LocalVariableDeclaration(localVarIndex, argType.getClassName()));
            }

            Method method = new Method(methodName, methodModifiers,
                                       returnTypeName, arguments, exceptions,
                                       constructor);
            _class.addMethod(method);

            try {
                logger.log(Level.FINE, "////////// Build Control flow graph of {0} //////////", methodSignature);
                ControlFlowGraph graph = new ControlFlowGraphBuilder().build(mn, methodSignature.toString());

                logger.log(Level.FINE, "////////// Analyse Control flow of {0} //////////", methodSignature);
                graph.analyse();

                logger.log(Level.FINE, "////////// Build Statements of {0} //////////", methodSignature);
                new ControlFlowGraphStmtAnalysis().analyse(graph);

                logger.log(Level.FINE, "////////// Analyse structure of {0} //////////", methodSignature);
                DirectedGraph<Region, Edge> regionGraph = new StructuralAnalysis(graph).analyse();
                if (regionGraph.getVertexCount() != 1) {
                    throw new ABCDException("Fail to recognize structure");
                }

                logger.log(Level.FINE, "////////// Build AST of {0} //////////", methodSignature);

                Region rootRegion = regionGraph.getVertices().iterator().next();
                new AbstractSyntaxTreeBuilder().build(rootRegion, method.getBody());

                method.getBody().accept(new ForLoopRefactoring(), null);
                method.getBody().accept(new ConditionalExpressionRefactoring(), null);

            } catch (Exception exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);

                method.getBody().clear();
                String bc = BytecodeWriter.toString(mn.instructions);
                method.getBody().add(new CommentStatement("\n" + bc));
                method.getBody().add(Statements.createThrowStmt(InternalError.class, "Decompilation failed"));
            }
        }

        if (os != null) {
            Writer writer = new OutputStreamWriter(new BufferedOutputStream(os));
            try {
                compilUnit.accept(new JavaCompilationUnitWriter(new TextCodeWriter(writer, 4), DEBUG), null);
            } finally {
                writer.close();
            }
        }
    }

    public void analyse(File dir) throws IOException {
        for (Map.Entry<String, MethodNode> entry : methodNodes.entrySet()) {
            String methodSignature = entry.getKey();
            MethodNode mn = entry.getValue();

            try {
                logger.log(Level.FINE, "////////// Build Control flow graph of {0} //////////", methodSignature);
                ControlFlowGraph graph = new ControlFlowGraphBuilder().build(mn, methodSignature.toString());

                logger.log(Level.FINE, "////////// Analyse Control flow of {0} //////////", methodSignature);
                graph.analyse();

                Writer writer = new FileWriter(dir.getPath() + "/" + methodSignature + "_CFG.dot");
                graph.writeDOT(writer);
                writer.close();
                
                writer = new FileWriter(dir.getPath() + "/" + methodSignature + "_DT.dot");
                graph.getDominatorInfo().getDominatorsTree().writeDOT("DT", writer);
                writer.close();
            
                writer = new FileWriter(dir.getPath() + "/" + methodSignature + "_PDT.dot");
                graph.getDominatorInfo().getPostDominatorsTree().writeDOT("PDT", writer);
                writer.close();
            } catch (ABCDException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }
        }
    }
    
    private static void printUsage() {
        System.out.println("Usage:");
        System.out.println("    -decompile <class file> <output java file>");
        System.out.println("    -analyse <class file> <output directory>");
        System.exit(1);
    }

    public static void main(String[] args) {
        if (args.length != 3) {
            printUsage();
        }
        String cmd = args[0];
        String classFileName = args[1];
        if ("-decompile".equals(cmd)) {
            String javaFileName = args[2];
            try {
                InputStream is = new FileInputStream(classFileName);
                OutputStream os = new FileOutputStream(javaFileName);

                Decompiler decompiler = new Decompiler(is);
                decompiler.decompile(os);
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            }            
        } else if ("-analyse".equals(cmd)) {
            String directoryName = args[2];
            File dir = new File(directoryName);
            if (!dir.exists())
                throw new ABCDException(directoryName + " does not exist");
            if (!dir.isDirectory())
                throw new ABCDException(directoryName + " is not a directory");
            try {
                InputStream is = new FileInputStream(classFileName);
                Decompiler decompiler = new Decompiler(is);
                decompiler.analyse(dir);
            } catch (IOException exc) {
                logger.log(Level.SEVERE, exc.toString(), exc);
            } 
        } else {
            printUsage();
        }
    }
}
