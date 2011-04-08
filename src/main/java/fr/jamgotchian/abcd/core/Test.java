package fr.jamgotchian.abcd.core;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.xml.stream.XMLStreamException;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Test {

    static {
        System.out.println("hello");
    }

    static {
        System.out.println("hello2");
    }

    enum TestEnum {
        A,
        B,
        C
    }

    int zz;
    float ee;

    public Test(int zz, float ee) {
        this.zz = zz;
        this.ee = ee;
    }

    static public class InnerStaticTestClass {
        void hello() {
            System.out.println("hello");
        }
    }

    private int driiing(int i) {
        System.out.println("driing");
        return i+1;
    }

    void driiing2() {
        System.out.println("driing2");
    }
    
    public class InnerTestClass {
        void hello() {
            a();
        }
        
        void hello2() {
            int a = driiing(3);
            driiing2();
        }
    }

    public class InnerTestClass2 {
        
        class InnerInnerTestClass {
            void aaa() {
                System.out.println("aaa");
            }
        }
        
        void hello() {
            a();
        }
        
        void hello2() {
            int a = driiing(3);
            driiing2();
        }
    }
    
    public void testInnerClass() {
        new InnerTestClass().hello();
    }
//
//    public Test() {
//    }
//
    Test(Test t) {
        driiing(2);
    }

//    private void methodWithInnerClass() {
//      Runnable runnable = new Runnable() {
//            public void run() {
//                b();
//            }
//        };
//        runnable.run();
//    }
//
//    void innerClassCall() {
//        new InnerTestClass().hello();
//    }
//
//    private void methodWithParams(int a, float b, boolean c) {
//    }
//
//    static private void staticMethodWithParams(int a, float b, boolean c) {
//    }
//
    private int[] testMethodArrayReturn() {
        return new int[4];
    }
    
    protected final String methodWithException() throws IOException, XMLStreamException {
        return null;
    }
    
    private void a() {
        System.out.println("a");
    }

    private void b() {
        System.out.println("b");
    }

    private void c() {
        System.out.println("c");
    }

    private void d() {
        System.out.println("d");
    }
    
    public void testIf(int a) {
        if (a > 1) {
            a();
        }
        b();
    }
        
   public void testIfEt(int a) {
        if (a > 1 && a <= 3) {
            System.out.println("a");
        }
        System.out.println("b");
    }
    
    public void testIfOu(int a) {
        if (a > 1 || a <= 3) {
            System.out.println("a");
        }
        System.out.println("b");
    }
     
    public void testIfEtOu(int a, int b) {
        if (a > 1 || (a <= 3 && b != 67)) {
            System.out.println("a");
        }
        System.out.println("b");
    }
    
    public boolean testNot(boolean a) {
        return !a;
    }
    
    public void testIfElse(int a) {
        if (a > 1) {
            a();
        } else {
            b();
        }
    }

    public void testIfElse2(int a) {
        if (a > 1) {
            a();
        } else {
            b();
        }
        c();
    }

    public void testNestedIfElse(int a, int b) {
        if (a > 1) {
            if (b <= 3) {
                a();
            } else {
                c();
            }
        } else {
            b();
        }
    }

    public void testIfElseReturn(int a) {
        if (a > 1) {
            a();
            return;
        } else {
            b();
        }
        c();
    }
      
    public void testNestedIfElseReturn(int a, int e) {
        if (a > 1) {
            a();
            return;
        } else {
            if (e == 3) {
                b();
                return;
            } else {
                c();
                return;
            }
        }
    }

    public int testTernaryOperator(int a) {
        return (a > 1 ? 1 : 3);
    }

    public void testTernaryOperator2(int a) {
        System.out.println("before");
        int ee = 1+ (a > 1 ? 1 : 3);
        System.out.println("after");
    }

    public int testNestedTernaryOperator(int a, int b) {
        return (a > 1 ? (b == 3 ? 1 : 6) : 3);
    }

    public int testComplexeTernaryOperator(int a) {
        int z = 3;
        return (a > 1 ? (z = 1) + 2  + new int[2].length : z++);
    }

    public void testIfElseWithTryCatch(int z) {
        if (z == 1) {
            System.out.println("a");
        } else {
            try {
                System.out.println("b");
            } catch(Exception exc) {
                System.out.println("c");
            }
        }
    }

    public void testFinally() {
        try {
            System.out.println("a");
        } finally {
            System.out.println("b");
        }
    }
    
    public void testComplexeFinally(int a) {
        try {
            System.out.println("a");
        } finally {
            if (a == 1) {
                System.out.println("b");
            } else {
                System.out.println("c");
            }
        }
    }
    
    public void testNestedFinally() {
        try {
            try {
                a();
            } finally {
                c();
            }
        } finally {
            b();
        }
    }

    public void testFinallyWithBreak() {
        int v = 1;
        for (int i = 0; i < 3; i++) {
            try {
                a();
                if(v == 1) {
                    try {
                        System.out.println("zzz");
                    } finally {
                        System.out.println("ssss");
                    }
                    break;
                }
                c();
            } finally {
                b();
            }
        }
    }

    public void testFinallyWithBreak1() {
        int v = 1;
        while (true) {
            try {
                a();
                if(v == 1) {
                    try {
                        System.out.println("zzz");
                    } finally {
                        System.out.println("ssss");
                    }
                    break;
                }
                c();
            } finally {
                b();
            }
        }
    }

    public void testFinallyWithBreak2() {
        int v = 1;
        try {
          while (true) {
                a();
                if(v == 1) {
                    try {
                        System.out.println("zzz");
                    } finally {
                        System.out.println("ssss");
                    }
                    break;
                }
                c();
            }
        } finally {
            b();
        }

    }
   
    public void testNestedIfWithBreak(int a, int b) {
        for (int i = 0; i < 10; i++) {
            if (a == 1) {
                System.out.println("a");
                if (b == 2) {
                    System.out.println("b");
                    break;
                } else {
                    System.out.println("c");
                }
            }
            System.out.println("d");
        }
    }
   
    public void testCatch() {
        try {
            a();
        } catch(Exception exc) {
            System.out.println(exc);
            b();
        }
    }

    public void testDoubleCatch() {
        try {
            a();
        } catch(IllegalStateException exc) {
            b();
        } catch(Exception exc) {
            c();
        }
    }
    
    public void testNestedCatch() {
        try {
            System.out.println("a");
            try {
                System.out.println("b");
            } catch(Exception exc) {
                System.out.println("c");
            }
        } catch(Exception exc) {
            System.out.println("d");
        }
    }

    public void testDoWhile() {
        int a = 2;
        do {
            a();
        } while(a > 3);
    }
      
    public void testDoWhileBreak() {
        int a = 2;
        do {
            if (a == 2) {
                a();
                break;
            }
            if (a == 3) {
                b();
                break;
            }
        } while(a > 3);
        c();
    }
      
    public void testWhileWithCond() {
        int a  = 0;
        while(a > 3) {
            a();
        }
    }
    
    public void testWhile() {
        while(true) {
            a();
        }
    }

    public void testNestedWhile() {
        while(true) {
            a();
            while(true) {
                b();
            }
        }
    }

    public void testFor() {
        for (int i = 0; i < 3; i++) {
            a();
        }
    }

    public void testNestedFor() {
        for (int i = 0; i < 3; i++) {
            a();
            for (int j = 0; j < 4; j++) {
                b();
            }
            c();
        }
        d();
    }

    public void testNestedForWithBreak() {
        for (int i = 0; i < 3; i++) {
            a();
            for (int j = 0; j < 4; j++) {
                b();
                if(j == 2) {
                    break;
                }
            }
        }
    }

    public void testNestedForWithContinue() {
        for (int i = 0; i < 3; i++) {
            a();
            for (int j = 0; j < 4; j++) {
                b();
                if(j == 2) {
                    continue;
                }
            }
        }
    }

    public void testBreak() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                break;
            }
            a();
        }
    }

    public void testBreak1() {
        for (int i = 0; i < 3; i++) {
            b();
            if (i == 1) {
                break;
            }
            a();
        }
    }

    public void testBreak2() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                break;
            }
            a();
        }
        b();
    }

    public void testBreak3() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                break;
            }
            a();
        }
        b();
        for (int i = 0; i < 5; i++) {
            if (i == 2) {
                break;
            }
            c();
        }
        d();
    }

    public void testBreak4() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                System.out.println("a");
                break;
            }
            System.out.println("b");
        }
    }

    public void testBreakLabel() {
        TUTU: for (int i = 0; i < 3; i++) {
            a();
            for (int j = 0; j < 4; j++) {
                b();
                if(j == 2) {
                    break TUTU;
                }
            }
            c();
        }
        d();
    }

    public void testBreakLabel2() {
        LABEL1: {
            LABEL2: {
                int i = 0;
                while (true) {
                    if (i == 2) {
                        break;
                    }
                    if (i == 3) {
                        break LABEL2;
                    }
                    i++;
                }
                a();
                break LABEL1;
            }
            b();
        }
    }

    public void testContinue() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                continue;
            }
            a();
        }
    }

    public void testContinue2() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                continue;
            }
            a();
        }
        b();
    }

    public void testContinueLabel() {
        TUTU: for (int i = 0; i < 3; i++) {
            a();
            for (int j = 0; j < 4; j++) {
                b();
                if(j == 2) {
                    continue TUTU;
                }
            }
        }
        c();
    }

    public void testSwitch(int a) {
        switch (a) {
            case 0:
                a();
                break;
            case 1:
                b();
                break;
            default:
                c();
                break;
        }
    }

    public void testSwitchReturn(int a) {
        switch (a) {
            case 0:
                a();
                return;
            case 1:
                b();
                return;
            default:
                c();
        }
    }

    public void testSwitchThrow(int a) {
        switch (a) {
            case 0:
                throw new IllegalArgumentException();
                
            case 1:
                System.out.println("b");
                break;
        }
    }

    public void testSwitchInfiniteLoop(int a) {
        switch (a) {
            case 0:
                while(true);
                
            case 1:
                System.out.println("b");
                break;
        }
    }

    public void testSwitchWithEmptyDefaultCase(int a) {
        System.out.println("a");
        switch (a) {
            case 0:
                System.out.println("b");
                break;
        }
        System.out.println("c");
    }
    
    public void testSwitchWithEmptyCase(int a) {
        System.out.println("a");
        switch (a) {
            case 0:
                System.out.println("b");
                break;
            case 1:
                break;
        }
        System.out.println("c");
    }
   
    public void testFallthroughSwitchEmptyBlock(int a) {
        switch (a) {
            case 0:
            case 1:
                b();
                break;
            default:
                c();
                break;
        }
    }

    public void testFallthroughSwitchNonEmptyBlock(int a) {
        switch (a + 3) {
            case 0:
                System.out.println("a");
            case 1:
                System.out.println("b");
                break;
            case 2:
                System.out.println("c");
            case 3:
                System.out.println("d");
            case 4:
                System.out.println("e");
                break;

            default:
                System.out.println("f");
                break;
        }
    }
    
    public void testSwitchTryCatch(int a) {
        try {
            switch (a) {
                case 0:
                    try {
                        System.out.println("a");
                    } catch (Exception exc) {
                        System.out.println("e");
                    }
                    System.out.println("f");
                    break;
                case 1:
                    System.out.println("b");
                    break;
                default:
                    System.out.println("c");
                    break;
            }
        } catch (Exception exc) {
            System.out.println("d");
        }
    }
   
    public void testIntArrayAlloc() {
        int[] a = new int[3];
    }

    public void testIntArrayAlloc2() {
        int i = 3;
        int[] a = new int[i * 4 + 6];
    }

    public void testFloatArrayAlloc() {
        float[] a = new float[3];
    }

    public void testObjectArrayAlloc() {
        String[] a = new String[3];
    }
 
    public void testIntArrayLength() {
        int[] a = new int[3];
        int b = a.length;
    }
 
    public void testVarDelcIfElse() {
        int a = 2;
        if (a > 1) {
            int b = 1;
            System.out.println(b);
        } else {
            int c = 4;
            System.out.println(c);
        }
    }

//    public void testSynchronized() {
//        final Object lock = new Object();
//        synchronized (lock) {
//            a();
//        }
//        b();
//    }

    public void testAllocation() {
        String s = new String("sss");
    }

    public void testInvokation(String s) {
        s.length();
    }

    public void testPutStaticField() {
        TOTO.a = 3;
    }
    
    public void testGetField() {
        TOTO t = new TOTO();
        float u = t.b;
    }

    public void testGetFieldFromThis() {
        float u = ee;
    }
    
    public void testPutField() {
        TOTO t = new TOTO();
        t.b = 3.4f;
    }

    public void testThrow() {
        throw new IllegalArgumentException("hello");
    }
    
    public void testThrow2() {
        IllegalStateException exc = new IllegalStateException("hello");
        int a = 1;
        throw exc;
    }
    
    public void testThrow3(int a) {
        System.out.println("a");
        try {
            if (a == 1) {
                throw new IllegalStateException();
            }
            System.out.println("b");
        } catch (Throwable t) {
            System.out.println("c");
        }
        System.out.println("d");
    }

    public void testInstanceOf() {
        String a = "sss";
        if (a instanceof String) {
            System.out.println("yes");
        }
    }

    public void testInstanceOf2() {
        String[] a = new String[2];
        a[0] = "aaa";
        a[1] = "bbb";
        if (a instanceof String[]) {
            System.out.println("yes");
        }
    }
    
    public void testAssertStmt() {
        int a = 1;
        assert a == 1;
    }
    
    public void testIntToString() {
        int a = 1;
        System.out.println("hello " + a);
    }
    
    public void testCast() {
        Object s = new String("aaa");
        System.out.println(((String)s).charAt(0));
    }

    public void testCast2() {
        Object[] s = new String[2];
        System.out.println((String[])s);
    }

    public void testArray() {
        int i = 4;
        int[] a = new int[3 + i];
        float[] b = new float[37];
        double[] c = new double[334];
        char[] d = new char[3 * i + 6];
        byte[] e = new byte[5];
        long[] f = new long[5];
        String[] g = new String[1];
        short[] h = new short[1];
        a[0] = 4;
        b[0] = 4.3f;
        c[0] = 4.6;
        d[0] = 's';
        e[0] = 4;
        f[0] = 4;
        g[0] = "hello";
        h[0] = 4;
        int zz = a[2];
    }
    
    void testArrayInitializer() {
        String[] a = { "a" , "b" };
    }
    
    int testMinusOp(int a) {
        return 3 * -(a + 1) + 5;
    }
    
    void testInc() {
        int a = 0;
        a++;
        a += 2;
        a = a + 3;
    }

    void testDec() {
        int a = 0;
        a--;
        a -= 17;
    }
    
    void testPostInc() {
       int a = 0;
       int b = a++;
    }
    
    void testPreInc() {
       int a = 0;
       int b = ++a;
    }
    
    void testIterator() {
        List<Integer> v = new ArrayList<Integer>(3);
        v.add(0);
        v.add(1);
        v.add(2);
        for (int i : v) {
            System.out.println(i);
        }
        for (Integer i : v) {
            System.out.println(i);
        }
    }
    
    void testSynchronized() {
        final Object lock = new Object();
        System.out.println("a");
        synchronized (lock) {
            System.out.println("b");
        }
        System.out.println("c");
    }
    
    void testPrimitiveTypeCast() {
        float f = 3.45577f;
        int i = (int)f;
        short s = (short) i;
        char c = (char) i;
        double d = 5556.6788d;
        f = (float) d;
        System.out.println(i);
    }
    
    int testRemainder(int i) {
        return i % 3;
    }
    
    boolean testXor(boolean a, boolean b) {
        return a ^ b;
    }
    
    int testShiftLeft(int a) {
        return a << 3;
    }

    int testShiftRight(int a) {
        return a >> 3;
    }

    int testLogicalShiftRight(int a) {
        return a >>> 3;
    }
    
    void testMultiArray(int size) {
        int[][][] a = new int[4][4][4 * size + 1];
        String[][] s = new String[34][6];
    }

    void testMultiArrayInitializer() {
        int[][] a = { {2, 2}, {2, 2} };
    }
    
//    public static void main(String[] args) {
//        new Test().testBreakLabel2();
//    }
}

class TOTO {
    static int a;
    float b;
}
