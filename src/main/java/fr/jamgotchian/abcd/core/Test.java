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

    private Runnable run;

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

//        void hello2() {
//            int a = driiing(3);
//            driiing2();
//        }
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

//        void hello2() {
//            int a = driiing(3);
//            driiing2();
//        }
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

    protected final java.lang.String methodWithException()
            throws IOException, XMLStreamException, NullPointerException {
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

    public void testIfBool(boolean a) {
        if (a) {
            a();
        }
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

    void testOrWithAssign(int a, int b) {
        int c = -1;
        if (a == 1 || (c = a) == 2) {
            System.out.println("a");
        }
        System.out.println(c);
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

    public int testTernaryOperatorLogical(int a, float b, boolean c) {
        return (a > 1 && b == 3 ? 1 : 3);
    }

    int testSerialTernaryOperators(int a, int b) {
        return (a > 1 ? 2 : 3) + (b == 4 ? 67 : 334);
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

    public void testFinallyWithBreak0(int a) {
        System.out.println("a");
        for (int i = 0; i < 3; i++) {
            try {
                System.out.println("b");
            } finally {
                System.out.println("c");
                if (i == a) {
                    System.out.println("d");
                    break;
                }
                System.out.println("e");
            }
            System.out.println("f");
        }
        System.out.println("g");
    }

    public void testFinallyWithContinue0(int a) {
        System.out.println("a");
        for (int i = 0; i < 3; i++) {
            try {
                System.out.println("b");
            } finally {
                System.out.println("c");
                if (i == a) {
                    System.out.println("d");
                    continue;
                }
                System.out.println("e");
            }
            System.out.println("f");
        }
        System.out.println("g");
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

    public void testNestedTryCatchReturn() {
        try {
            try {
                System.out.println("a");
            } catch (IllegalAccessError exc) {
                System.out.println("b");
                return;
            }
        } catch (Exception exc) {
            System.out.println("c");
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

//    public void testInfiniteLoop() {
//        while(true) {
//            a();
//        }
//    }
//
//    public void testInfiniteLoop2() {
//        while(true) {
//            a();
//            while(true) {
//                b();
//            }
//        }
//    }

//    public void testInfiniteLoop3(int a) {
//        for (int i = 0; i < a; i++) {
//            a();
//            while(true) {
//                b();
//            }
//        }
//        c();
//    }

    public void testFor() {
        for (int i = 0; i < 3; i++) {
            a();
        }
    }

    public void testForReturn(int a) {
        if (a < 4) {
            for (int i = 0; i < 3; i++) {
                System.out.println("a");
                if (i == 2) {
                    System.out.println("b");
                    return;
                }
                System.out.println("c");
            }
        }
        System.out.println("c");
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

    public void testForWithBreak(int i) {
        for (int j = 0; j < 4; j++) {
            a();
            if(j == 2) {
                b();
                if (i == 4) {
                    break;
                }
            }
            c();
        }
        d();
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

    public void testForIfReturn() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                System.out.println("a");
                return;
            }
            System.out.println("b");
        }
        System.out.println("c");
    }

    public void testForIfThrow() throws ClassCastException {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                System.out.println("a");
                throw new ClassCastException("aaaa");
            }
            System.out.println("b");
        }
        System.out.println("c");
    }

    public void testBreak() {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                break;
            }
            a();
        }
    }

    public void testWhileTrue(int a) {
        System.out.println("a");
        while (true) {
            System.out.println("b");
            if (a == 1) {
                System.out.println("c");
                break;
            }
            System.out.println("d");
        }
        System.out.println("e");
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

    public void testContinue(int j) {
        for (int i = 0; i < 3; i++) {
            if (i == 1) {
                a();
                if (j == 4) {
                    continue;
                }
                b();
            }
            c();
        }
        d();
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

    public void testContinue3(int j) {
        for (int i = 0; i < 3; i++) {
            System.out.println("a");
            if (i == 1) {
                System.out.println("b");
                if (j == 4) {
                    continue;
                }
                System.out.println("b");
            } else {
                System.out.println("d");
                if (j == 5) {
                    continue;
                }
                System.out.println("e");
            }
            System.out.println("f");
        }
        System.out.println("g");
    }

    public void testContinueLabel() {
        TUTU: for (int i = 0; i < 3; i++) {
            a();
            for (int j = 0; j < 4; j++) {
                b();
                if(j == 2) {
                    continue TUTU;
                }
                c();
            }
        }
        d();
    }

    public int foo(int i, int j) {
        while (true) {
            try {
                while (i < j)
                    i = j++/i;
            }
            catch (RuntimeException re) {
                i = 10;
                continue;
            }
            break;
        }
        return j;
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

    public void testSwitchOneReturn(int a) {
        for (int i = 0; i < 13; i++) {
            switch (a) {
                case 0:
                    System.out.println("a");
                    return;
                case 1:
                    System.out.println("b");
                    break;

                default:
                    System.out.println("c");
                    break;
            }
            System.out.println("d");
        }
        System.out.println("e");
    }

    public void testSwitchOneReturn2(int a) {
        for (int i = 0; i < 13; i++) {
            switch (a) {
                case 0:
                    System.out.println("a");
                    return;
                case 1:
                    System.out.println("b");
                    break;

                default:
                    System.out.println("c");
                    break;
            }
        }
        System.out.println("e");
    }

    public void testSwitchAllReturnExceptOne(int a) {
        for (int i = 0; i < 13; i++) {
            switch (a) {
                case 0:
                    System.out.println("a");
                    return;
                case 1:
                    System.out.println("b");
                    break;

                default:
                    System.out.println("c");
                    return;
            }
            System.out.println("d");
        }
        System.out.println("e");
    }

    public void testSwitchIfReturn(int a) {
        for (int i = 0; i < 13; i++) {
            switch (a) {
                case 0:
                    if (a == 3) {
                        System.out.println("a");
                        return;
                    }
                    break;
                case 1:
                    System.out.println("b");
                    break;

                default:
                    System.out.println("c");
                    break;
            }
            System.out.println("d");
        }
        System.out.println("e");
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

//    public void testSwitchInfiniteLoop(int a) {
//        switch (a) {
//            case 0:
//                while(true);
//
//            case 1:
//                System.out.println("b");
//                break;
//        }
//    }

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
            case 5:
                System.out.println("f");
                return;
            default:
                System.out.println("g");
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
        java.lang.String s = new java.lang.String("sss");
    }

    public void testInvokation(java.lang.String s) {
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
        java.lang.String a = "sss";
        if (a instanceof java.lang.String) {
            System.out.println("yes");
        }
    }

    public void testInstanceOf2() {
        java.lang.String[] a = new java.lang.String[2];
        a[0] = "aaa";
        a[1] = "bbb";
        if (a instanceof java.lang.String[]) {
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
        Object s = new java.lang.String("aaa");
        System.out.println(((java.lang.String)s).charAt(0));
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
        java.lang.String[] g = new java.lang.String[1];
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
        java.lang.String[] a = { "a" , "b" };
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

    void testLongFloatDoubleComparison() {
        for (long j = 0; j < 3; j++) {
            System.out.println(j);
        }
        for (float j = 0; j < 3; j++) {
            System.out.println(j);
        }
        for (double j = 0; j < 3; j++) {
            System.out.println(j);
        }
        double a = 1;
        if (a > 3.45) {
            System.out.println("ss");
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
        java.lang.String[][] s = new java.lang.String[34][6];
    }

    void testMultiArrayInitializer() {
        int[][] a = { {1, 2}, {3, 4} };
    }

    int function(float e) {
        return (int)e +1;
    }

    void testVoidMethodCall(float e) {
        System.out.println("a");
        function(e);
        System.out.println("b");
    }

    void testMethodCall() {
        System.out.println(function(1.334f));
    }

    Class<?> testClassType() {
        return String.class;
    }

    int[] testReturnArray() {
        return new int[] { 1, 2};
    }

    int[][] testReturnDoubleArray() {
        return null;
    }

    String[][] testReturnDoubleString() {
        return null;
    }

    void testTypeInference() {
        StringBuilder builder = new StringBuilder(4);
        char c = 'e';
        char d = c;
        builder.append(c);
        System.out.println(d);
    }

    void testPhiFunc(int c) {
        int a = 0;
        if (c == 3) {
            a = 3;
            System.out.println("then");
        } else {
            System.out.println("else");
        }
        System.out.println(a);
    }

    void testPhiFunc2(int c) {
        int a = 0;
        if (c == 3) {
            a = 3;
            System.out.println("then");
        } else {
            System.out.println("else");
        }
    }

    void testPhiFunc3(int c) {
        int a = 0;
        if (c == 3) {
            a = 3;
            System.out.println("then");
        } else {
            a = 4;
            System.out.println("else");
        }
        System.out.println(a);
    }

    void testSSA() {
        for (int i = 0; i < 3; i++) {
            System.out.println(i);
        }
        for (float f = 0; f < 3; f++) {
            System.out.println(f);
        }
    }

    void testType() {
        int a = 3;
        float b = a;
        int c = a + 2;
        System.out.println((double)b);
    }

    void testType2() {
        int a = 3;
        int b = a + 2;
    }

    void testCharTypeInference() {
        char c = 'z';
        StringBuilder builder = new StringBuilder();
        builder.append(c);
    }

    private static class A {
    }

    private static class B {
    }

    private static class C extends A {
    }

    private static class D extends A {
    }

    void testType3(int x) {
        Object c = null;
        if (x == 1) {
            c = new A();
        } else {
            c = new B();
        }
        System.out.println(c);
    }

    void testType4(int x) {
        A a = null;
        if (x == 1) {
            a = new C();
        } else {
            a = new D();
        }
        System.out.println(a);
    }

    int testType5() {
        char a = 'e';
        return a;
    }

    void testTypeArray(int x) {
        A[] a = null;
        if (x == 45) {
            a = new C[3];
        } else {
            a = new D[3];
        }
        Object o = a;
        Object[] ee = a;
        System.out.println(a[1]);
        Object bb = new float[2][5];
        A[][] zzz = new C[2][5];
    }

    class String {

    }

    protected final void testImport() {
        fr.jamgotchian.abcd.core.Test.String a = new fr.jamgotchian.abcd.core.Test.String();
        java.lang.String b = new java.lang.String();
    }

    void testCallStaticMethod(int rrr) {
        TOTO.aaa(rrr * 4);
    }

//    public static void main(String[] args) {
//        new Test().testBreakLabel2();
//    }

}

class TOTO {
    static int a;
    float b;

    static int aaa(int a_int) {
        return -1 + a_int;
    }
}
