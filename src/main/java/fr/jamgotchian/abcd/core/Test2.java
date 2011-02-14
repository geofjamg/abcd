package fr.jamgotchian.abcd.core;

/**
 *
 * @author Geoffroy Jamgotchian <geoffroy.jamgotchian at gmail.com>
 */
public class Test2 {

//    public void testFinallyWithBreak() {
//        int v = 1;
//        while (true) {
//            try {
//                System.out.println("a");
//                if(v == 1) {
//                    try {
//                        System.out.println("zzz");
//                    } finally {
//                        System.out.println("ssss");
//                    }
//                    break;
//                }
//                System.out.println("c");
//            } finally {
//                System.out.println("b");
//            }
//        }
//    }
  
    public void testFinallyWithBreak() {
        int v = 1;
        while (true) {
                System.out.println("a");
                if(v == 1) {
                    System.out.println("zzz");
                    System.out.println("ssss");
                    break;
                }
                System.out.println("c");
                System.out.println("b");
        }
    }
      
//    public void testIfElseWithTryCatch2(int z) {
//        System.out.println("d");
//        try {
//            if (z == 1) {
//                System.out.println("a");
//            } else {
//                System.out.println("b");
//            }
//        } catch(Exception exc) {
//            System.out.println("c");
//        }
//    }
      
//    public void testFinally(int a) {
//        try {
//            System.out.println("a");
//        } finally {
//            if (a == 1) {
//                System.out.println("b");
//            } else {
//                System.out.println("c");
//            }
//        }
//    }

//    public void testFinallyWithBreak() {
//        int v = 1;
//        while (true) {
//            try {
//                System.out.println("a");
//                if(v == 1) {
//                    try {
//                        System.out.println("zzz");
//                    } finally {
//                        System.out.println("ssss");
//                    }
//                    break;
//                }
//                System.out.println("c");
//            } finally {
//                System.out.println("b");
//            }
//        }
//    }
}