
public class A {
  public static int m1() {
    return 42;
  }
  public int m2() {
    A a = new A();
    return a.m1();
  }
}

