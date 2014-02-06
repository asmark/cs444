
public class A {
  public int m1(Object x, Object y) {
    return 42;
  }
  public int m1(Object x, A y) {
    return 87;
  }
  public int m2() {
    return this.m1(new A(), new A());
  }
}

