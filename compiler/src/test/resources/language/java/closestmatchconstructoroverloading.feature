
public class A {
  public A() {}
  public A(Object x, Object y) {}
  public A(Object x, A y) {}
  public void m() {
    new A(new A(), new A());
  }
}

