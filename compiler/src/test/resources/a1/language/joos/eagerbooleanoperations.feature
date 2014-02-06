public class A {
  public A() {}
  public boolean m(boolean x) {
    return (x & true) | !x;
  }
}