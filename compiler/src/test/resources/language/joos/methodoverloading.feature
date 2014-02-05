
public class A {
  public int m1(int x) {
    return 42;
  }
  public int m1(boolean x) {
    return 87;
  }
  public int m2() {
    return this.m1(true);
  }
}

