
public class A {
  public int m(java.io.FileInputStream x) {
    int y;
    try {
      y = x.read();
    } catch (java.io.IOException e) {
      y = 42;
    }
    return y;
  }
}

