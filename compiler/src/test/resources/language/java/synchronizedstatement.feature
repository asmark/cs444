
public class A {
  public int x;
  public void m() {
    synchronized(x) {
      x = x-1;
    }
}

