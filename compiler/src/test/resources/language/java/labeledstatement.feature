
public class A {
  public int m(int x) {
    loop:
    while (x&gt;0) {
       x=x-1;
       if (x=87) {
         x=42;
         continue loop;
       }
    }
    return x;
  }
}

