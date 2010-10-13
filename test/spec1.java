

interface FacI {
  public int fac (int n);
}

class FacC implements FacI, Bar, Foo {
  public int y;

  public int fac (int n) {
    int x = new Foo();
    int bar;
    int baz;
    //String foo, bar, baz;
    //x = 1 * 2 * 3 * 4;
    if (n > 0)
      x = n * fac(n - 1);
    else
      x = 1;
    //fac(x == 1 ? 4 : x == fac(2) ? fac(fac(3)) : fac(3));
    //new Foo(bar, baz, 2);
    //y = 20;
    //this.y = 42;
    //this.yyy = this.y + this.y;
    return x;
  }
}
