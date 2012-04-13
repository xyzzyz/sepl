int exprs(int a, int b; locals int c, int d, bool x, bool y, bool z; arrays;) {
  a = a + a;
  b = a - b * c;
  c = c * c / d;
  a = b = c;
  c = a - (b = a);
  x = !x;
  x = !(!y);
  z = !x && y;
  x = !z || y;
  y = y ^^ z;
};
