int foo([int] a, bool b; locals int c; arrays;) {
  if(b) {
    a[c+2] = c*c;
  } else if (c < 0) {
    c = 2;
  } else {};
  return 2;
};


void main(; locals; arrays;) {
  return;
};
