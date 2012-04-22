int testFun0Arg(; locals; arrays;) {
  return 0;
};


int testFun1Arg(int a; locals; arrays;) {
  return 0;
};

int testFun2Arg(int a, int b; locals; arrays;) {
  return 0;
};

int testFun1Arg1Arr(int a; locals; arrays [int] b;) {
  return 0;
};


int test(; locals int a, int b, bool c; arrays;) {
  b = 3;
  a = testFun0Arg();
  a = testFun1Arg(b);
  a = testFun2Arg(b, b);
  a = testFun1Arg1Arr(2; arrays 3*3);
  return a;
};

void main(; locals; arrays;) {
  return;
};
