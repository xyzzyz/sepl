int gcd(int a, int b; locals; arrays;) {
  if(b == 0) {
    return a;
  } else {
    b = call gcd(b, a%b);
    return b;
  };
};

void print_int_rev(int a; locals int b; arrays;) {
  if(a == 0) {
    output '0';
  } else {
    while(a > 0) {
      output '0'+(a%10);
      a = a/10;
    };
  };
  return;
};

void init(; locals int a; arrays;) {
  a = call gcd(60, 25);
  call print_int_rev(a);
  return;
};

void main(; locals; arrays;) {
  call init();
  return;
};
