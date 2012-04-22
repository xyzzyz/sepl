void print_int_helper(int a; locals int i, int j; arrays [int] out;) {
  if(a == 0) {
    out[0] = 0;
    i = i+1;
  } else {
    while(a > 0) {
      out[i] = a % 10;
      a = a / 10;
      i = i + 1;
    };
  };
  j = 0;
  while(j < i) {
    output out[i - j - 1] + 48;
    j = j + 1;
  };
  return;
};

void print_int(int a; locals ; arrays ;) {
  print_int_helper(a; arrays 20);
  return;
};

void hello(int a; locals int i; arrays [int] arr ;) {
  arr = "Hello, World\n";
  i = 0;
  while(!(arr[i] == 0)) {
    output arr[i];
    i = i + 1;
  };
  return;
};

void main(; locals ; arrays ;) {
  hello(0; arrays 100);
  print_int(6667);
  return;
};

