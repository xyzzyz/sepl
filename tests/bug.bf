void int_to_char(int a; locals; arrays;) {
  return;
};

void print_char(int c; locals; arrays;) {
  return;
};

void print_int_rev(int a; locals int c; arrays;) {
  output 'p';

  call int_to_char(a);
  call print_char(c);
  while(a > 0) {
    call int_to_char(1);
    call print_char(c);
  };
  return;
};

int foo(int a, bool b; locals int c; arrays;) {
  output 'f';
  return 0;
};

void init(; locals int a; arrays;) {
  output 'i';
  a = 'a';
  call foo('a', true);
  output 'o';
  return;
};

void main(; locals; arrays;) {
  call init();
  return;
};
