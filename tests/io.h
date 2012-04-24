#ifndef IO_H
#define IO_H

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
    output out[i - j - 1] + '0';
    j = j + 1;
  };
  return;
};

void print_int(int a; locals ; arrays ;) {
  call print_int_helper(a; arrays 20);
  return;
};

void print_string([int] arr; locals int i; arrays;) {
  i = 0;
  while(!(arr[i] == 0)) {
    output arr[i];
    i = i + 1;
  };
  return;
};

#endif
