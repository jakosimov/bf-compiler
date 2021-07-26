#include <stdio.h>

int kanske(int a) {
  return a * a;
}

int main(int cc, char** vc) {

  int a = 0;
  int b = 1;
  int N = 10;

  for (int i = 0 ; i < N ; i++) {
    printf(a);
    b += a;
    a = b - a;
  }


  return 0;
}
