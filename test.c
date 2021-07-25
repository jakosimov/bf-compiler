#include <stdio.h>

int main() {

  int a = 2;
  int b = 2 * (a + 2);
  while (b / a) {
    a = a + 1;
  }

  return 0;
}
