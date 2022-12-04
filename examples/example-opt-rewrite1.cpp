int symbolicI32() { return 0; }

int main() {
  int x = symbolicI32();
  int y = symbolicI32();
  int z;
  if (x <= 128) {
    int i = x * x;
    int j = y * y;
    int k = 2 * y * x;
    z = i + k + j;
  } else {
    z = 0;
  }
  return z;
}
