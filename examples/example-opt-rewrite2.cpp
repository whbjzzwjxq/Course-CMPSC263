int symbolicI32() { return 0; }

int main() {
  int x = symbolicI32();
  int y = symbolicI32();
  int i = x * y;
  int j = y * x;
  int k = 2 * y * x;
  int z = i + k + j;
  return z;
}
