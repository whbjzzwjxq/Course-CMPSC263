int symbolicI32() { return 0; }

int main() {
  int x = symbolicI32();
  int y = x << 2;
  int z = y + x;
  return z;
}
