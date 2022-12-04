int symbolicI32() { return 0; }

int main() {
  int x = symbolicI32();
  int z;
  if (x <= 128) {
    int y = x + x;
    z = (y + 2) + (y + 1) << 2;
  } else {
    z = 0;
  }
  return z;
}
