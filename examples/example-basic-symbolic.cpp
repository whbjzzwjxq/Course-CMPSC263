int symbolicI32() { return 0; }

int main() {
  int x = symbolicI32();
  if (x >= 128) {
    int z = x + 256;
    return z;
  } else {
    int y = x << 2;
    return y;
  }
}
