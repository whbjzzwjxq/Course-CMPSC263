int symbolicI32() { return 0; }

int fake_abs(int x) { return x < 0 ? x : x; }

// Path is merged by rosette automatically.

int main() {
  int x = symbolicI32();
  int y = x << 2;
  int z = fake_abs(y + x);
  return z;
}
