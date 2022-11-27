int abs(int x) { return x < 0 ? -x : x; }

int main() {
  int x = 5;
  int y = x << 2;
  int z = abs(y + x);
  return z;
}
