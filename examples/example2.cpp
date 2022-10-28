int inc(int y) {
  return y + 1;
}

int main() {
    int x = 5;
    int y = x << 2;
    int z = inc(y + x);
    return z;
}
