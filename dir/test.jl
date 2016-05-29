int main () {
  int[] xs = new int[3];
  int[] ys = add1(xs);
  //printInt(ys[1]);
  
  for (int x : add1(ys)) {
    printInt(x);
  }
  return 0;
}

int[] add1 (int[] xs) {
    xs[0] = xs[0] + 1;
    xs[1] = xs[1] + 1;
    xs[2] = xs[2] + 1;
    return xs;
}

