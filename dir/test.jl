int main(){
    int[] xs = new int[5];
    for (int x : xs) printInt(x);
    xs[xs.length-1] = 5;
    return xs[0];
}

