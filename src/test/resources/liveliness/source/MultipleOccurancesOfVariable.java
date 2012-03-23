class MultipleOccurancesOfVariable {

  int x;

  int test(int n) {
    if (n >= 0) {
      this.x += 1;
    } else {
      this.x -= 1;
    }
    return x;
  }
}