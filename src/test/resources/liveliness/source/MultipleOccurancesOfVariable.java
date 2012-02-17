class MultipleOccurancesOfVariable {
    int x;
    public void m(int n) {
        if (n >= 0) {
            x += 1;
        } else {
            x -= 1;
        }
        return x;
    }
}