description: Function overloading with different call-by-type
value: 7

static something(&x) {
  x = 4;
  return x;
}

static something(x, y) {
  return x + y;
}

static main() {
  var x = 3;
  something(x);
  return something(1, 2) + x;
}
