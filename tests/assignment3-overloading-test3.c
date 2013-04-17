description: Function overloading works from inside functions
value: 8

static something(x) {
  return x + 1;
}

static something(x, y) {
  return something(x) + something(x, y, 1);
}

static something(x, y, z) {
  return y * 2;
}

static main() {
  return something(3, 2);
}
