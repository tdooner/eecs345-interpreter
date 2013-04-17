description: Function overloading works
value: 7

static something(x) {
  return x;
}

static something(x, y) {
  return x + y;
}

static main() {
  return something(4, 3);
}
