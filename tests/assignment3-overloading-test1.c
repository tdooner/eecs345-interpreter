description: Function overloading works
value: 7

something(x) {
  return x;
}

something(x, y) {
  return x + y;
}

main() {
  return something(4, 3);
}
