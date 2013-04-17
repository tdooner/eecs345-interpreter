description: Function overloading works from inside functions
value: 8

something(x) {
  return x + 1;
}

something(x, y) {
  return something(x) + something(x, y, 1);
}

something(x, y, z) {
  return y * 2;
}

main() {
  return something(3, 2);
}
