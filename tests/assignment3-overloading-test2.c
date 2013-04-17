description: Function overloading with different call-by-type
value: 7

something(&x) {
  x = 4;
  return x;
}

something(x, y) {
  return x + y;
}

main() {
  var x = 3;
  something(x);
  return something(1, 2) + x;
}
