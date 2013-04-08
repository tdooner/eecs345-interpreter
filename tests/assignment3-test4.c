description: Functions changing global variables
value: 8

var global = 3;
other() {
    global = 1;
}
main() {
  other();
  return global;
}
