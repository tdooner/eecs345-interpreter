description: Basic class instantiation
value: 4

class A {
  var x = 4;

  result() {
    return x;
  }
}

class Main {
  static main() {
    var b = new A();
    return b.result();
  }
}
