description: function dots with call by reference params
value: 20

class A {
  static negate(&y) {
    y = -y;
  }
}

class Main {
  static main() {
    var x = -20;
    A.negate(x);
    return x;
  }
}
