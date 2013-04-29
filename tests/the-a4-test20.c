description: Calling static methods when not declared in order
value: 615

class A {
  static var a = 1;
  static var b = 10;

  static setA(x) {
    a = x;
  }

  static getSum() {
    return a + b;
  }
}

class Main {
  static main() {
    A.setA(5);

    return A.getSum() + C.x + C.timesX(A.a);
  }
}

class C {
  static var x = 100;
  static timesX(a) {
    return a * x;
  }
}
