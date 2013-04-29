description: Error on unknown variables with extends
value: ERROR

class A {
  static var a = 1;
  static var b = 20;
}

class Main extends A {
  static var c = 300;

  static main() {
    return a + b + c + d;
  }
}

class C extends Main {
  static var d = 4000;

  static main() {
    return a + b + c + d;
  }
}
