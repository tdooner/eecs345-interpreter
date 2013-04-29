description: Correct answer on static variables with extends
value: 4321

class A {
  static var a = 1;
  static var b = 20;
}

class B extends A {
  static var c = 300;

  static main() {
    return a + b + c + d;
  }
}

class Main extends B {
  static var d = 4000;

  static main() {
    return a + b + c + d;
  }
}
