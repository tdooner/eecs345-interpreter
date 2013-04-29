description: Basic extends code (part A)
value: 30

class Main {
  static var x = 10;
  static var y = 20;

  static add(a, b) {
    return a + b;
  }

  static main() {
    return Main.add(Main.x, y);
  }
}

class B extends Main {
  static var y = 200;
  static var z = 300;

  static main() {
    return add(x+y,z);
  }
}
