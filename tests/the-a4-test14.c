description: Basic static method usage
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
