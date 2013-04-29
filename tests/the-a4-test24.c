description: Method overloading
value: 530

class Main {
  static var x = 10;
  static var y = 20;

  static add(a, b) {
    return a + b;
  }

  static add(a,b,c) {
    return a + b + c;
  }

  static main() {
    return Main.add(x, y) + Main.add(x, y, y) * 10;
  }
}
