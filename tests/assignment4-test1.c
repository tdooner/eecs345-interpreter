description: Example from assignment
value: 400

class Rectangle {
  static var width = 10;
  static var height = 12;

  static area() {
    var a = width * height;
    return a;
  }

  static setSize(x, y) {
    width = x;
    height = y;
  }
}

class Main extends Rectangle {
  static setSize(x) {
    super.setSize(x, x);
  }

  static main() {
    setSize(20);
    return area();
  }
}
