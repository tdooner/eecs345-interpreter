description: Static variables overwriting when extended
value: 1155

class A {
  static var x = 49*5*9;
  static var y = 7*25*3;

  static gcd(a,b) {
    if (a < b) {
      var temp = a;
      a = b;
      b = temp;
    }
    var r = a % b;
    while (r != 0) {
      a = b;
      b = r;
      r = a % b;
    }
    return b;
  }

  static main() {
    return gcd(x, y);
  }
}

class Main extends A {
  static var y = super.y * 121;
  static var x = super.x * 11;

  static main() {
    return gcd(x,y);
  }
}
