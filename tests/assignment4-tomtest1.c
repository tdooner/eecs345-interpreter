description: Basic class extensions work
value: 5

class A {
  value() {
    return 5;
  }
}
class Main extends A{
  static main() {
    return value();
  }
}
