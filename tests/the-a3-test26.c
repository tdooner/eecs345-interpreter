description: The straggler test
value: 115

static var a = 10;
static var b = 20;

static bmethod() {
  var b = 30;
  return a + b;
}

static cmethod() {
  var a = 40;
  return bmethod() + a + b;
}

static main () {
  var b = 5;
  return cmethod() + a + b;
}

