description: Mutating variable state inside if statements
value: false

var x = 5;
if (5 == (x = 4))
  return true;
else
  return false;
