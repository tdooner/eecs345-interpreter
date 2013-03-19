description: While loops with break and continue should work
value: 5

var i = 0;
var j = 20;
while (j > 0)
  if (j % 2 == 0)
    continue
  if (i == 5)
    break
  i = i + 1;
return i;
