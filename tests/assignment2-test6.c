description: While loops with continue should work
value: 5

var i = 0;
var j = 10;
while (j > 0)
  if (j % 2 == 0)
    continue
  i = i + 1;
return i;
