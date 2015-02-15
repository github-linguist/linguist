ary = [["UK", "London"], ["US", "New York"], ["US", "Birmingham"], ["UK", "Birmingham"]]
print(ary);

ary.sort(function(a,b){return (a[1]<b[1] ? -1 : (a[1]>b[1] ? 1 : 0))});
print(ary);

/* a stable sort will output ["US", "Birmingham"] before ["UK", "Birmingham"] */
