int[] array = {1, 2, 3, 4, 5 };
List<Integer> evensList = new ArrayList<Integer>();
for (int  i: array) {
    if (i % 2 == 0) evensList.add(i);
}
int[] evens = evensList.toArray(new int[0]);
