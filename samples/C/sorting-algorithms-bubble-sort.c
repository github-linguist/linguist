void bubble_sort(int *a, int n) {
	int j, t = 1;
	while (n-- && t)
		for (j = t = 0; j < n; j++) {
			if (a[j] <= a[j + 1]) continue;
			t = a[j], a[j] = a[j + 1], a[j + 1] = t;
			t=1;
		}
}

int main(void) {
	int a[] = {4, 65, 2, -31, 0, 99, 2, 83, 782, 1};
	bubble_sort(a, sizeof(a) / sizeof(a[0]));

	return 0;
}
