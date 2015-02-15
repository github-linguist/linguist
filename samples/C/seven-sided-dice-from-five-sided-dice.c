int rand5()
{
	int r, rand_max = RAND_MAX - (RAND_MAX % 5);
	while ((r = rand()) >= rand_max);
	return r / (rand_max / 5) + 1;
}

int rand5_7()
{
	int r;
	while ((r = rand5() * 5 + rand5()) >= 27);
	return r / 3 - 1;
}

int main()
{
	printf(check(rand5, 5, 1000000, .05) ? "flat\n" : "not flat\n");
	printf(check(rand7, 7, 1000000, .05) ? "flat\n" : "not flat\n");
	return 0;
}
