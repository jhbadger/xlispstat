foo(n, x, sum)
     int *n;
     double *x, *sum;
{
  int i;

  for (i = 0, *sum = 0.0; i < *n; i++) {
    *sum += x[i];
  }
}
