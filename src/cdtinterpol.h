#include <R.h>
#include <Rmath.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>

void F77_SUB(dposv_solve)(double *a, double *b, int *n, int *nrhs, int *info);
void F77_SUB(dsysv_solve)(double *a, double *b, int *n, int *nrhs, int *info);
void F77_SUB(dgesv_solve)(double *a, double *b, int *n, int *nrhs, int *info);
