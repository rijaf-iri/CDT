#include "cdtinterpol.h"

void F77_SUB(dposv_solve)(double *a, double *b, int *n, int *nrhs, int *info)
{
    int N = n[0];
    int lda = N, ldb = N;

    // Solve the equations A*X = B 
    F77_CALL(dposv)("L", n, nrhs, a, &lda, b, &ldb, info);
}

void F77_SUB(dsysv_solve)(double *a, double *b, int *n, int *nrhs, int *info)
{
    int N = n[0];
    int lda = N, ldb = N, lwork;
    int ipiv[N];
    double wkopt;
    double* work;

	// Query and allocate the optimal workspace 
    lwork = -1;
    F77_CALL(dsysv)("L", n, nrhs, a, &lda, ipiv, b, &ldb, &wkopt, &lwork, info);
    lwork = (int)wkopt;
    work = (double*) malloc(lwork * sizeof(double));
    // Solve the equations A*X = B 
    F77_CALL(dsysv)("L", n, nrhs, a, &lda, ipiv, b, &ldb, work, &lwork, info);
    free((void*) work);
}

void F77_SUB(dgesv_solve)(double *a, double *b, int *n, int *nrhs, int *info)
{
    int N = n[0];
    int lda = N, ldb = N;
    int ipiv[N];

    // Solve the equations A*X = B 
    F77_CALL(dgesv)(n, nrhs, a, &lda, ipiv, b, &ldb, info);
}
