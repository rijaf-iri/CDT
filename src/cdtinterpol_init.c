
#include <R_ext/RS.h>
#include <stdlib.h>
#include <R_ext/Rdynload.h>

/* .Fortran calls */
extern void F77_NAME(idw_interp)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(shepard_interp)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(barnes_interp)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(spheremap_interp)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(cressman_interp)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(kriging_interp)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(distance_vector)(void *, void *, void *, void *, void *);
extern void F77_NAME(distance_matrix)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(distance_pixel)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

#define FDEF(name, n) {#name, (DL_FUNC) &F77_NAME(name), n}
static const R_FortranMethodDef FortranEntries[] = {
	FDEF(idw_interp, 10),
	FDEF(shepard_interp, 10),
	FDEF(barnes_interp, 10),
	FDEF(cressman_interp, 9),
	FDEF(spheremap_interp, 9),
	FDEF(kriging_interp, 13),
	FDEF(distance_vector, 5),
	FDEF(distance_matrix, 6),
	FDEF(distance_pixel, 10),
	{NULL, NULL, 0}
};

void R_init_CDT (DllInfo *dll) {
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
