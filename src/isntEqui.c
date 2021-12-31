#include "whichWithin.h"

void checkEquiRealReal(SEXP x, SEXP y, const char * nx, const char * ny) {
  if (!isReal(x)) {
    error("`%s` was type '%s' but must be type double.", nx, type2char(TYPEOF(x)));
  }
  if (!isReal(y)) {
    error("`%s` was type '%s' but must be type double.", ny, type2char(TYPEOF(y)));
  }
  if (xlength(x) != xlength(y)) {
    error("`length(%s) = %lld` yet `length(%s) = %lld`",
          nx, xlength(x), ny, xlength(y));
  }
}

void checkEquiIntInt(SEXP x, SEXP y, const char * nx, const char * ny) {
  if (!isInteger(x)) {
    error("`%s` was type '%s' but must be type integer.", nx, type2char(TYPEOF(x)));
  }
  if (!isInteger(y)) {
    error("`%s` was type '%s' but must be type integer.", ny, type2char(TYPEOF(y)));
  }
  if (xlength(x) != xlength(y)) {
    error("`length(%s) = %lld` yet `length(%s) = %lld`",
          nx, xlength(x), ny, xlength(y));
  }
}
