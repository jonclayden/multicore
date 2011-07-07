/* performance-enhancing functions */

#define USE_RINTERNALS 1

#include <Rinternals.h>

SEXP create_list(SEXP sLength) {
    int len = Rf_asInteger(sLength);
    if (len < 1) len = 0;
    return Rf_allocVector(VECSXP, len);
}

