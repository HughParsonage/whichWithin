#include "whichWithin.h"


int char12_to_int(char x[]) {
  int o = 0;
  int ten = 1;
  for (int i = 11; i >= 4; --i) {
    o += ten * (x[i] - '0');
    ten *= 10;
  }
  return o;
}

int char2int(char x[], int s) {
  int o = 0;
  int ten = 1;
  for (int i = s - 1; i >= 0; --i) {
    o += ten * (x[i] - '0');
    ten *= 10;
  }
  return o;
}

// [[Rcpp::export(rng = false)]]
IntegerVector Char2Int(CharacterVector x) {
  IntegerVector out = no_init(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = char2int(x[i], x[i].size());
  }
  return out;
}

// [[Rcpp::export(rng = false)]]
IntegerVector EncodeP(CharacterVector x) {
  R_xlen_t N = x.length();
  IntegerVector out = no_init(N);
  
  for (R_xlen_t i = 0; i < N; ++i) {
    int nchar = x[i].size();
    
    bool starts_with_3202 = (nchar == 12) && x[i][0] == '3' && x[i][1] == '2' && x[i][2] == '0' && x[i][3] == '2';
    
    if (starts_with_3202) {
      out[i] = char12_to_int(x[i]);
    } else {
      out[i] = char2int(x[i], nchar);
    }
    
  }
  
  return out;
}
