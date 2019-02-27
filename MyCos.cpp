#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double sumSquared(NumericVector x) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i]*x[i];
  }
  return total;
}

// [[Rcpp::export]]
double sumProd(NumericVector x, NumericVector y) {
  int n = x.size();
  double total = 0;
  for(int i = 0; i < n; ++i) {
    total += x[i]*y[i];
  }
  return total;
}

// [[Rcpp::export]]
double myCos(NumericVector x, NumericVector y) {
  double a = sumProd(x,y);
  double b = sqrt(sumSquared(x));
  double c = sqrt(sumSquared(y));
  double cos = a/(b*c);
  return cos;
}

// [[Rcpp::export]]
NumericMatrix applyCos(List x_, List y_) {
  List x = clone(x_);
  List y = clone(y_);
  NumericVector maxVec = NumericVector::create(-2.0, -2.0);
  NumericMatrix response(x.size(),2);
  double tmp = -1;
  for( List::iterator i = x.begin(); i != x.end(); ++i ) {
    for( List::iterator j = y.begin(); j != y.end(); ++j ) {
      NumericVector tmp1 = as<NumericVector>(*i);
      NumericVector tmp2 = as<NumericVector>(*j);
      
      tmp = myCos(tmp1, tmp2);
      if(tmp > maxVec[1]){
        maxVec[0] = j.index() + 1;
        maxVec[1] = tmp;
      }
    }
    response(i.index(),0) = maxVec[0];
    response(i.index(),1) = maxVec[1];
    maxVec = NumericVector::create(-2.0, -2.0);
  }
  
  return response;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// /*** R
// # A = list(a = c(1,2,3), b = c(1,2,4))
// # B = list(a = c(1,2,4), b = c(1,2,3))
// similarityMatrix = applyCos(encWordToVecList,cboWordToVecList)
// */
