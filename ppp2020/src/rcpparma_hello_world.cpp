// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// simple example of creating two matrices and
// returning the result of an operatioon on them
//
// via the exports attribute we tell Rcpp to make this function
// available from R
//
// [[Rcpp::export]]
arma::mat rcpparma_hello_world() {
    arma::mat m1 = arma::eye<arma::mat>(3, 3);
    arma::mat m2 = arma::eye<arma::mat>(3, 3);
	                     
    return m1 + 3 * (m1 + m2);
}


// another simple example: outer product of a vector, 
// returning a matrix
//
// [[Rcpp::export]]
arma::mat rcpparma_outerproduct(const arma::colvec & x) {
    arma::mat m = x * x.t();
    return m;
}

// and the inner product returns a scalar
//
// [[Rcpp::export]]
double rcpparma_innerproduct(const arma::colvec & x) {
    double v = arma::as_scalar(x.t() * x);
    return v;
}


// and we can use Rcpp::List to return both at the same time
//
// [[Rcpp::export]]
Rcpp::List rcpparma_bothproducts(const arma::colvec & x) {
    arma::mat op = x * x.t();
    double    ip = arma::as_scalar(x.t() * x);
    return Rcpp::List::create(Rcpp::Named("outer")=op,
                              Rcpp::Named("inner")=ip);
}


////////////////////////////////////////////////////////////
//' pmf for zero-inflated poisson
//' @param p proportion of structural zero's
//' @param theta the poisson mean
//' @param y the observed value
//' @param loga Logical. Whether to return the log probability or not.
//' @return the probability mass of the zero-inflated poisson distribution
//' @export
// [[Rcpp::export]]
double dzip(double p, double theta, int y, bool loga){
    double result;
    if(loga==FALSE){
        if(y==0) result = p + (1-p) * exp(-theta);
        else result = (1-p) * R::dpois(y, theta, loga);
    }
    else{
        if(y==0) result = log(p + (1-p) * exp(-theta));
        else result = log(1-p) + R::dpois(y, theta, loga);
    }
    return result;
}
