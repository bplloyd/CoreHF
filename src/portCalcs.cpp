#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]

#include <RcppParallel.h>


#include <math.h>

using namespace std;
using namespace arma;
using namespace Rcpp;
using namespace RcppParallel;



// [[Rcpp::export]]
arma::vec return_port_cpp(arma::mat R, arma::vec weights) {
  return(R * weights);
  // return(weights);
  // return(R);
}

// [[Rcpp::export]]
double return_annualized_cpp(arma::vec r, double freq = 12) {
  // exp(sum((freq/length(R))*log(1+R))) - 1
  return(exp(arma::sum( (freq/r.n_elem)  *  arma::log(1.0 + r) )) - 1.0);
  // return(arma::as_scalar(  arma::exp(  arma::sum( (freq/r.n_elem)  *  arma::log(1.0 + r) ) ) )   );
}

// [[Rcpp::export]]
double std_cpp(arma::vec r){
  // return(sqrt(freq) * arma::stddev(r));
  return(arma::stddev(r));
}


struct AnalyzePorts: public Worker {

  const mat R;
  const mat ports;
  const double freq;
  mat & output;
  

  AnalyzePorts(const mat R, const mat ports, const double freq, mat & out)
    : R(R), ports(ports), freq(freq), output(out) {}

  void operator()(std::size_t begin, std::size_t end) {
    for (std::size_t i = begin; i < end; i++) {
      vec r_i = return_port_cpp(R, ports.row(i).t());
      rowvec row_i = {return_annualized_cpp(r_i, freq), std_cpp(r_i)};
      output.row(i) = row_i;
    }
  }
};

// [[Rcpp::export]]
NumericMatrix analyzePorts_cpp(const arma::mat R, const arma::mat ports, const double freq = 12){
  arma::mat out(ports.n_rows, 2);

  AnalyzePorts analyzePorts(R, ports, freq, out);
  parallelFor(0, ports.n_rows, analyzePorts);

  // return(wrap(cholesk));
  return wrap(out);
}

