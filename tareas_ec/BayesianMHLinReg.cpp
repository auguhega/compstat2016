#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List MHBayesLinReg(int nsim, NumericVector theta0, double sigma0, Function objdens, Function proposal,
                   NumericMatrix data, NumericVector y){
  // theta will contain the output, one column pero parameter, row per simulation
  int nparam=theta0.size();
  NumericMatrix theta(nsim, nparam);
  NumericVector sigma(nsim);
  theta(0,_) = theta0;
  sigma[0] = sigma0;

  // X will save proposals, Rej will save number of rejection rates=(trials-1)/trials
  NumericVector X(nparam), aux(nparam+1);
  double sigmaX;
  NumericVector rejections(nsim);
  // logU is for the test
  double logU;
  // accept tells wether a proposal is accepted, trials counts attemps before accepting
  bool accept=false;
  // trials max is the maxnumber of inner cycles in what follows, trial the counter
  int trials;
  int maxtrials=100000;
  // outer cycle: sim n jumps
  for (int i=1; i<nsim; i++){
    // inner cycle: repeat until accepting
    trials = 0;
    accept = false;
    while (!accept && trials<maxtrials){
      aux = as<NumericVector>(proposal(theta(i-1,_), sigma[i-1]));
      sigmaX = aux[nparam];
      for(int j=0; j<nparam; j++){
        X[j] = aux[j];
      }
      logU = log(R::runif(0,1));
      // the minus is since we used LOGS!!!!!
      if(logU <= as<double>(objdens(data, y, X, sigmaX)) - as<double>(objdens(data, y, theta(i-1,_), sigma[i-1]))) {
        accept = true;
        theta(i,_) = X;
        sigma[i] = sigmaX;
      }
      trials++;
    }
    rejections[i] = trials;
  }
  return List::create(Named("theta") = theta, Named("sigma") = sigma, Named("rejections")  = rejections);
}




