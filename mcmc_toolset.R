cppFunction('
            NumericVector proposal(NumericVector theta, double sigma){
            int nparam = theta.size();
            double jump = 0.1; 
            NumericVector newtheta(nparam+1);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            newtheta[nparam] = R::rnorm(sigma, jump);
            if(newtheta[nparam] <= 0){
            newtheta[nparam] = 0.0001;
            }
            return newtheta;
            }')

cppFunction('
                double objdens(NumericMatrix X, NumericVector y, NumericVector theta, double sigma){
                double lkh, logprior, yhat;
                int m=X.nrow(), p=X.ncol();
                NumericVector aux(m);
                // Compute loglikelihood
                lkh=0;
                for (int i=0; i<m; i++){
                aux = X(i,_)*theta;
                yhat = std::accumulate(aux.begin(), aux.end(), 0.0);
                lkh += -.5/pow(sigma,2)*pow(y[i] - yhat,2);
                }
                // Compute logprior
                logprior = 0.0;
                for(int j=0; j<p; j++){
                logprior += R::dnorm(theta[j], 0.0, 100, true); // Aqu?? la inicial!!
                }
                logprior += R::dgamma(sigma, 5.0, 0.01, true);
                // Log of target density
                return lkh + logprior;
                }')
