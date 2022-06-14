data {

  /* Dimensions */
  int<lower=0> N;
  vector[N] offset;     // a predictor with a coefficient of 1
  int<lower=0> p;
   matrix[N,p] X;
  /* Outcome */
  int<lower=0> y[N];
  /* Design Matrix */
  
  /* Hyperparameters*/
  real<lower=0> s; # prior variance

}

parameters {
  vector[p] beta;
}

transformed parameters {
  vector[N] eta;

  eta = offset + X * beta;
}

model {
  /* Prior */
  for (j in 1:p) {
      target += normal_lpdf(beta[j] | 0, s);
  }
  

  /* Likelihood */
  /* y_i ~ poisson(exp(eta_i)); */
  for (i in 1:N) {
      target += poisson_log_lpmf(y[i] | eta[i]);
  }
}

generated quantities {
  vector[N] log_lik;
  int<lower=0> y_rep[N];
  
  for (i in 1:N) {
    
      if (eta[i] > 20) {
          /* To avoid erros like the below during the warmup. */
          /* [2] "  Exception: poisson_log_rng: Log rate parameter is 69.8999, but must be less than 20.7944  (in 'modelb25632c451a8_0ba3e86968d73bc8913ee68b8ce5542b' at line 40)" */
          /* Check posterior predictive. */
          y_rep[i] = poisson_log_rng(20);
      } else {
          y_rep[i] = poisson_log_rng(eta[i]);
      }

      log_lik[i] = poisson_log_lpmf(y[i] | eta[i]);
  }
}

