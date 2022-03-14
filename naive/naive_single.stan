data {
  int N;        // Number of observations (101)
  int y[N];     // Vector of observations
  vector[N] x;  // Vector of DOY
}
 
// The parameters we are going to estimate in our model
parameters {
  real<lower=0> p;  // day of peak escapement  
  real log_r;       // total escapement 
  real<lower=0, upper=15> sigma; //standard deviation in arrival timing
  real<lower=0> reciprocal_phi;  // over dispersion parameter for the negative binomial 
}
 
transformed parameters{
  real r=exp(log_r);  
  real phi;
  vector[N] log_phi2;
  vector[N] c_hat;     // expected values of the model
  phi = 1. / reciprocal_phi;
//// These are vectorized
  log_phi2 = -square(x-p) / (2*square(sigma));
  c_hat = log_r + log_phi2 - log_sum_exp(log_phi2);
}

 model {
  //Priors
  reciprocal_phi ~ cauchy(0., 3);
  log_r ~ uniform(1,20);
  sigma ~ cauchy(0., 3);
  p ~ uniform(50, 330);
  // MODEL 
  y ~ neg_binomial_2_log(c_hat, phi);
}

generated quantities {
  vector[N] mu;
  //vector[N] log_lik;
  vector[N] y_rep;
  mu = exp(c_hat);
for (i in 1:N) {
  //log_lik[i] = neg_binomial_2_log_lpmf(y[i] | c_hat[i], phi);
  y_rep[i] = neg_binomial_2_log_rng(c_hat[i], phi);
  }
}