data{
  int <lower = 0> n;
  int <lower =0> sales[n];
  real <lower = 0> own_price[n];
  real <lower = 0> comp_price[n];
  real <lower = 0> pg_price[n];
  real <lower = 0> diff_365[n];
}

parameters{
  real alpha;
  real own_price_ela;
  real comp_price_ela;
  real pg_price_ela;
  real b4;
}

transformed parameters{
  real lp[n];
  real <lower = 0> mu[n];
  for(i in 1:n){
    lp[i] = alpha + own_price_ela * log(own_price[i]) + comp_price_ela * log(comp_price[i]) + pg_price_ela * log(pg_price[i]) + b4 * diff_365[i];
    mu[i] = exp(lp[i]);
  }
}

model{
  sales ~ poisson(mu);
  own_price_ela ~ normal(-2.7, 0.6);
  comp_price_ela ~ normal(1.8, 0.4);
}
