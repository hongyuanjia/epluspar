/*
    The MIT License (MIT)

    Copyright (c) 2019-2025 Hongyuan Jia and Adrian Chong

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE.
*/

data {
  int<lower=1> n; // number of field data
  int<lower=1> m; // number of computer simulation
  int<lower=1> n_pred; // number of predictions
  int<lower=1> p; // number of observable inputs x
  int<lower=1> q; // number of calibration parameters t
  vector[n] yf; // field observations
  vector[m] yc; // output of computer simulations
  array[n] row_vector[p] xf; // observable inputs corresponding to y
  // (xc, tc): design points corresponding to eta
  array[m] row_vector[p] xc;
  array[m] row_vector[q] tc;
  array[n_pred] row_vector[p] x_pred;
}

transformed data {
  real delta = 1e-5;
  // real delta = 1;
  int<lower = 1> N;
  vector[n+m] y;
  vector[n+m+n_pred] mu; // mean vector
  array[n + n_pred] row_vector[p] X; // X=[xf, x_pred]

  N = n + m + n_pred;
  // set mean vector to zero
  for (i in 1:N) {
    mu[i] = 0;
  }
  X[1:n] = xf;
  X[n+1:(n+n_pred)] = x_pred;
  y = append_row(yf, yc); // y = [yf, yc]
}

parameters {
  // tf: calibration parameters
  // rho_eta: reparameterization of beta_eta
  // lambda_eta: precision parameter for eta
  // lambda_e: precision parameter of observation error
  // y_pred: predictions
  row_vector<lower=0, upper=1>[q] tf;
  row_vector<lower=0, upper=1>[p+q] rho_eta;
  real<lower=0> sigma_e;
  vector[n_pred] y_pred;
}

transformed parameters {
  // beta_delta: correlation parameter for bias term
  // beta_e: correlation parameter of observation error
  row_vector[p+q] beta_eta;
  array[N] row_vector[p+q] xt;
  beta_eta = -4.0 * log(rho_eta);
  // xt = [[xf,tf],[xc,tc],[x_pred,tf]]
  for (i in 1:n) {
    xt[i] = append_col(xf[i],tf);
  }
  for (i in (n+1):(n+m)) {
    xt[i] = append_col(xc[i-n],tc[i-n]);
  }
  for (i in (n+m+1):N) {
    xt[i] = append_col(x_pred[i-n-m],tf);
  }
}

model {
  // declare variables
  vector[N] z; // z = [yf, yc, y_pred]
  matrix[N, N] sigma_eta; // simulator covarinace
  matrix[N, N] sigma_z; // covariance matrix
  matrix[N, N] L; // cholesky decomposition of covariance matrix
  row_vector[p+q] temp_eta;

  z = append_row(y, y_pred); // z = [y, eta, y_pred]

  // elements of sigma_eta
  for (i in 1:(N-1)) {
  sigma_eta[i, i] = 1 + delta;
    for (j in (i+1):N) {
    sigma_eta[i, j] = exp(-dot_self((xt[i] - xt[j]) .* beta_eta));
      sigma_eta[j, i] = sigma_eta[i, j];
    }
  }
  sigma_eta[N, N] = 1 + delta;


  // computation of covariance matrix sigma_z
  sigma_z = sigma_eta;

  // add observation errors
  for (i in 1:n) {
    sigma_z[i, i] = sigma_z[i, i] + sigma_e;
  }

  // Specify priors here
  rho_eta ~ beta(1.0, 0.3);
  sigma_e ~ normal(0, 0.05);

  L = cholesky_decompose(sigma_z); // cholesky decomposition
  z ~ multi_normal_cholesky(mu, L);
}

