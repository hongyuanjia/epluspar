data {
    int<lower=1> n; // number of field data
    int<lower=1> m; // number of computer simulation
    int<lower=1> p; // number of observable inputs x
    int<lower=1> q; // number of calibration parameters t
    int<lower=1> D; // number of observed outputs y
    matrix[n, D] y; // field observations multi output
    matrix[m, D] eta; // output of computer simulations
    row_vector[p] xf[n]; // observable inputs corresponding to y
    // (xc, tc): design points corresponding to eta
    row_vector[p] xc[m];
    row_vector[q] tc[m];
}

transformed data {
    real delta = 1e-9;
    real<lower=0> rho = 1.0;
    vector<lower=0>[D] alpha = rep_vector(1.0, D);
    real<lower=0> sigma = sqrt(0.1);
    int<lower=1> N = n+m;
    matrix[N, D] z = append_row(y, eta); // z = [y, eta]
}

parameters {
    cholesky_factor_corr[D] L_Omega;
    matrix[N, D] tau;
    // tf: calibration parameters
    // rho_eta: reparameterization of beta_eta
    // lambda_eta: precision parameter for eta
    // lambda_e: precision parameter of observation error
    row_vector<lower=0,upper=1>[q] tf;
    row_vector<lower=0,upper=1>[p+q] rho_eta;
}

transformed parameters {
    row_vector[p+q] beta_eta;
    row_vector[p+q] xt[N];
    beta_eta = -4.0 * log(rho_eta);

    // xt = [[xt,tf],[xc,tc]]
    for (i in 1:n) {
        xt[i] = append_col(xf[i],tf);
    }
    for (i in (n+1):N) {
        xt[i] = append_col(xc[i-n],tc[i-n]);
    }
}

model {
    // declare variables
    matrix[N, D] f;

    matrix[N, N] K; // covariance matrix
    matrix[N, N] L; // cholesky decomposition of covariance matrix

    // off-diagonal elements of sigma_eta
    for (i in 1:(N-1)) {
        K[i, i] = 1 + delta;
        for (j in (i+1):N) {
            K[i, j] = exp(-dot_self((xt[i] - xt[j]) .* beta_eta));
            K[j, i] = K[i, j];
        }
    }
    K[N, N] = 1 + delta;

    L = cholesky_decompose(K); // cholesky decomposition
    f = L * tau * diag_pre_multiply(alpha, L_Omega)';

    rho_eta[1:(p+q)] ~ beta(1.0, 0.4);
    L_Omega ~ lkj_corr_cholesky(1);
    to_vector(tau) ~ normal(0, 1);
    to_vector(z) ~ normal(to_vector(f), sigma);

}

generated quantities {
    matrix[D, D] Omega;
    Omega = L_Omega * L_Omega';
}
