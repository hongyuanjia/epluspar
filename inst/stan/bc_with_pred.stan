data {
    int<lower=1> n; // number of field data
    int<lower=1> m; // number of computer simulation
    int<lower=1> n_pred; // number of predictions
    int<lower=1> p; // number of observable inputs x
    int<lower=1> q; // number of calibration parameters t
    int<lower=1> D; // number of observed outputs y
    matrix[n, D] y; // field observations multi output
    matrix[m, D] eta; // output of computer simulations
    matrix[n, p] xf; // observable inputs corresponding to y
    // (xc, tc): design points corresponding to eta
    matrix[m, p] xc;
    matrix[m, q] tc;
    // x_pred: new design points for predictions
    matrix[n_pred, p] x_pred;
}

transformed data {
    real delta = 1e-9;
    // real<lower=0> rho = 1.0;
    vector<lower=0>[D] alpha = rep_vector(1.0, D);
    real<lower=0> sigma = sqrt(0.01);
    int<lower=1> N = n+m+n_pred;
    matrix[n+m, D] y_eta = append_row(y, eta); // y_eta = [y, eta]
}

parameters {
    // tf: calibration parameters
    row_vector<lower=0,upper=1>[q] tf;
    matrix[n_pred, D] y_pred;
    cholesky_factor_corr[D] L_Omega;
    matrix[N, D] tau;
    real<lower=0,upper=1> rho;
    real<lower=0> lambda;
    // real<lower=0> sigma;
}

transformed parameters {
    real beta = -4.0 * log(rho);
}

model {
    // declare variables
    matrix[N, D] f;
    matrix[N, (p+q)] xt;
    matrix[N, N] K;
    matrix[N, N] L; // cholesky decomposition of covariance matrix
    matrix[N, D] z; // z = [y, eta, y_pred]

    z = append_row(y_eta, y_pred); // z = [y, eta, y_pred]

    // xt = [[xt,tf],[xc,tc]]
    xt[1:n, 1:p] = xf;
    xt[1:n, (p+1):(p+q)] = rep_matrix(tf, n);
    xt[(n+1):(n+m), 1:p] = xc;
    xt[(n+1):(n+m), (p+1):(p+q)] = tc;
    xt[(n+m+1):N, 1:p] = x_pred;
    xt[(n+m+1):N, (p+1):(p+q)] = rep_matrix(tf, n_pred);

    // off-diagonal elements of sigma_eta
    for (i in 1:(N-1)) {
        K[i, i] = (1 / lambda) + delta;
        for (j in (i+1):N) {
            K[i, j] = exp(-dot_self((xt[i] - xt[j]) * beta));
            K[i, j] = K[i, j] / lambda;
            K[j, i] = K[i, j];
        }
    }
    K[N, N] = (1 / lambda) + delta;
    L = cholesky_decompose(K); // cholesky decomposition

    f = L * tau * diag_pre_multiply(alpha, L_Omega)';

    rho ~ beta(1.0, 0.4);
    lambda ~ gamma(5, 5); // gamma (shape, rate)
    // sigma ~ std_normal();
    L_Omega ~ lkj_corr_cholesky(1);
    to_vector(tau) ~ std_normal();
    to_vector(z) ~ normal(to_vector(f), sigma);

}
