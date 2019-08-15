sampleQuality <- function(sample, population, k){
    KL <- rep(NA,k)
    for(ii in (1:k)){
        Y1 <- sample[,ii]
        Y2 <- population[,ii]
        nr <- length(Y2)
        r <- range(Y2)
        Y1.dis <- entropy::discretize(Y1,numBins = nr^(1/3), r=r)
        Y2.dis <- entropy::discretize(Y2,numBins = nr^(1/3), r=r)
        #KL[ii] <- KL.Dirichlet(Y1.dis,Y2.dis,a1=1/length(Y1),a2=1/length(Y2)) # Schurmann-Grassberger (1996) entropy estimator
        KL[ii] <- KL.Dirichlet(Y1.dis,Y2.dis,a1=1,a2=1) # KL divergence with laplace prior
        #p1 <- as.data.frame(freqs.Dirichlet(Y1.dis+1, 0))$Freq 
        #p2 <- as.data.frame(freqs.Dirichlet(Y2.dis+1, 0))$Freq
        #KL[ii] <- sum((p1-p2)*(log(p1)-log(p2)))
    }
    return(exp(-mean(KL)))
}


BayesCalib <- R6::R6Class() {

}


run_eplus <- function() {
    #' @param y_labels: list of output(s) to extract from each energyplus simulation to form the matrix yc
    #' @param x_labels: list of observed input(s) to extract from each energyplus simulation to form the matrix xc 
    #' @param param: list of calibration parameters and the range to sample from 
    #'            (similar to sensitivity analysis parametric)
    #' @param num_sim: number of energyplus simulation to run
    #' @param run_period: modify run period object in EnergyPlus to set simulation run period
    #' @return eta: A matrix containing the output(s) based on y_labels
    #' @return xc: A matrix containing the observed input(s) based on x_labels for each corresponding yc
    #' @return tc: A matrix containing the values of the calibration parameters used for each corresponding yc
}

run_stan <- function() {
    #' preconditions: 
    #' ncol(yf) == ncol(yc)
    #' ncol(xf) == ncol(xc)
    #' nrow(yf) == nrow(xf)
    #' nrow(yc) == nrow(xc) == nrow(tc)
    #' @param y:
    #' @param xf:
    #' @param eta:
    #' @param xc:
    #' @param tc:
    #' @param x_pred:
    #' @param subset: If `TRUE` subset data used for the calibration to reduce computation time
    #' @param iter: number of iterations for each MCMC chain
    #' @param chains: number of MCMC chains
    #' @return fit: stan_fit object
    #' 
    
    d <- ncol(y)
    p <- ncol(xf)
    n <- nrow(yf) 
    m <- nrow(eta) 
    q <- ncol(tc)
    
    # standardization of output y and eta
    for (i in (1:d)){
        eta_mu <- mean(eta[,i], na.rm = TRUE) # mean value
        eta_sd <- sd(eta[,i], na.rm = TRUE) # standard deviation
        y[,i] <- (y[,i] - eta_mu) / eta_sd
        eta[,i] <- (eta[,i] - eta_mu) / eta_sd
    }
    
    
    # Put design points xf and xc on [0,1]
    x <- rbind(as.matrix(xf), as.matrix(xc))
    for (i in (1:p)){
        x_min <- min(x[,i], na.rm = TRUE)
        x_max <- max(x[,i], na.rm = TRUE)
        xf[,i] <- (xf[,i] - x_min) / (x_max - x_min)
        xc[,i] <- (xc[,i] - x_min) / (x_max - x_min)
        x_pred[,i] <- (x_pred[,i] - x_min) / (x_max - x_min)
    }
    
    # create data as list for input to Stan
    stan_data <- list(d=d, n=n, m=m, p=p, q=q, 
                      eta=eta, y=y,
                      xf=xf, xc=xc, tc=tc)
    
    fit <- stan(file = "bc.stan", 
                data = stan_data, 
                iter = iter, 
                chains = chains)
    
    
}


