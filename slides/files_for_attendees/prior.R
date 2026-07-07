params_prior <- function(n){
  params <- matrix(0, nrow = n, ncol = 5)
  colnames(params) <- c("sigma", "epsilon", "L", "nChains", "alpha")
  
  # Proposal width for MH, RECMH, MC3, ...
  params[,"sigma"] <- runif(n, 0,50)
  # Trajectory length for HMC, RECHMC, ...
  params[,"epsilon"] <- .1
  params[,"L"] <- sample(1:100, n, replace = T)
  
  ## Multiple chains
  params[,"nChains"] <- sample(4:6, n, replace = T)
  
  # Alpha for recycled samplers
  params[,"alpha"] <- runif(n,0,1)
  
  return(params)
}

model_prior <- function(n){
  sample(c(
    "MH",
    "RECMH",
    "HMC",
    "RECHMC",
    "MC3",
    "RECMC3",
    "MCHMC",
    "MCREC"), size = n, replace = T)
}

mcmc_sampler <- function(
    model, params, start, 
    dname="norm", dparams=c(176.4, 12), iter=150
){
  #           1           2      3       4         5
  # params: "sigma", "epsilon", "L", "nChains", "alpha"
  
  model <- toupper(model)
  
  if (model == "MH"){
    res <- samplr::sampler_mh(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      sigma_prop = params[1], 
      iterations = iter
    )[[1]][1:iter]
  } else if (model == "RECMH"){
    res <- samplr::sampler_mh(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      sigma_prop = params[1], 
      iterations = iter, 
      alpha=params[5]
    )[[1]][1:iter]
  } else if (model == "HMC"){
    res <- samplr::sampler_hmc(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      epsilon = params[2], 
      L = params[3], 
      iterations = iter, 
    )[[1]][1:iter]
  } else if (model == "RECHMC"){
    res <- samplr::sampler_mcrec(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      nChains = 1, 
      epsilon = params[2], 
      L = params[3], 
      alpha = params[5], 
      iterations = iter, 
    )[[1]][1:iter,,1]
  } else if (model == "MC3"){
    res <- samplr::sampler_mc3(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      sigma_prop = params[1], 
      nChains = params[4],
      iterations = iter, 
    )[[1]][1:iter,,1]
  } else if (model == "RECMC3"){
    res <- samplr::sampler_mc3(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      sigma_prop = params[1], 
      nChains = params[4],
      iterations = iter, 
      alpha = params[5]
    )[[1]][1:iter,,1]
  } else if (model == "MCHMC"){
    res <- samplr::sampler_mchmc(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      epsilon = params[2], 
      L = params[3], 
      nChains = params[4],
      iterations = iter, 
    )[[1]][1:iter,,1]
  } else if (model == "MCREC"){
    res <- samplr::sampler_mcrec(
      start = start, 
      distr_name = dname, 
      distr_params = dparams,
      epsilon = params[2], 
      L = params[3], 
      nChains = params[4],
      iterations = iter, 
      alpha = params[5]
    )[[1]][1:iter,,1]
  } else {
    stop("model not recognised")
  }
  return(res)
}
