library(samplr)
source("prior.R")
set.seed(1)

N_SIMS <- 1e5
models <- model_prior(N_SIMS)
parameters <- params_prior(N_SIMS)
starts <- round(rnorm(N_SIMS, mean = 176.4, sd = 12))

responses <- matrix(nrow=N_SIMS, ncol = 150)

for (i in 1:N_SIMS){
  if (i %% 100 == 0) print(i / N_SIMS)
  
  responses[i, ] <- round(mcmc_sampler(models[i], parameters[i,], start = starts[i]))
}

saveRDS(responses, "simulations.rds")
saveRDS(data.frame(models, parameters), "parameters.rds")

