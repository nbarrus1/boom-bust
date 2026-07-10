library(here)
library(tidyverse)
library(R2jags)
library(ggmcmc)

rm(list = ls())

load(here("output","final_set.Rdata"))



### jags code

write("model {
  # Prior for process variance
  tau <- 1 / (sigma * sigma)
  sigma ~ dunif(0, 100)

  # Priors for lag weights (bj terms)
  for (k in 1:(L+1)) {
    b[k] ~ dnorm(0, 1.0E-6)
  }

#first value
mu[1] ~ dnorm(first_vals[1], 0.01)

# For early time points with partial lag sums
  for (i in 2:(L+1)) {
    mu[i] <- inprod(b[1:(i-1)], y[(i-(i-1)):(i-1)])
  }

  # Likelihood and internal lag handling
  for (i in (L+2):M) {
  
  mu[i] <- inprod(b[1:(L+1)], y[(i-(L+1)):(i-1)])
  
  }
  # Observation model
  for (i in 1:M) {  
    y[i] ~ dnorm(mu[i], tau)
  }
}
", file = here("JAGS_mods","aagard_method_jags.txt"))




bayesian_ss_aagard <- function(df) {

ydata <-c(df$y,NA_real_)
  
rle.result = rle(ydata == 0)

L <- if_else(max(rle.result$lengths[rle.result$values == TRUE], na.rm =T) == -Inf,
            true = 0, false = max(rle.result$lengths[rle.result$values == TRUE],na.rm = T))

M <- length(ydata)


jags_data <- list(y = ydata,
                  L = L,
                  M = length(ydata),
                  first_vals = ydata[1:(L+1)])


jags_out <- jags(jags_data,
                  parameters.to.save = c("mu","tau","b","sigma"),
                  model.file = here("JAGS_mods","aagard_method_jags.txt"),
                  n.chains = 2, n.burnin = 1000, n.iter = 10000, n.thin = 2)

jags_out

}


final.set.preds <- final.set |> 
  mutate(jags.output = map(.x = ls, .f = bayesian_ss_aagard))|> 
  mutate(ggs.output = map(.x = jags.output, .f = function (df) {
    ggs(as.mcmc(df))
  }),
  convergence_check = map_dbl(.x = jags.output, .f = function (df) {
    out.summary <- as_tibble(df$BUGSoutput$summary)
    nrow(out.summary |>
           filter(n.eff <400 | Rhat > 1.01))
  }),
  predictions = map2(.x = jags.output, .y = ls, .f = function (df1,df2) {
    n <- length(df2$y)+1
    par.names <- paste0("mu[",1:n,"]")
    tibble(obs = c(df2$y,NA_real_),
           x_obs = c(df2$x,df2$x[length(df2$x)]+1),
           x_pred = c(df2$x-1,df2$x[length(df2$x)]),
           pred = df1$BUGSoutput$summary[par.names,"mean"],
           lci = df1$BUGSoutput$summary[par.names,"2.5%"],
           uci = df1$BUGSoutput$summary[par.names,"97.5%"])
  }))

bayesian_model_output <- final.set.preds |> 
  select(plot,group,jags.output,ggs.output)


save(bayesian_model_output,
       file = here("output","bayesian_model_output.Rdata"))

final.set.preds <- final.set.preds |> 
  select(-jags.output, -ggs.output)
  
save(final.set.preds,
     file = here("output","final_set.Rdata"))


rm(list = ls())
