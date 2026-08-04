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

###set up the checkpoint function and initialization of the for loop


checkpoint_file <- here(
  "output",
  "bayesian_model_checkpoint.rds"
)

save_every <- 10L


# Overwrites the same checkpoint file each time
save_checkpoint <- function(object, path) {
  
  saveRDS(
    object = object,
    file = path
  )
  
  invisible(NULL)
}


if (file.exists(checkpoint_file)) {
  
  message("Loading existing checkpoint")
  
  final.set.preds <- readRDS(checkpoint_file)
  
  if (nrow(final.set.preds) != nrow(final.set)) {
    stop(
      "The checkpoint contains ",
      nrow(final.set.preds),
      " rows, but final.set contains ",
      nrow(final.set),
      " rows."
    )
  }
  
} else {
  
  message("No checkpoint found. Initializing a new run.")
  
  final.set.preds <- final.set |>
    mutate(
      # Posterior mu summaries used for classification
      predictions = rep(list(NULL), n()),
      
      # Compact summaries for b, sigma, and tau
      parameter_summary = rep(list(NULL), n()),
      
      # Names of parameters that trigger a warning
      convergence_parameters = rep(list(NULL), n()),
      
      # Diagnostics across all monitored parameters
      max_Rhat = NA_real_,
      min_neff = NA_real_,
      n_parameters = NA_integer_,
      n_flagged = NA_integer_,
      proportion_flagged = NA_real_,
      
      # Diagnostics specifically for mu
      max_mu_Rhat = NA_real_,
      min_mu_neff = NA_real_,
      n_mu_flagged = NA_integer_,
      proportion_mu_flagged = NA_real_,
      
      # Run tracking
      model_error = NA_character_,
      completed = FALSE
    )
}


#### the for loop

n_series <- nrow(final.set.preds)
processed_since_save <- 0L


for (i in seq_len(n_series)) {
  
  # Skip rows successfully fitted or previously attempted
  if (isTRUE(final.set.preds$completed[i])) {
    next
  }
  
  message(
    "\nTime series ",
    i,
    "/",
    n_series
  )
  
  
  model_result <- tryCatch(
    
    {
      
      # Fit model
      fit <- bayesian_ss_aagard(
        final.set.preds$ls[[i]]
      )
      
      
      # Complete JAGS summary
      output_summary <- fit$BUGSoutput$summary |>
        as.data.frame() |>
        rownames_to_column("parameter") |>
        as_tibble() |>
        mutate(
          convergence_flag =
            is.na(Rhat) |
            is.na(n.eff) |
            Rhat > 1.01 |
            n.eff < 400
        )
      
      
      # Parameters that triggered the initial warning criteria
      problem_parameters <- output_summary |>
        filter(convergence_flag) |>
        pull(parameter)
      
      
      # Overall convergence diagnostics
      diagnostic_summary <- output_summary |>
        summarise(
          n_parameters = n(),
          
          n_flagged = sum(
            convergence_flag,
            na.rm = TRUE
          ),
          
          proportion_flagged =
            n_flagged / n_parameters,
          
          max_Rhat = if (
            all(is.na(Rhat))
          ) {
            NA_real_
          } else {
            max(Rhat, na.rm = TRUE)
          },
          
          min_neff = if (
            all(is.na(n.eff))
          ) {
            NA_real_
          } else {
            min(n.eff, na.rm = TRUE)
          }
        )
      
      
      series_data <- final.set.preds$ls[[i]]
      
      n_mu <- length(series_data$y) + 1L
      
      mu_names <- paste0(
        "mu[",
        seq_len(n_mu),
        "]"
      )
      
      
      # Extract mu summaries in their correct numeric order
      mu_summary <- output_summary |>
        filter(parameter %in% mu_names) |>
        mutate(
          mu_index = as.integer(
            str_extract(parameter, "\\d+")
          )
        ) |>
        arrange(mu_index)
      
      
      if (nrow(mu_summary) != n_mu) {
        stop(
          "Expected ",
          n_mu,
          " mu parameters, but found ",
          nrow(mu_summary),
          "."
        )
      }
      
      
      # Save the mu estimates and their diagnostics
      prediction_table <- tibble(
        obs = c(
          series_data$y,
          NA_real_
        ),
        
        x_obs = c(
          series_data$x,
          max(series_data$x, na.rm = TRUE) + 1
        ),
        
        x_pred = c(
          series_data$x - 1,
          max(series_data$x, na.rm = TRUE)
        ),
        
        pred = mu_summary$mean,
        sd = mu_summary$sd,
        lci = mu_summary$`2.5%`,
        median = mu_summary$`50%`,
        uci = mu_summary$`97.5%`,
        Rhat = mu_summary$Rhat,
        n.eff = mu_summary$n.eff
      )
      
      
      # Diagnostics specifically for mu
      mu_diagnostic_summary <- mu_summary |>
        summarise(
          n_mu_flagged = sum(
            convergence_flag,
            na.rm = TRUE
          ),
          
          proportion_mu_flagged =
            n_mu_flagged / n(),
          
          max_mu_Rhat = if (
            all(is.na(Rhat))
          ) {
            NA_real_
          } else {
            max(Rhat, na.rm = TRUE)
          },
          
          min_mu_neff = if (
            all(is.na(n.eff))
          ) {
            NA_real_
          } else {
            min(n.eff, na.rm = TRUE)
          }
        )
      
      
      # Compact summaries for important non-mu parameters
      compact_parameter_summary <- output_summary |>
        filter(
          str_detect(
            parameter,
            "^b\\[|^sigma$|^tau$"
          )
        ) |>
        select(
          parameter,
          mean,
          sd,
          `2.5%`,
          `50%`,
          `97.5%`,
          Rhat,
          n.eff,
          convergence_flag
        )
      
      
      list(
        predictions =
          prediction_table,
        
        parameter_summary =
          compact_parameter_summary,
        
        convergence_parameters =
          problem_parameters,
        
        max_Rhat =
          diagnostic_summary$max_Rhat,
        
        min_neff =
          diagnostic_summary$min_neff,
        
        n_parameters =
          diagnostic_summary$n_parameters,
        
        n_flagged =
          diagnostic_summary$n_flagged,
        
        proportion_flagged =
          diagnostic_summary$proportion_flagged,
        
        max_mu_Rhat =
          mu_diagnostic_summary$max_mu_Rhat,
        
        min_mu_neff =
          mu_diagnostic_summary$min_mu_neff,
        
        n_mu_flagged =
          mu_diagnostic_summary$n_mu_flagged,
        
        proportion_mu_flagged =
          mu_diagnostic_summary$proportion_mu_flagged
      )
    },
    
    error = function(e) {
      e
    }
  )
  
  
  if (inherits(model_result, "error")) {
    
    final.set.preds$model_error[i] <-
      conditionMessage(model_result)
    
    # Prevent automatic retry whenever the script resumes
    final.set.preds$completed[i] <- TRUE
    
    message(
      "MODEL ERROR: ",
      final.set.preds$model_error[i]
    )
    
  } else {
    
    final.set.preds$predictions[[i]] <-
      model_result$predictions
    
    final.set.preds$parameter_summary[[i]] <-
      model_result$parameter_summary
    
    final.set.preds$convergence_parameters[[i]] <-
      model_result$convergence_parameters
    
    final.set.preds$max_Rhat[i] <-
      model_result$max_Rhat
    
    final.set.preds$min_neff[i] <-
      model_result$min_neff
    
    final.set.preds$n_parameters[i] <-
      model_result$n_parameters
    
    final.set.preds$n_flagged[i] <-
      model_result$n_flagged
    
    final.set.preds$proportion_flagged[i] <-
      model_result$proportion_flagged
    
    final.set.preds$max_mu_Rhat[i] <-
      model_result$max_mu_Rhat
    
    final.set.preds$min_mu_neff[i] <-
      model_result$min_mu_neff
    
    final.set.preds$n_mu_flagged[i] <-
      model_result$n_mu_flagged
    
    final.set.preds$proportion_mu_flagged[i] <-
      model_result$proportion_mu_flagged
    
    final.set.preds$model_error[i] <- NA_character_
    final.set.preds$completed[i] <- TRUE
    
    message(
      "Completed | all flagged: ",
      model_result$n_flagged,
      "/",
      model_result$n_parameters,
      " | mu flagged: ",
      model_result$n_mu_flagged,
      "/",
      nrow(model_result$predictions),
      " | max mu Rhat: ",
      round(model_result$max_mu_Rhat, 3),
      " | min mu n.eff: ",
      round(model_result$min_mu_neff, 0)
    )
  }
  
  
  processed_since_save <-
    processed_since_save + 1L
  
  rm(model_result)
  
  
  # Overwrite the same checkpoint every save_every attempts
  if (processed_since_save >= save_every) {
    
    save_checkpoint(
      final.set.preds,
      checkpoint_file
    )
    
    message(
      "\nCheckpoint saved: ",
      sum(final.set.preds$completed),
      "/",
      n_series,
      " attempted"
    )
    
    processed_since_save <- 0L
    
    gc()
  }
}


# Save models completed since the last periodic checkpoint
save_checkpoint(
  final.set.preds,
  checkpoint_file
)

message(
  "\nRun finished: ",
  sum(final.set.preds$completed),
  "/",
  n_series,
  " attempted"
)





#Old code that used a nested framework (good but didn't allow to save the checkpoints)

#final.set.preds <- final.set |> 
#  mutate(jags.output = map(.x = ls, .f = bayesian_ss_aagard))|> 
#  mutate(ggs.output = map(.x = jags.output, .f = function (df) {
#    ggs(as.mcmc(df))
#  }),
#  convergence_check = map_dbl(.x = jags.output, .f = function (df) {
#    out.summary <- as_tibble(df$BUGSoutput$summary)
#    nrow(out.summary |>
#           filter(n.eff <400 | Rhat > 1.01))
#  }),
#  predictions = map2(.x = jags.output, .y = ls, .f = function (df1,df2) {
#    n <- length(df2$y)+1
#    par.names <- paste0("mu[",1:n,"]")
#    tibble(obs = c(df2$y,NA_real_),
#           x_obs = c(df2$x,df2$x[length(df2$x)]+1),
#           x_pred = c(df2$x-1,df2$x[length(df2$x)]),
#           pred = df1$BUGSoutput$summary[par.names,"mean"],
#           lci = df1$BUGSoutput$summary[par.names,"2.5%"],
#           uci = df1$BUGSoutput$summary[par.names,"97.5%"])
#  }))


rm(list = ls())
