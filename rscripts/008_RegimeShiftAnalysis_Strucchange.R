#### this script is for performing the regime shift analysis using the 
##strucchange package as one suggested in Anderson et al., 2008-Cell

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)

theme_set(theme_bw())


##load in the final set of time series for meta-analysis

final.set.preds <- readRDS(here("output","bayesian_model_checkpoint.rds"))


#------------------------------------------------------------------
####create the function for dynamic modelling of the break-points###
#------------------------------------------------------------------

fit.breaks <-  function(df, brks = 2) {
  
  if(nrow(df)*0.15<2) { 
    
    BP_fit <- breakpoints(pred~1, data = df, breaks = brks, h = 2)
    BP_fit
  } else {
    BP_fit <- breakpoints(pred~1, data = df, breaks = brks)
    BP_fit  
  }
}


fix.aagaard <- function (df, plot) {
  if(str_detect(plot, "Aagaard")) {
    
    df |> 
      select(-x_pred,-pred) |> 
      rename(pred = obs,
             x_pred = x_obs) |> 
      select(pred,x_pred) |> 
      drop_na(pred)
    
  } else {
    
    df
    
  }
} 

#------------------------------------------------------------------
#### Initialize breakpoint-analysis columns ####
#------------------------------------------------------------------

checkpoint_file <- here(
  "output",
  "final_set_brks_checkpoint.rds"
)

final_file <- here(
  "output",
  "final_set_brks.rds"
)

checkpoint_every <- 100L




final.set.preds.brks <- final.set.preds |>
  mutate(
    brks_fit      = vector("list", n()),
    brks_fit_summ = vector("list", n()),
    brks_opt_num  = NA_integer_,
    brks_fit_opt  = vector("list", n()),
    breaks.preds  = vector("list", n()),
    breakpoint_error = NA_character_,
    breakpoint_complete = FALSE
  )


#------------------------------------------------------------------
#### Run breakpoint analysis ####
#------------------------------------------------------------------

for (i in seq_len(nrow(final.set.preds.brks))){
  
  message(
    i, "/", nrow(final.set.preds.brks),
    " | ", final.set.preds.brks$plot[i]
  )
  
  error.temp <- tryCatch({
    
    #--------------------------------------------------------------
    # Replace Aagaard predictions with observations when necessary
    #--------------------------------------------------------------
    
    predictions.temp <- fix.aagaard(
      df   = final.set.preds.brks$predictions[[i]],
      plot = final.set.preds.brks$plot[i]
    )
    
    final.set.preds.brks$predictions[[i]] <- predictions.temp
    
    
    #--------------------------------------------------------------
    # Fit models allowing up to two breakpoints
    #--------------------------------------------------------------
    
    breaks.fit.temp <- fit.breaks(
      df = predictions.temp,
      brks = 2
    )
    
    final.set.preds.brks$brks_fit[[i]] <- breaks.fit.temp
    
    
    #--------------------------------------------------------------
    # Summarize candidate breakpoint models
    #--------------------------------------------------------------
    
    breaks.summary.temp <- summary(breaks.fit.temp)
    
    final.set.preds.brks$brks_fit_summ[[i]] <-
      breaks.summary.temp
    
    
    #--------------------------------------------------------------
    # Select the optimal number of breakpoints using BIC
    #--------------------------------------------------------------
    
    optimal.breaks.temp <-
      as_tibble(
        pluck(breaks.summary.temp, "RSS")
      ) |>
      slice(2) |>
      pivot_longer(
        cols = everything(),
        names_to = "position",
        values_to = "BIC"
      ) |>
      mutate(
        position = as.integer(position)
      ) |>
      filter(
        !is.na(BIC),
        BIC == min(BIC, na.rm = TRUE)
      ) |>
      slice(1) |>
      pull(position)
    
    if (length(optimal.breaks.temp) == 0L) {
      stop("No optimal breakpoint model could be selected.")
    }
    
    final.set.preds.brks$brks_opt_num[i] <-
      optimal.breaks.temp
    
    
    #--------------------------------------------------------------
    # Refit using the selected number of breakpoints
    #--------------------------------------------------------------
    
    optimal.fit.temp <- fit.breaks(
      df = predictions.temp,
      brks = optimal.breaks.temp
    )
    
    final.set.preds.brks$brks_fit_opt[[i]] <-
      optimal.fit.temp
    
    
    #--------------------------------------------------------------
    # Extract rows corresponding to estimated breakpoints
    #--------------------------------------------------------------
    
    breakpoint.indices.temp <-
      optimal.fit.temp$breakpoints
    
    if (
      length(breakpoint.indices.temp) == 0L ||
      all(is.na(breakpoint.indices.temp))
    ) {
      
      final.set.preds.brks$breaks.preds[[i]] <-
        predictions.temp[0, , drop = FALSE]
      
    } else {
      
      final.set.preds.brks$breaks.preds[[i]] <-
        predictions.temp |>
        slice(
          breakpoint.indices.temp[
            !is.na(breakpoint.indices.temp)
          ]
        )
    }
    
    
    #--------------------------------------------------------------
    # Mark successful completion
    #--------------------------------------------------------------
    
    final.set.preds.brks$breakpoint_complete[i] <- TRUE
    
    NA_character_
    
  }, error = function(e) {
    
    conditionMessage(e)
    
  })
  
  
  #----------------------------------------------------------------
  # Store any error from the current iteration
  #----------------------------------------------------------------
  
  final.set.preds.brks$breakpoint_error[i] <-
    error.temp
  
  if (!is.na(error.temp)) {
    
    message(
      "  Error in row ", i, ": ",
      error.temp
    )
  }
  
  
  #----------------------------------------------------------------
  # Save checkpoint periodically
  #----------------------------------------------------------------
  
  if (
    i %% checkpoint_every == 0L ||
    i == nrow(final.set.preds.brks)
  ) {
    
    saveRDS(
      final.set.preds.brks,
      file = checkpoint_file
    )
    
    message(
      "Checkpoint saved at row ",
      i,
      "."
    )
  }
}


#------------------------------------------------------------------
#### Save completed breakpoint results ####
#------------------------------------------------------------------

saveRDS(
  final.set.preds.brks,
  file = final_file
)

message("Breakpoint analysis complete.")




















 

#------------------------------------------------------------------
####implement the dynamic modelling approach for break-points###
#---------------------------------------------------------------

final.set.preds.brks <- final.set.preds |>
  mutate(predictions = map2(.x = predictions,.y = plot, .f = fix.aagaard),
         brks_fit = map(.x = predictions, .f = fit.breaks),
         brks_fit_summ = map(.x = brks_fit, .f = summary),
         brks_opt_num = map_int(.x = brks_fit_summ, .f = function(.x) {
           temp <- as_tibble(pluck(.x, "RSS")) |> 
             slice(2)
           temp |> 
             pivot_longer(1:ncol(temp), names_to = "position", values_to = "BIC") |> 
             mutate(position = as.integer(position)) |> 
             filter(BIC == min(BIC)) |> 
             pull(position)
         }),
         brks_fit_opt = map2(.x = predictions, .y = brks_opt_num, .f = fit.breaks),
         breaks.preds = map2(.x = predictions, .y = brks_fit_opt, .f = function(.x, .y){
           .x |> 
             slice(.y$breakpoints)
         }))


save(final.set.preds.brks,file = here("output","final_set_brks.Rdata"))
#------------------------------------
####function for dynamic modelling the plots####
#------------------------------------


plot.brks <- function(df.data, df.breakpoints, plot) {
  
  if(str_detect(plot, "Aagaard")) {
  
  df.data |> 
    mutate(lci = if_else(lci < 0, true = 0, false = lci)) |> 
    ggplot(aes(x = x_pred, y = pred))+
    geom_vline(data = df.breakpoints,
               aes(xintercept = x_pred),
               color = "black", linetype = "dashed")+
    #geom_ribbon(aes(ymax = lci, ymin = uci, fill = "Predicted"), alpha = 0.3)+
    geom_line(aes(color = "Predicted"))+
    #geom_point(aes(x = x_obs, y = obs, shape = "Observed"),size = 2, color = "black",  fill = "#666666")+
    #geom_line(aes(y = REGIME.AVE), color = "red", linetype = "dashed")+
    #coord_cartesian(ylim = c(min(if_else(df.data$lci<0,true = 0, false = df.data$lci<0)),max(df.data$uci)))+
    theme_bw(base_size = 12) +
    scale_fill_manual(values = "red", name = NULL)+
    scale_color_manual(values = "red", name = NULL)+
    scale_shape_manual(values = 21, name = NULL)+
    theme(
      axis.ticks.length = unit(.25, "cm"),
      axis.title.y = element_text(vjust = 2),
      plot.subtitle = element_text(face = "italic", size = 9),
      plot.title = element_text(size = 9),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
      #plot.margin = unit(c(0,0,0,0), "cm")
    )+
    labs(x = unique(df.data$x_variable), y = unique(df.data$measure),
         subtitle = unique(df.data$species.names),
         title = paste0(unique(df.data$author)))
  
  } else {
    
    df.data |> 
      mutate(lci = if_else(lci < 0, true = 0, false = lci)) |> 
      ggplot(aes(x = x_pred, y = pred))+
      geom_vline(data = df.breakpoints,
                 aes(xintercept = x_pred),
                 color = "black", linetype = "dashed")+
      geom_ribbon(aes(ymax = lci, ymin = uci, fill = "Predicted"), alpha = 0.3)+
      geom_line(aes(color = "Predicted"))+
      geom_point(aes(x = x_obs, y = obs, shape = "Observed"),size = 2, color = "black",  fill = "#666666")+
      #geom_line(aes(y = REGIME.AVE), color = "red", linetype = "dashed")+
      coord_cartesian(ylim = c(min(if_else(df.data$lci<0,true = 0, false = df.data$lci<0)),max(df.data$uci)))+
      theme_bw(base_size = 12) +
      scale_fill_manual(values = "red", name = NULL)+
      scale_color_manual(values = "red", name = NULL)+
      scale_shape_manual(values = 21, name = NULL)+
      theme(
        axis.ticks.length = unit(.25, "cm"),
        axis.title.y = element_text(vjust = 2),
        plot.subtitle = element_text(face = "italic", size = 9),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7)
        #plot.margin = unit(c(0,0,0,0), "cm")
      )+
      labs(x = unique(df.data$x_variable), y = unique(df.data$measure),
           subtitle = unique(df.data$species.names),
           title = paste0(unique(df.data$author)))  
    
    
  }
  
  
  }


final.plots <- final.set.preds.brks |>
  unnest(cols = predictions) |> 
  group_by(plot,group, breaks.preds) |> 
  nest(.key = "predictions") |> 
  mutate(timeseries = pmap(list(df.data = predictions,df.breakpoints = breaks.preds, plot = plot),
                           .f = plot.brks)) 


final.plots$timeseries[1:21]

save(final.plots,file = here("output","final_plots.Rdata"))


#---------------------------------------------
###for loop to save my ggplot pannels###
#---------------------------------------------


nrow(final.plots)/9

pdf(here("output","timeseriespanels_Strucchange.pdf"), width = 11, height = 8)


for(i in 1:(ceiling((nrow(final.plots)/9)-1))) {
  
  
  if (i == 1) {
    
    timeseries.panel <- reduce(final.plots$timeseries[i:(i*9)],`+`)+
      plot_layout(ncol = 3, nrow = 3)
    print(timeseries.panel)
    
  } else {
    
    timeseries.panel <- reduce(final.plots$timeseries[((i*9)+1):((i+1)*9)],`+`)+
      plot_layout(ncol = 3, nrow = 3)
    print(timeseries.panel)
    
  }
  
}


dev.off()
















