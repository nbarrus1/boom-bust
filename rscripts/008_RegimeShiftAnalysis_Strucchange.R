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

load(here("output","final_set.Rdata"))

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
















