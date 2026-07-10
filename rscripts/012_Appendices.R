rm(list = ls())

####libraries####

library(here)
library(tidyverse)
library(patchwork)

###data###

load(here("output","all_data.Rdata"))
load(here("output","regimeclassification.Rdata"))




all_data <- all_data_summ

####

all_data_summ <- all_data|>
  filter(native.species == "N") |>
  filter(time.series.length<300) |> 
  mutate(tsl.scaled = time.series.length/longevity.yrs,
         index1 = if_else((tsl.scaled >10| years.surveyed > 10)&years.surveyed > 7,
                          true = 1, false = 0),
         index2 = if_else(completeness.10yrs >= 0.75, true = 1, false = 0),
         index3 = if_else(measure != "Harvest", true = 1, false = 0)) |> 
  left_join(regimeclassification |> ungroup()|>select(plot, group, class), by = c("plot","group")) 


final.data <- all_data_summ |> 
  unnest(ls) |> 
  select(-cumabund,-y_variable_old,-month,-y_error,-n, -tsl.scaled,-index1,-index2,-index3,-digitized,
         -x.round,-x.floor,-x.ceiling, -x.seq)


write_csv(final.data, file = here("output","boomb-bust_final.data.csv"))

#------------------------------------
####function for dynamic modelling the plots####
#------------------------------------


#create a function that takes the data, the break points and plots them, while filtering for Aagaard plots
#because they were already modelled using the the state space model


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

##create a function for all the other analyses.



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



