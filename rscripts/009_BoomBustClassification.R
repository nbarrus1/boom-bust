#### this script is for classifying the time series based off of the regimes deteted
####in step 4

rm(list = ls())

##load libraries

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)
library(flowchart)


theme_set(theme_bw())

##load in the final set of time series for meta-analysis

load(file = here("output","final_set_brks.Rdata"))
load(here("output","all_data.Rdata"))



###specify functions




regime.means <- function(df.original, df.breaks) {
  
  if(nrow(df.breaks)==1) {
    
    df.original |> 
      mutate(regime.class = if_else(x_pred <= df.breaks$x_pred[1], true = "reg1", false = "reg2"),
             rate = log(lead(pred)/pred)) |> 
      group_by(regime.class) |> 
      mutate(regime.ave = mean(pred, na.rm = T),
             regime.max = max(pred, na.rm = T),
             regime.min = min(pred, na.rm = T),
             regime.rate.max = max(rate, na.rm = T),
             regime.rate.min = min(rate, na.rm = T))
    
  } else if(nrow(df.breaks)==2) {
    
    df.original |> 
      mutate(regime.class = case_when(x_pred <=df.breaks$x_pred[1] ~ "reg1",
                                      x_pred > df.breaks$x_pred[1] & x_pred <= df.breaks$x_pred[2]~"reg2",
                                      x_pred > df.breaks$x_pred[2]~"reg3",
                                      .default = NA_character_),
             rate = log(lead(pred)/pred)) |> 
      group_by(regime.class) |> 
      mutate(regime.ave = mean(pred, na.rm = T),
             regime.max = max(pred, na.rm = T),
             regime.min = min(pred, na.rm = T),
             regime.rate.max = max(rate, na.rm = T),
             regime.rate.min = min(rate, na.rm = T))
  }
  
}





classification_scheme <- function (df, n_breaks, longevity) {
  
  
  if(n_breaks > 0) {
  
##calculate the maximum percent decline  
    decline_mag = df |> 
      ungroup() |> 
      group_by(regime.class) |> 
      summarise(regime.max = mean(regime.max, na.rm =T),
                regime.min = mean(regime.min, na.rm = T)) |> 
      mutate(regime_min_lead = lead(regime.min)) |> 
      ungroup() |> 
      filter(regime.max == max(df$regime.max)) |> 
      mutate(perc_decline = (regime.max-regime_min_lead)/regime.max) |>
      summarise(perc_decline = max(perc_decline,na.rm = T)) |> 
      pull(perc_decline)
    
##identify the types of regimes from the using the regime means   
 
    
    regime.means <- df |> 
      ungroup() |> 
      group_by(regime.class) |> 
      summarise(regime.max = mean(regime.max, na.rm =T),
                regime.min = mean(regime.min, na.rm = T)) 
    
    
    n_regimes <- length(regime.means$regime.class)
    
    regime_type <- case_when(
      #find timeseries that have a 3 regimes with lag.growth, high n and low n phase and name them lowhighlow
                             (n_regimes == 3) &
                             (lead(regime.means$regime.max) > regime.means$regime.max)[1] &
                             (lag(regime.means$regime.max) > regime.means$regime.max)[3]~"lowhighlow",
      #find timeseries that have a 3 regimes lag.growth, high n, and higher n and name them lowhighhigh
                             (n_regimes == 3) &
                             (lead(regime.means$regime.max) > regime.means$regime.max)[1] &
                             !(lag(regime.means$regime.max) > regime.means$regime.max)[3]~"lowhighhigh",
      #find timeseries that have a 2 regimes lag.growth and high n and name them lowhigh
                             (n_regimes == 2) &
                             (lead(regime.means$regime.max) > regime.means$regime.max)[1] &
                             !(lag(regime.means$regime.max) > regime.means$regime.max)[2]~"lowhigh",                       
      #find timeseries that have a 2 regimes high and low n and name them highlow
                             (n_regimes == 2) &
                             !(lead(regime.means$regime.max) > regime.means$regime.max)[1] &
                             (lag(regime.means$regime.max) > regime.means$regime.max)[2]~"highlow",
      #find timeseries that have a 3 regimes high n, low n, high n and name them highlowhigh
                             (n_regimes == 3) &
                             !(lead(regime.means$regime.max) > regime.means$regime.max)[1] &
                             !(lag(regime.means$regime.max) > regime.means$regime.max)[3]~"highlowhigh",
      #find timeseries that have a 3 regimes high n, low n, low n and name them highlowhigh
                             (n_regimes == 3) &
                             !(lead(regime.means$regime.max) > regime.means$regime.max)[1] &
                             (lag(regime.means$regime.max) > regime.means$regime.max)[3]~"highlowlow" 
                             )
      
###find the rate of increase for regimes that have lag/growth and high n phase
   max.year <-  df$x_pred[df$pred == max(df$pred, na.rm = T)]
   
   max.year.hlh <- df$x_pred[df$pred == max(df$pred[df$regime.class=="reg1"], na.rm = T)]
   

   rate_increase <-  case_when(
     #for regimes that have a lag.growth and high n phase
                regime_type == "lowhighlow"|regime_type == "lowhighhigh"|
                regime_type == "lowhigh"|regime_type=="highlowhigh" ~  
                  df |> 
                     ungroup() |> 
                     mutate(lambda = lead(pred)/pred*longevity,                #lambda scaled by longevity
                            split = case_when(x_pred < max.year ~ "before_max",
                                         x_pred > max.year ~ "after_max",
                                         x_pred == max.year ~ "max")) |> 
                    filter(split == "before_max"|split == "max") |> 
                    summarise(lambda.max = max(lambda, na.rm = T)) |> 
                    pull(lambda.max),
      #for regimes that don't have a lag.growth phase          
                regime_type == "highlow"|regime_type=="highlowlow"~
                 NA_real_     )
    

###find the minimum rate of increase for regimes that have highn and low n phases
   rate_decline <-  case_when(
     #for regimes that have a lag.growth and high n phase
     regime_type == "lowhighlow"|regime_type == "highlow"|regime_type=="highlowlow" ~  
       df |> 
       ungroup() |> 
       mutate(lambda = lead(pred)/pred/longevity,                      #lambda scaled by longevity
              split = case_when(x_pred < max.year ~ "before_max",
                                x_pred > max.year ~ "after_max",
                                x_pred == max.year ~ "max")) |> 
       filter(split == "after_max"|split == "max") |> 
       summarise(lambda.min = min(lambda, na.rm = T)) |> 
       pull(lambda.min),
     #for regimes that have a high n then low n then high n phase
     regime_type == "highlowhigh"~
       df |> 
       ungroup() |> 
       filter(regime.class != "reg3") |> 
       mutate(lambda = lead(pred)/pred/longevity,                      #lambda scaled by longevity
              split = case_when(x_pred < max.year.hlh ~ "before_max",
                                x_pred > max.year.hlh ~ "after_max",
                                x_pred == max.year ~ "max")) |> 
       filter(split == "after_max"|split == "max") |> 
       summarise(lambda.min = min(lambda, na.rm = T)) |> 
       pull(lambda.min),
     #for regimes that don't have a lag.growth phase          
     regime_type == "lowhigh"|regime_type == "lowhighhigh"~
       NA_real_     )
   

##find the number of observation within the low n phase
    n_length <- df |> 
      group_by(regime.class) |> 
      summarise(n = n()) |>
      slice(length(unique(df$regime.class))) |> 
      pull(n) 
    
##find the number of leads needed for the sustained calculation    
    n_leads = n_length-1

##calculate the number of consecutive values across the sequence of the low n phase
##that are  >= 90% smaller than the maximum
   n_sustained <- max(df |>
      filter(regime.class == unique(df$regime.class)[length(unique(df$regime.class))]) |> 
      ungroup() |> 
      select(pred) |> 
      mutate(pred = if_else(pred <= max(df$pred)*0.1, true = 1, false = 0)) |> 
      mutate(across(
        everything(),
        .fns = list(!!!setNames(
          lapply(seq_len(n_leads), function(i) ~lead(., i)),
          paste0("lead_", seq_len(n_leads))
        )))) |> 
      mutate(nrow = 1:n()) |> 
      pivot_longer(cols = 1:(n_leads+1)) |> 
      group_by(nrow) |> 
      summarise(n_below = sum(value,na.rm = T)) |> 
      pull(n_below))

   
##determine if the last value of the sequence in the low n phase is >= 90% smaller than
## the max value

decline_lastposition <- df|> 
  filter(regime.class == unique(df$regime.class)[length(unique(df$regime.class))]) |> 
  ungroup() |> 
  select(pred) |> 
  mutate(if.declined = if_else(pred <= max(df$pred)*0.1, true = 1, false = 0)) |> 
  slice(n_length) |> 
  pull(if.declined)


###perform the classification using our definition    
    case_when(decline_mag == (-Inf)~"\nestablished",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_increase >= 2&
                rate_decline <= 0.5&
                length(unique(df$regime.class))==3 ~ "boom &\nbust",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_increase >= 2&
                rate_decline <= 0.5&
                decline_lastposition > 0 &
                length(unique(df$regime.class))==3 ~ "boom &\n sust. unk",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_decline <= 0.5&
                is.na(rate_increase)&
                length(unique(df$regime.class))>=2 ~ "unk rate &\nbust",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_decline <= 0.5&
                is.na(rate_increase)&
                 decline_lastposition > 0 &
                length(unique(df$regime.class))>=2~ "unk rate &\nsust. unk",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_increase >= 2&
                rate_decline <= 0.5&
                !(decline_lastposition > 0) &
                length(unique(df$regime.class))==3 ~ "\novershoot",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_increase < 2&
                rate_decline <= 0.5&
                !(decline_lastposition > 0) &
                length(unique(df$regime.class))==3 ~ "\novershoot",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_decline <= 0.5&
                is.na(rate_increase)&
                !(decline_lastposition) > 0 &
                length(unique(df$regime.class))>=2~ "\novershoot",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_increase >= 2&
                rate_decline > 0.5&
                length(unique(df$regime.class))==3 ~ "boom &\nbust",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_increase < 2&
                rate_decline > 0.5&
                length(unique(df$regime.class))==3 ~ "boom &\nbust",
              decline_mag>=0.9 &
                n_sustained>=3&
                is.na(rate_increase)&
                rate_decline > 0.5&
                length(unique(df$regime.class))>=2~ "unk rate &\nbust",
              decline_mag>=0.9 &
                !(n_sustained>=3)&
                rate_increase <2&
                rate_decline <= 0.5&
                decline_lastposition > 0 &
                length(unique(df$regime.class))==3 ~ "boom &\n sust. unk",
              decline_mag>=0.9 &
                n_sustained>=3&
                rate_increase < 2&
                rate_decline <= 0.5&
                length(unique(df$regime.class))>=2 ~ "boom &\nbust",
              decline_mag<0.9~"\novershoot")
    
  } else {
    
    NA_character_
    
    
  }
}


#------------------------------------------------------------------
####create the function for dynamic modelling of the break-points###
#------------------------------------------------------------------

regimeclassification <- final.set.preds.brks |> 
  select(-brks_fit,-brks_fit_summ,-brks_fit_opt) |> 
  mutate(predictions = if_else(brks_opt_num > 0,
                      true = map2(.x = predictions, .y = breaks.preds, .f = regime.means),
                      false = ls),
         index4 = if_else(brks_opt_num > 0, true = 1, false = 0),
         class = pmap_chr(list(df = predictions, n_breaks = brks_opt_num, longevity = longevity.yrs),
                          .f = classification_scheme))


save(regimeclassification, file = here("output","regimeclassification.Rdata"))


table(regimeclassification$brks_opt_num>0,
      is.na(regimeclassification$class))

table(regimeclassification$class)

###reading list

reading.list <- regimeclassification |> 
  filter(class == "boom &\nbust") |> 
  ungroup() |> 
  select(title, author,species.names,class)
