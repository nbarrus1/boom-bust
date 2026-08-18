#### this script is for classifying the time series based off of the regimes deteted
####in step 4

rm(list = ls())
gc()

##load libraries

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)
library(flowchart)


theme_set(theme_bw())

##load in the final set of time series for meta-analysis

final.set.preds.brks <- readRDS(file = here("output","final_set_brks.rds"))
#load(here("output","all_data.Rdata"))



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


##------------------------------------------------------------------
#### Updated regime-classification checkpoint workflow ############
#------------------------------------------------------------------


#------------------------------------------------------------------
#### File paths
#------------------------------------------------------------------

checkpoint_file <- here(
  "output",
  "regimeclassification_checkpoint.rds"
)

final_file <- here(
  "output",
  "regimeclassification.rds"
)

checkpoint_every <- 100L


#------------------------------------------------------------------
#### Determine whether previous classification results exist
#------------------------------------------------------------------

if (file.exists(final_file)) {
  
  old_classification_file <- final_file
  
  message(
    "Loading existing completed regime-classification results."
  )
  
} else if (file.exists(checkpoint_file)) {
  
  old_classification_file <- checkpoint_file
  
  message(
    "Completed classification file not found; ",
    "loading existing checkpoint."
  )
  
} else {
  
  old_classification_file <- NULL
  
  message(
    "No existing regime-classification results found. ",
    "A new classification object will be initialized."
  )
}


#------------------------------------------------------------------
#### Safer checkpoint-saving function
#------------------------------------------------------------------

save_classification_checkpoint <- function(object, path) {
  
  # Write to temporary file first
  temp_path <- paste0(
    path,
    ".tmp"
  )
  
  
  saveRDS(
    object,
    temp_path
  )
  
  
  # Make sure temporary file can be read successfully
  test_object <- readRDS(
    temp_path
  )
  
  
  if (nrow(test_object) != nrow(object)) {
    
    unlink(
      temp_path
    )
    
    stop(
      "Temporary classification checkpoint failed validation. ",
      "The existing checkpoint has not been overwritten."
    )
  }
  
  
  rm(
    test_object
  )
  
  
  # Keep immediately preceding checkpoint
  previous_path <- paste0(
    path,
    ".previous"
  )
  
  
  if (file.exists(path)) {
    
    previous_success <- file.copy(
      from = path,
      to = previous_path,
      overwrite = TRUE
    )
    
    
    if (!previous_success) {
      
      unlink(
        temp_path
      )
      
      stop(
        "Could not create the .previous classification checkpoint. ",
        "The active checkpoint was not changed."
      )
    }
  }
  
  
  # Replace active checkpoint only after temp file was validated
  copied <- file.copy(
    from = temp_path,
    to = path,
    overwrite = TRUE
  )
  
  
  unlink(
    temp_path
  )
  
  
  if (!copied) {
    
    stop(
      "Could not replace the active classification checkpoint."
    )
  }
  
  
  invisible(NULL)
}


#------------------------------------------------------------------
#### Load/update existing regime classifications
#------------------------------------------------------------------

if (!is.null(old_classification_file)) {
  
  
  #----------------------------------------------------------------
  # Back up existing classifications BEFORE changing anything
  #----------------------------------------------------------------
  
  backup_file <- here(
    "output",
    paste0(
      "regimeclassification_BACKUP_",
      format(
        Sys.time(),
        "%Y%m%d_%H%M%S"
      ),
      ".rds"
    )
  )
  
  
  backup_success <- file.copy(
    from = old_classification_file,
    to = backup_file,
    overwrite = FALSE
  )
  
  
  if (!backup_success) {
    
    stop(
      "Could not create backup of existing classification results. ",
      "No changes have been made."
    )
  }
  
  
  message(
    "\nExisting classification results backed up to:\n",
    backup_file
  )
  
  
  #----------------------------------------------------------------
  # Load existing classification results
  #----------------------------------------------------------------
  
  old.classification <- readRDS(
    old_classification_file
  )
  
  
  message(
    "Previous classification rows: ",
    nrow(old.classification)
  )
  
  
  #----------------------------------------------------------------
  # Keep only classifications corresponding to time series that
  # still exist in the CURRENT breakpoint dataset
  #----------------------------------------------------------------
  
  old.classification.current <- old.classification |>
    semi_join(
      final.set.preds.brks,
      by = c(
        "plot",
        "group"
      )
    )
  
  
  n_removed <- nrow(old.classification) -
    nrow(old.classification.current)
  
  
  message(
    "Old classification rows no longer in current dataset: ",
    n_removed
  )
  
  
  #----------------------------------------------------------------
  # Identify new time series that have breakpoint results but
  # have never been classified
  #----------------------------------------------------------------
  
  newdata <- final.set.preds.brks |>
    anti_join(
      old.classification.current,
      by = c(
        "plot",
        "group"
      )
    )
  
  
  message(
    "New time series needing classification: ",
    nrow(newdata)
  )
  
  
  #----------------------------------------------------------------
  # Initialize classification columns ONLY for new rows
  #----------------------------------------------------------------
  
  if (nrow(newdata) > 0) {
    
    newdata.classification <- newdata |>
      
      # These large breakpoint model objects are no longer needed
      # for classification
      select(
        -brks_fit,
        -brks_fit_summ,
        -brks_fit_opt
      ) |>
      
      mutate(
        
        index4 =
          NA_integer_,
        
        class =
          NA_character_,
        
        classification_error =
          NA_character_,
        
        classification_complete =
          FALSE,
        
        classification_attempted =
          FALSE
      )
    
    
    #----------------------------------------------------------------
    # Make sure old object has classification_attempted
    #
    # For old rows:
    # completed classification = attempted
    # stored error             = attempted
    # otherwise                = never attempted
    #----------------------------------------------------------------
    
    if (
      !"classification_attempted" %in%
      names(old.classification.current)
    ) {
      
      old.classification.current <-
        old.classification.current |>
        mutate(
          classification_attempted =
            classification_complete |
            !is.na(classification_error)
        )
    }
    
    
    #----------------------------------------------------------------
    # Combine old completed classifications with new rows
    #----------------------------------------------------------------
    
    regimeclassification <- bind_rows(
      old.classification.current,
      newdata.classification
    )
    
    
  } else {
    
    
    regimeclassification <-
      old.classification.current
    
    
    if (
      !"classification_attempted" %in%
      names(regimeclassification)
    ) {
      
      regimeclassification <-
        regimeclassification |>
        mutate(
          classification_attempted =
            classification_complete |
            !is.na(classification_error)
        )
    }
  }
  
  
} else {
  
  
  #----------------------------------------------------------------
  # No previous classification results exist
  #----------------------------------------------------------------
  
  regimeclassification <- final.set.preds.brks |>
    
    select(
      -brks_fit,
      -brks_fit_summ,
      -brks_fit_opt
    ) |>
    
    mutate(
      
      index4 =
        NA_integer_,
      
      class =
        NA_character_,
      
      classification_error =
        NA_character_,
      
      classification_complete =
        FALSE,
      
      classification_attempted =
        FALSE
    )
}


#------------------------------------------------------------------
#### Sanity checks
#------------------------------------------------------------------


#------------------------------------------------------------------
# Check plot/group uniqueness in current breakpoint data
#------------------------------------------------------------------

duplicate_current_keys <- final.set.preds.brks |>
  count(
    plot,
    group
  ) |>
  filter(
    n > 1
  )


if (nrow(duplicate_current_keys) > 0) {
  
  stop(
    "Duplicate plot/group combinations detected ",
    "in final.set.preds.brks."
  )
}


#------------------------------------------------------------------
# Check plot/group uniqueness in classification object
#------------------------------------------------------------------

duplicate_classification_keys <- regimeclassification |>
  count(
    plot,
    group
  ) |>
  filter(
    n > 1
  )


if (nrow(duplicate_classification_keys) > 0) {
  
  stop(
    "Duplicate plot/group combinations detected ",
    "in regimeclassification."
  )
}


#------------------------------------------------------------------
# Check total row count
#------------------------------------------------------------------

if (
  nrow(regimeclassification) !=
  nrow(final.set.preds.brks)
) {
  
  stop(
    "Classification object contains ",
    nrow(regimeclassification),
    " rows but the current breakpoint dataset contains ",
    nrow(final.set.preds.brks),
    " rows."
  )
}


#------------------------------------------------------------------
# Confirm every current time series is represented exactly once
#------------------------------------------------------------------

missing_from_classification <- final.set.preds.brks |>
  anti_join(
    regimeclassification,
    by = c(
      "plot",
      "group"
    )
  )


if (nrow(missing_from_classification) > 0) {
  
  stop(
    nrow(missing_from_classification),
    " current time series are missing from regimeclassification."
  )
}


obsolete_classification <- regimeclassification |>
  anti_join(
    final.set.preds.brks,
    by = c(
      "plot",
      "group"
    )
  )


if (nrow(obsolete_classification) > 0) {
  
  stop(
    nrow(obsolete_classification),
    " classification rows do not exist in the current breakpoint dataset."
  )
}


#------------------------------------------------------------------
#### Summary before running classification
#------------------------------------------------------------------

message(
  "\nTotal current time series: ",
  nrow(regimeclassification)
)


message(
  "Successful classifications already present: ",
  sum(
    regimeclassification$classification_complete,
    na.rm = TRUE
  )
)


message(
  "Previously attempted with errors: ",
  sum(
    regimeclassification$classification_attempted &
      !regimeclassification$classification_complete,
    na.rm = TRUE
  )
)


message(
  "Never attempted: ",
  sum(
    !regimeclassification$classification_attempted,
    na.rm = TRUE
  )
)


#------------------------------------------------------------------
#### Save updated object BEFORE running any classifications
#------------------------------------------------------------------

save_classification_checkpoint(
  regimeclassification,
  checkpoint_file
)


message(
  "\nUpdated classification checkpoint saved before analysis."
)


#------------------------------------------------------------------
#### Identify exactly which rows have NEVER been attempted
#------------------------------------------------------------------

rows_to_run <- which(
  is.na(
    regimeclassification$classification_attempted
  ) |
    regimeclassification$classification_attempted == FALSE
)


n_to_run <- length(
  rows_to_run
)


message(
  "\n",
  n_to_run,
  " regime classifications need to be run."
)


processed_since_save <- 0L


#------------------------------------------------------------------
#### Run regime classification
#------------------------------------------------------------------

for (i in rows_to_run) {
  
  
  message(
    "\n",
    i,
    "/",
    nrow(regimeclassification),
    " | ",
    regimeclassification$species.names[i]
  )
  
  
  error.temp <- tryCatch(
    
    {
      
      #------------------------------------------------------------
      # Verify breakpoint analysis was completed successfully
      #------------------------------------------------------------
      
      if (
        "breakpoint_complete" %in%
        names(regimeclassification)
      ) {
        
        if (
          !isTRUE(
            regimeclassification$breakpoint_complete[i]
          )
        ) {
          
          stop(
            "Breakpoint analysis was not completed for this time series."
          )
        }
      }
      
      
      if (
        "breakpoint_error" %in%
        names(regimeclassification)
      ) {
        
        if (
          !is.na(
            regimeclassification$breakpoint_error[i]
          )
        ) {
          
          stop(
            paste0(
              "Breakpoint analysis contains an error: ",
              regimeclassification$breakpoint_error[i]
            )
          )
        }
      }
      
      
      #------------------------------------------------------------
      # Verify Bayesian predictions exist
      #------------------------------------------------------------
      
      if (
        is.null(
          regimeclassification$predictions[[i]]
        )
      ) {
        
        stop(
          "Bayesian predictions are NULL."
        )
      }
      
      
      #------------------------------------------------------------
      # Add regime classifications to nested prediction tibble
      #------------------------------------------------------------
      
      if (
        !is.na(
          regimeclassification$brks_opt_num[i]
        ) &&
        regimeclassification$brks_opt_num[i] > 0
      ) {
        
        
        predictions_temp <- regime.means(
          df.original =
            regimeclassification$predictions[[i]],
          
          df.breaks =
            regimeclassification$breaks.preds[[i]]
        )
        
        
        index4_temp <- 1L
        
        
      } else {
        
        
        # Retain original nested prediction tibble
        predictions_temp <-
          regimeclassification$predictions[[i]]
        
        
        index4_temp <- 0L
      }
      
      
      #------------------------------------------------------------
      # Classify the time series
      #------------------------------------------------------------
      
      class_temp <- classification_scheme(
        df =
          predictions_temp,
        
        n_breaks =
          regimeclassification$brks_opt_num[i],
        
        longevity =
          regimeclassification$longevity.yrs[i]
      )
      
      
      #------------------------------------------------------------
      # Store successful results
      #------------------------------------------------------------
      
      regimeclassification$predictions[[i]] <-
        predictions_temp
      
      
      regimeclassification$index4[i] <-
        index4_temp
      
      
      regimeclassification$class[i] <-
        class_temp
      
      
      regimeclassification$classification_error[i] <-
        NA_character_
      
      
      regimeclassification$classification_complete[i] <-
        TRUE
      
      
      regimeclassification$classification_attempted[i] <-
        TRUE
      
      
      message(
        "Completed | class: ",
        if_else(
          is.na(class_temp),
          "NA",
          class_temp
        )
      )
      
      
      NA_character_
      
      
    },
    
    
    error = function(e) {
      
      conditionMessage(e)
    }
  )
  
  
  #----------------------------------------------------------------
  # Record failed attempt
  #----------------------------------------------------------------
  
  if (!is.na(error.temp)) {
    
    
    regimeclassification$classification_error[i] <-
      error.temp
    
    
    regimeclassification$classification_complete[i] <-
      FALSE
    
    
    regimeclassification$classification_attempted[i] <-
      TRUE
    
    
    message(
      "ERROR in row ",
      i,
      ": ",
      error.temp
    )
  }
  
  
  processed_since_save <-
    processed_since_save + 1L
  
  
  rm(
    error.temp
  )
  
  
  #----------------------------------------------------------------
  # Save every checkpoint_every ATTEMPTS
  #----------------------------------------------------------------
  
  if (
    processed_since_save >= checkpoint_every
  ) {
    
    
    save_classification_checkpoint(
      regimeclassification,
      checkpoint_file
    )
    
    
    message(
      "\nClassification checkpoint saved: ",
      sum(
        regimeclassification$classification_attempted,
        na.rm = TRUE
      ),
      "/",
      nrow(regimeclassification),
      " attempted."
    )
    
    
    processed_since_save <- 0L
    
    
    gc()
  }
}


#------------------------------------------------------------------
#### Final checkpoint
#------------------------------------------------------------------

save_classification_checkpoint(
  regimeclassification,
  checkpoint_file
)


#------------------------------------------------------------------
#### Save completed classification object
#------------------------------------------------------------------

saveRDS(
  regimeclassification,
  final_file
)


#------------------------------------------------------------------
#### Final summary
#------------------------------------------------------------------

message(
  "\nRegime classification complete."
)


message(
  "Total rows: ",
  nrow(regimeclassification)
)


message(
  "Successfully classified: ",
  sum(
    regimeclassification$classification_complete,
    na.rm = TRUE
  )
)


message(
  "Classification errors: ",
  sum(
    regimeclassification$classification_attempted &
      !regimeclassification$classification_complete,
    na.rm = TRUE
  )
)


message(
  "Still unattempted: ",
  sum(
    !regimeclassification$classification_attempted,
    na.rm = TRUE
  )
)

