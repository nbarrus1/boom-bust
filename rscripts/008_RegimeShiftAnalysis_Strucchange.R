#### this script is for performing the regime shift analysis using the
## strucchange package as one suggested in Anderson et al., 2008-Cell


rm(list = ls())


#------------------------------------------------------------
#### Load libraries
#------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(here)
library(strucchange)

theme_set(theme_bw())


#------------------------------------------------------------
#### Load updated Bayesian-model results
#------------------------------------------------------------

final.set.preds <- readRDS(
  here(
    "output",
    "bayesian_model_checkpoint.rds"
  )
)


#------------------------------------------------------------
#### Create the function for dynamic modelling of breakpoints
#------------------------------------------------------------

fit.breaks <- function(df, brks = 2) {
  
  if (nrow(df) * 0.15 < 2) {
    
    BP_fit <- breakpoints(
      pred ~ 1,
      data = df,
      breaks = brks,
      h = 2
    )
    
    BP_fit
    
  } else {
    
    BP_fit <- breakpoints(
      pred ~ 1,
      data = df,
      breaks = brks
    )
    
    BP_fit
  }
}


fix.aagaard <- function(df, plot) {
  
  if (str_detect(plot, "Aagaard")) {
    
    df |>
      select(
        -x_pred,
        -pred
      ) |>
      rename(
        pred = obs,
        x_pred = x_obs
      ) |>
      select(
        pred,
        x_pred
      ) |>
      drop_na(
        pred
      )
    
  } else {
    
    df
  }
}


#------------------------------------------------------------
#### Breakpoint checkpoint setup
#------------------------------------------------------------

checkpoint_file <- here(
  "output",
  "final_set_brks_checkpoint.rds"
)

final_file <- here(
  "output",
  "final_set_brks.rds"
)

checkpoint_every <- 100L


#------------------------------------------------------------
#### Determine which existing breakpoint file to use
#------------------------------------------------------------

if (file.exists(final_file)) {
  
  old_breakpoint_file <- final_file
  
  message(
    "Loading existing completed breakpoint results."
  )
  
} else if (file.exists(checkpoint_file)) {
  
  old_breakpoint_file <- checkpoint_file
  
  message(
    "Completed breakpoint file not found; ",
    "loading existing checkpoint."
  )
  
} else {
  
  old_breakpoint_file <- NULL
  
  message(
    "No existing breakpoint results found. ",
    "A new breakpoint-analysis object will be initialized."
  )
}


#------------------------------------------------------------
#### Safe checkpoint-saving function
#------------------------------------------------------------

save_breakpoint_checkpoint <- function(object, path) {
  
  temp_path <- paste0(
    path,
    ".tmp"
  )
  
  
  # Write temporary copy first
  saveRDS(
    object,
    temp_path
  )
  
  
  # Make sure it can be read successfully
  test_object <- readRDS(
    temp_path
  )
  
  
  if (nrow(test_object) != nrow(object)) {
    
    unlink(temp_path)
    
    stop(
      "Temporary breakpoint checkpoint failed validation. ",
      "The existing checkpoint was not overwritten."
    )
  }
  
  
  rm(test_object)
  
  
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
      
      unlink(temp_path)
      
      stop(
        "Could not create the .previous breakpoint checkpoint. ",
        "The active checkpoint was not changed."
      )
    }
  }
  
  
  # Replace active checkpoint
  copied <- file.copy(
    from = temp_path,
    to = path,
    overwrite = TRUE
  )
  
  
  unlink(temp_path)
  
  
  if (!copied) {
    
    stop(
      "Could not replace breakpoint checkpoint."
    )
  }
  
  
  invisible(NULL)
}


#------------------------------------------------------------
#### Load previous breakpoint results or initialize new object
#------------------------------------------------------------

if (!is.null(old_breakpoint_file)) {
  
  
  #----------------------------------------------------------
  # Back up existing breakpoint results BEFORE changing them
  #----------------------------------------------------------
  
  backup_file <- here(
    "output",
    paste0(
      "final_set_brks_BACKUP_",
      format(
        Sys.time(),
        "%Y%m%d_%H%M%S"
      ),
      ".rds"
    )
  )
  
  
  backup_success <- file.copy(
    from = old_breakpoint_file,
    to = backup_file,
    overwrite = FALSE
  )
  
  
  if (!backup_success) {
    
    stop(
      "Could not create backup of existing breakpoint results. ",
      "No changes have been made."
    )
  }
  
  
  message(
    "\nExisting breakpoint results backed up to:\n",
    backup_file
  )
  
  
  #----------------------------------------------------------
  # Load previous breakpoint results
  #----------------------------------------------------------
  
  old.brks <- readRDS(
    old_breakpoint_file
  )
  
  
  message(
    "Previous breakpoint rows: ",
    nrow(old.brks)
  )
  
  
  #----------------------------------------------------------
  # Keep only old breakpoint results that still exist in the
  # updated Bayesian dataset
  #----------------------------------------------------------
  
  old.brks.current <- old.brks |>
    semi_join(
      final.set.preds,
      by = c(
        "plot",
        "group"
      )
    )
  
  
  n_removed <- nrow(old.brks) -
    nrow(old.brks.current)
  
  
  message(
    "Old breakpoint rows no longer in current dataset: ",
    n_removed
  )
  
  
  #----------------------------------------------------------
  # Identify genuinely new Bayesian time series
  #----------------------------------------------------------
  
  newdata <- final.set.preds |>
    anti_join(
      old.brks.current,
      by = c(
        "plot",
        "group"
      )
    )
  
  
  message(
    "New time series needing breakpoint analysis: ",
    nrow(newdata)
  )
  
  
  #----------------------------------------------------------
  # Initialize breakpoint columns ONLY for new rows
  #----------------------------------------------------------
  
  if (nrow(newdata) > 0) {
    
    newdata.brks <- newdata |>
      mutate(
        
        brks_fit =
          vector(
            "list",
            n()
          ),
        
        brks_fit_summ =
          vector(
            "list",
            n()
          ),
        
        brks_opt_num =
          NA_integer_,
        
        brks_fit_opt =
          vector(
            "list",
            n()
          ),
        
        breaks.preds =
          vector(
            "list",
            n()
          ),
        
        breakpoint_error =
          NA_character_,
        
        breakpoint_complete =
          FALSE
      )
    
    
    #--------------------------------------------------------
    # Combine old breakpoint results and new initialized rows
    #--------------------------------------------------------
    
    final.set.preds.brks <- bind_rows(
      old.brks.current,
      newdata.brks
    )
    
    
  } else {
    
    final.set.preds.brks <-
      old.brks.current
  }
  
  
} else {
  
  
  #----------------------------------------------------------
  # No previous breakpoint results exist
  #----------------------------------------------------------
  
  final.set.preds.brks <- final.set.preds |>
    mutate(
      
      brks_fit =
        vector(
          "list",
          n()
        ),
      
      brks_fit_summ =
        vector(
          "list",
          n()
        ),
      
      brks_opt_num =
        NA_integer_,
      
      brks_fit_opt =
        vector(
          "list",
          n()
        ),
      
      breaks.preds =
        vector(
          "list",
          n()
        ),
      
      breakpoint_error =
        NA_character_,
      
      breakpoint_complete =
        FALSE
    )
}


#------------------------------------------------------------
#### Sanity checks before breakpoint analysis
#------------------------------------------------------------

if (
  nrow(final.set.preds.brks) !=
  nrow(final.set.preds)
) {
  
  stop(
    "Breakpoint object contains ",
    nrow(final.set.preds.brks),
    " rows but Bayesian results contain ",
    nrow(final.set.preds),
    " rows."
  )
}


# Check plot/group uniqueness

duplicate_keys <- final.set.preds.brks |>
  count(
    plot,
    group
  ) |>
  filter(
    n > 1
  )


if (nrow(duplicate_keys) > 0) {
  
  stop(
    "Duplicate plot/group combinations detected ",
    "in breakpoint-analysis object."
  )
}


# Confirm every breakpoint row corresponds to current data

if (
  nrow(
    final.set.preds.brks |>
    anti_join(
      final.set.preds,
      by = c(
        "plot",
        "group"
      )
    )
  ) > 0
) {
  
  stop(
    "Some breakpoint rows do not exist ",
    "in the current Bayesian dataset."
  )
}


message(
  "\nTotal current time series: ",
  nrow(final.set.preds.brks)
)

message(
  "Breakpoint analyses already attempted: ",
  sum(
    final.set.preds.brks$breakpoint_complete,
    na.rm = TRUE
  )
)

message(
  "Breakpoint analyses still needing to run: ",
  sum(
    is.na(
      final.set.preds.brks$breakpoint_complete
    ) |
      final.set.preds.brks$breakpoint_complete == FALSE
  )
)


#------------------------------------------------------------
#### Save expanded object BEFORE running new analyses
#------------------------------------------------------------

save_breakpoint_checkpoint(
  final.set.preds.brks,
  checkpoint_file
)


message(
  "\nUpdated breakpoint checkpoint saved before analysis."
)


#------------------------------------------------------------
#### Identify exactly which rows need breakpoint analysis
#------------------------------------------------------------

rows_to_run <- which(
  is.na(
    final.set.preds.brks$breakpoint_complete
  ) |
    final.set.preds.brks$breakpoint_complete == FALSE
)


n_to_run <- length(
  rows_to_run
)


message(
  "\n",
  n_to_run,
  " breakpoint analyses need to be run."
)


processed_since_save <- 0L


#------------------------------------------------------------
#### Run breakpoint analysis
#------------------------------------------------------------

for (i in rows_to_run) {
  
  
  message(
    "\n",
    i,
    "/",
    nrow(final.set.preds.brks),
    " | ",
    final.set.preds.brks$plot[i]
  )
  
  
  error.temp <- tryCatch(
    
    {
      
      #------------------------------------------------------
      # Make sure Bayesian predictions exist
      #------------------------------------------------------
      
      if (
        is.null(
          final.set.preds.brks$predictions[[i]]
        )
      ) {
        
        stop(
          "Bayesian predictions are NULL."
        )
      }
      
      
      #------------------------------------------------------
      # Replace Aagaard predictions with observations
      # when necessary
      #------------------------------------------------------
      
      predictions.temp <- fix.aagaard(
        df =
          final.set.preds.brks$predictions[[i]],
        
        plot =
          final.set.preds.brks$plot[i]
      )
      
      
      final.set.preds.brks$predictions[[i]] <-
        predictions.temp
      
      
      #------------------------------------------------------
      # Fit models allowing up to two breakpoints
      #------------------------------------------------------
      
      breaks.fit.temp <- fit.breaks(
        df =
          predictions.temp,
        
        brks =
          2
      )
      
      
      final.set.preds.brks$brks_fit[[i]] <-
        breaks.fit.temp
      
      
      #------------------------------------------------------
      # Summarize candidate breakpoint models
      #------------------------------------------------------
      
      breaks.summary.temp <- summary(
        breaks.fit.temp
      )
      
      
      final.set.preds.brks$brks_fit_summ[[i]] <-
        breaks.summary.temp
      
      
      #------------------------------------------------------
      # Select optimal number of breakpoints using BIC
      #------------------------------------------------------
      
      optimal.breaks.temp <-
        as_tibble(
          pluck(
            breaks.summary.temp,
            "RSS"
          )
        ) |>
        slice(
          2
        ) |>
        pivot_longer(
          cols =
            everything(),
          
          names_to =
            "position",
          
          values_to =
            "BIC"
        ) |>
        mutate(
          position =
            as.integer(
              position
            )
        ) |>
        filter(
          !is.na(BIC),
          
          BIC ==
            min(
              BIC,
              na.rm = TRUE
            )
        ) |>
        slice(
          1
        ) |>
        pull(
          position
        )
      
      
      if (
        length(optimal.breaks.temp) == 0L
      ) {
        
        stop(
          "No optimal breakpoint model could be selected."
        )
      }
      
      
      final.set.preds.brks$brks_opt_num[i] <-
        optimal.breaks.temp
      
      
      #------------------------------------------------------
      # Refit using selected number of breakpoints
      #------------------------------------------------------
      
      optimal.fit.temp <- fit.breaks(
        df =
          predictions.temp,
        
        brks =
          optimal.breaks.temp
      )
      
      
      final.set.preds.brks$brks_fit_opt[[i]] <-
        optimal.fit.temp
      
      
      #------------------------------------------------------
      # Extract rows corresponding to estimated breakpoints
      #------------------------------------------------------
      
      breakpoint.indices.temp <-
        optimal.fit.temp$breakpoints
      
      
      if (
        length(
          breakpoint.indices.temp
        ) == 0L ||
        all(
          is.na(
            breakpoint.indices.temp
          )
        )
      ) {
        
        final.set.preds.brks$breaks.preds[[i]] <-
          predictions.temp[
            0,
            ,
            drop = FALSE
          ]
        
        
      } else {
        
        final.set.preds.brks$breaks.preds[[i]] <-
          predictions.temp |>
          slice(
            breakpoint.indices.temp[
              !is.na(
                breakpoint.indices.temp
              )
            ]
          )
      }
      
      
      #------------------------------------------------------
      # Successful completion
      #------------------------------------------------------
      
      final.set.preds.brks$breakpoint_error[i] <-
        NA_character_
      
      
      final.set.preds.brks$breakpoint_complete[i] <-
        TRUE
      
      
      message(
        "Completed | optimal breakpoints: ",
        optimal.breaks.temp
      )
      
      
      NA_character_
      
    },
    
    
    error = function(e) {
      
      conditionMessage(e)
    }
  )
  
  
  #----------------------------------------------------------
  # Store error
  #----------------------------------------------------------
  
  if (!is.na(error.temp)) {
    
    final.set.preds.brks$breakpoint_error[i] <-
      error.temp
    
    
    # Mark this row as attempted so it will not automatically
    # rerun every time the script resumes.
    final.set.preds.brks$breakpoint_complete[i] <-
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
  
  
  #----------------------------------------------------------
  # Clean temporary objects
  #----------------------------------------------------------
  
  rm(
    error.temp
  )
  
  
  #----------------------------------------------------------
  # Save checkpoint every checkpoint_every attempts
  #----------------------------------------------------------
  
  if (
    processed_since_save >= checkpoint_every
  ) {
    
    save_breakpoint_checkpoint(
      final.set.preds.brks,
      checkpoint_file
    )
    
    
    message(
      "\nBreakpoint checkpoint saved: ",
      sum(
        final.set.preds.brks$breakpoint_complete,
        na.rm = TRUE
      ),
      "/",
      nrow(final.set.preds.brks),
      " attempted."
    )
    
    
    processed_since_save <- 0L
    
    
    gc()
  }
}


#------------------------------------------------------------
#### Save any models completed since last checkpoint
#------------------------------------------------------------

save_breakpoint_checkpoint(
  final.set.preds.brks,
  checkpoint_file
)


#------------------------------------------------------------
#### Save completed breakpoint results
#------------------------------------------------------------

saveRDS(
  final.set.preds.brks,
  final_file
)


message(
  "\nBreakpoint analysis complete."
)


message(
  "Total rows: ",
  nrow(final.set.preds.brks)
)


message(
  "Successfully completed: ",
  sum(
    final.set.preds.brks$breakpoint_complete &
      is.na(
        final.set.preds.brks$breakpoint_error
      ),
    na.rm = TRUE
  )
)


message(
  "Breakpoint errors: ",
  sum(
    final.set.preds.brks$breakpoint_complete &
      !is.na(
        final.set.preds.brks$breakpoint_error
      ),
    na.rm = TRUE
  )
)


message(
  "Still unattempted: ",
  sum(
    is.na(
      final.set.preds.brks$breakpoint_complete
    ) |
      final.set.preds.brks$breakpoint_complete == FALSE
  )
)









