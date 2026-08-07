# Safe project synchronisation ---------------------------------------------

run_safe_update <- function() {
  if (
    !file.exists("_quarto.yml") ||
    !dir.exists("modules") ||
    !dir.exists("support")
  ) {
    stop(
      "Run this function from the Scup School project root."
    )
  }
  

  update_steps <- c(
    "dev/sync-support-skeleton.R",
    "dev/apply-core-page-shell.R",
    "dev/apply-module-overview-width.R",
    "dev/apply-meeting-preparation-shell.R",
    "dev/apply-meeting-pack-shell.R",
    "dev/apply-milestone-shell.R",
    "dev/apply-support-article-shell.R",
    "dev/update-module-overview-routes.R",
    "dev/update-module-navigation.R"
  )
  
  steps <- update_steps
  
  required_inputs <- c(
    "_quarto.yml",
    "dev/course-manifest.csv",
    "dev/support-manifest.csv"
  )
  
  missing_inputs <- required_inputs[
    !file.exists(required_inputs)
  ]
  
  if (length(missing_inputs) > 0) {
    stop(
      "Required project inputs are missing:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  }
  
  missing_steps <- steps[
    !file.exists(steps)
  ]
  
  if (length(missing_steps) > 0) {
    stop(
      "Safe-update scripts are missing:\n",
      paste(
        missing_steps,
        collapse = "\n"
      )
    )
  }
  
  message(
    "\nScup School safe update\n",
    "=======================\n"
  )
  
  message(
    "Production manifests will be used as source data.\n"
  )
  
  results <- vector(
    "list",
    length(steps)
  )
  
  names(results) <- steps
  
  for (i in seq_along(steps)) {
    path <- steps[[i]]
    
    message(
      sprintf(
        "[%02d/%02d] %s",
        i,
        length(steps),
        path
      )
    )
    
    started <- proc.time()[["elapsed"]]
    
    step_environment <- new.env(
      parent = globalenv()
    )
    
    tryCatch(
      {
        sys.source(
          path,
          envir = step_environment,
          keep.source = FALSE
        )
      },
      error = function(error) {
        stop(
          "\nSafe update stopped in:\n",
          path,
          "\n\n",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
    
    elapsed <- proc.time()[["elapsed"]] -
      started
    
    results[[i]] <- data.frame(
      step = path,
      seconds = round(
        elapsed,
        3
      ),
      stringsAsFactors = FALSE
    )
  }
  
  results <- do.call(
    rbind,
    results
  )
  
  row.names(results) <- NULL
  
  message(
    "\nSafe update completed successfully.\n"
  )
  
  print(
    results,
    row.names = FALSE
  )
  
  invisible(results)
}