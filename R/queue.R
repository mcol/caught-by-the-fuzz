##===========================================================================
##
## Copyright (c) 2025-2026 Marco Colombo
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
##===========================================================================

setup_queue <- function(funs, what, char, timeout,
                        package, ignore_patterns, ignore_warnings) {
  n_tasks <- length(funs) * length(what)
  daemons <- mirai::info()[["connections"]]
  results <- vector(mode = "list", length = n_tasks)
  running <- list()
  current <- 0L
  finishd <- 0L
  message <- ""

  ## fuzzing engine
  fuzzer <- quote({
    fun <- check_fuzzable(name, package, ignore_deprecated = FALSE)
    is.character(fun) && return(data.frame(res = "SKIP", msg = fun))

    whitelist_and_label <- function(label, msg) {
      res <- if ((label == "WARN" && ignore_warnings) ||
                 is.null(msg) ||
                 grepl(name, msg, fixed = TRUE) ||
                 grepl(ignore_patterns, msg)) {
               "OK"
             } else {
               label
             }
      data.frame(res = res, msg = toString(msg))
    }

    warnings <- NULL
    tryCatch(
        withCallingHandlers({
          fun(what)
          whitelist_and_label("WARN", warnings)
        }, warning = function(w) {
          warnings <<- conditionMessage(w)
        }),
        error = function(e) {
          whitelist_and_label("FAIL", conditionMessage(e))
        })
  })

  ## export common data to the daemons
  ## for performance reason, we pass only the functions we need
  env <- sapply(funs, function(x) .GlobalEnv[[x]])
  env[vapply(env, is.null, logical(1))] <- NULL
  env <- as.environment(env)
  list2env(list(package = package,
                ignore_patterns = ignore_patterns,
                ignore_warnings = ignore_warnings,
                check_fuzzable = check_fuzzable,
                fuzzer = fuzzer), envir = env)
  mirai::everywhere({}, env)

  ## Run tasks and collect the results
  process <- function() {
    cli::cli_progress_bar(format = paste(
                              "{cli::pb_spin} Test input: {cli::pb_bar}|",
                              "{.timestamp {cli::pb_current}/{cli::pb_total}}",
                              " {.strong {strtrim(message, 40)}}"),
                          auto_terminate = FALSE,
                          total = length(what))

    ## process all the tasks
    tic <- Sys.time()
    while (run_tasks()) {
      cli::cli_progress_update(set = finishd)
    }
    elapsed <- difftime(Sys.time(), tic, units = "secs")
    elapsed <- if (elapsed < 1) paste0(round(elapsed * 1000), "ms")
               else paste0(round(elapsed, 1), "s")
    cli::cli_progress_done()
    cli::cli_alert_info("{n_tasks} tests run  {.timestamp {elapsed}}")

    collect_results()
  }

  ## Start running a task
  run_tasks <- function() {
    while (length(running) < daemons && current < n_tasks) {
      fidx <- current %% length(funs) + 1L
      widx <- current %/% length(funs) + 1L
      args <- list(name = funs[[fidx]],
                   what = what[[widx]],
                   char = char[[widx]])
      running[[length(running) + 1]] <<- list(index = current,
                                              mirai = mirai::mirai(eval(fuzzer),
                                                                   .args = args,
                                                                   .timeout = timeout * 1000))
      current <<- current + 1L
    }

    get_tasks()
  }

  ## Get the results from a task
  get_tasks <- function() {
    ## return early if no more tasks are running
    length(running) || return(current < n_tasks)

    ## wait until the first task completion
    mirai::race_mirai(lapply(running, `[[`, "mirai"))

    ## collect the completed runs
    rm.idx <- NULL
    lapply(seq_along(running), function(idx) {
      !mirai::unresolved(running[[idx]]$mirai) || return()
      res <- running[[idx]]$mirai$data
      if (mirai::is_error_value(res) && as.integer(res) == 5L) {
        res <- data.frame(res = "OK",
                          msg = sprintf("Timed out after %d seconds", timeout))
      }

      results[[running[[idx]]$index + 1L]] <<- res
      rm.idx <<- c(rm.idx, idx)
    })

    running[rm.idx] <<- NULL
    if (length(running)) {
      finishd <<- min(sapply(running, `[[`, "index")) %/% length(funs) + 1L
      message <<- char[[finishd]]
    }

    length(running) || current < n_tasks
  }

  ## Collect the final results
  collect_results <- function() {
    res <- do.call(rbind, results)

    ## group the results by input
    lapply(seq_along(what), function(idx) {
      sub <- res[(idx - 1) * length(funs) + seq_along(funs), ]
      rownames(sub) <- NULL
      attr(sub, "what") <- char[[idx]]
      sub
    })
  }

  environment()
}
