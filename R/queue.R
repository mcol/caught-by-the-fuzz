setup_queue <- function(funs, what, char, timeout) {
  n_tasks <- length(funs) * length(what)
  daemons <- mirai::info()[["connections"]]
  results <- vector(mode = "list", length = n_tasks)
  running <- list()
  current <- 0L
  finishd <- 0L
  message <- ""

  ## silence note from R CMD check, `fuzzer` is already on the daemons
  fuzzer <- NULL

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

    results
  }

  ## Start running a task
  run_tasks <- function() {
    while (length(running) < daemons && current < n_tasks) {
      fidx <- current %% length(funs) + 1L
      widx <- current %/% length(funs) + 1L
      args <- list(fun_name = funs[[fidx]],
                   what = what[[widx]],
                   char = char[[widx]])
      running[[length(running) + 1]] <<- list(index = current,
                                              mirai = mirai::mirai(eval(fuzzer),
                                                                   .args = args,
                                                                   .timeout = timeout * 1000))
      current <<- current + 1L
    }

    get_results()
  }

  ## Get the results from a task
  get_results <- function() {
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

  environment()
}
