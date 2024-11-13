get_exported_functions <- function(package, ignore.names = NULL) {
  funs <- sort(getNamespaceExports(package))
  funs <- grep(".__", funs, fixed = TRUE, invert = TRUE, value = TRUE)
  funs <- setdiff(funs, ignore.names)
  attr(funs, "package") <- package
  return(funs)
}

fuzz <- function(funs, what, ignore.patterns = NULL,
                 ignore.warnings = FALSE) {
  header <- function(count) {
    if (count > 0) return()
    cat("\n\t\U0001f6a8   CAUGHT BY THE FUZZ!   \U0001f6a8\n\n")
  }
  footer <- function(count) {
    if (count > 0) return(invisible())
    cat("\U0001f3c3 You didn't get caught by the fuzz!\n")
  }
  getter <- function() {
    if (is.null(package))
      get
    else
      function(x) getFromNamespace(x, package)
  }

  count <- 0
  ignore.patterns <- paste0(c(ignore.patterns,
                              "is missing, with no default"),
                            collapse = "|")
  package <- attr(funs, "package")
  for (f in funs) {
    fun <- getter()(f)
    tryCatch(capture.output(suppressMessages(fun(what))),
             error = function(e) {
               if (!grepl(f, e) &&
                   !grepl(ignore.patterns, e)) {
                 header(count)
                 cat("FAIL:", f, "(", class(what), ")\n   ", e$message, "\n\n")
                 count <<- count + 1
               }
             },
             warning = function(w) {
               if (!ignore.warnings && !grepl(ignore.patterns, w)) {
                 header(count)
                 cat("WARN:", f, "\n   ", w$message, "\n\n")
                 count <<- count + 1
               }
             })
  }
  footer(count)
}
