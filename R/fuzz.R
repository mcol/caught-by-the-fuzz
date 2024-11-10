get_exported_functions <- function(package, ignore.names) {
  funs <- sort(getNamespaceExports(package))
  funs <- grep(".__", funs, fixed = TRUE, invert = TRUE, value = TRUE)
  funs <- setdiff(funs, ignore.names)
  return(funs)
}

fuzz <- function(funs, what) {
  header <- function(count) if (count == 0) cat("\tCAUGHT BY THE FUZZ!\n\n")
  count <- 0
  for (f in funs) {
    tryCatch(capture.output(suppressMessages(get(f)(what))),
             error = function(e) {
               if (!grepl(f, e) && !grepl("is missing, with no default", e)) {
                 header(count)
                 cat("FAIL:", f, "(", class(what), ")\n   ", e$message, "\n\n")
                 count <<- count + 1
               }
             },
             warning = function(w) {
               header(count)
               cat("WARN:", f, "\n   ", w$message, "\n\n")
               count <<- count + 1
             })
  }
  if (count == 0)
    cat("You didn't get caught by the fuzz\n")
}
