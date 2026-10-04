# Check filesystem links only; no model data or native execution.
expressions <- parse(file = "reproduce/rerun-saved.R")
selected <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name("<-")) &&
                     identical(x[[2L]], as.name("has_symlink")), as.list(expressions))
stopifnot(length(selected) == 1L)
helpers <- new.env(parent = baseenv())
eval(selected[[1L]], envir = helpers)
root <- tempdir()
fresh <- file.path(root, "new-output")
regular <- file.path(root, "original-input")
live <- file.path(root, "input-link")
broken <- file.path(root, "broken-link")
writeLines("filesystem fixture", regular)
stopifnot(!helpers$has_symlink(fresh), !helpers$has_symlink(regular),
          file.symlink(regular, live), file.symlink(fresh, broken),
          helpers$has_symlink(live), helpers$has_symlink(broken),
          !file.exists(fresh))
cat("Saved-PAR path checks passed; no model execution.\n")
