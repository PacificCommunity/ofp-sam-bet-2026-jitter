# Retain a saved-PAR evaluation in a new directory. No optimisation history,
# Hessian/projection outputs, B0 parity or full-REP identity are asserted.
options(stringsAsFactors = FALSE)

# Scalar and steepness checks match scripts/verify-native-pars.R.
sha256_file <- function(path) {
  output <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("sha256sum failed for ", path, call. = FALSE)
  }
  sub("[[:space:]].*$", "", output[[1L]])
}

par_scalar <- function(path, label) {
  lines <- readLines(path, warn = FALSE)
  index <- which(trimws(lines) == label)
  if (length(index) != 1L || index[[1L]] >= length(lines)) {
    stop("PAR is missing exactly one ", label, ": ", path, call. = FALSE)
  }
  value_index <- index[[1L]] + 1L
  while (value_index <= length(lines) && !nzchar(trimws(lines[[value_index]]))) {
    value_index <- value_index + 1L
  }
  value <- suppressWarnings(as.numeric(trimws(lines[[value_index]])))
  if (length(value) != 1L || !is.finite(value)) {
    stop("PAR has an invalid value after ", label, ": ", path, call. = FALSE)
  }
  value
}

numeric_row_after <- function(lines, marker, source) {
  marker_index <- which(trimws(lines) == marker)
  if (length(marker_index) != 1L || marker_index[[1L]] >= length(lines)) {
    stop(source, " is missing exactly one ", marker, ".", call. = FALSE)
  }
  value_index <- marker_index[[1L]] + 1L
  while (value_index <= length(lines) && !nzchar(trimws(lines[[value_index]]))) {
    value_index <- value_index + 1L
  }
  values <- scan(text = lines[[value_index]], quiet = TRUE)
  if (!length(values) || any(!is.finite(values))) {
    stop(source, " has an invalid numeric row after ", marker, ".", call. = FALSE)
  }
  values
}

assert_fixed_steepness <- function(path) {
  lines <- readLines(path, warn = FALSE)
  growth <- numeric_row_after(lines, "# Seasonal growth parameters", path)
  age_flags <- numeric_row_after(lines, "# age flags", path)
  if (
    length(growth) < 29L || abs(growth[[29L]] - 0.90) > 1e-12 ||
      length(age_flags) < 162L || age_flags[[162L]] != 0
  ) {
    stop(path, " is not fixed at sv(29)=0.90 with age flag 162=0.", call. = FALSE)
  }
  invisible(TRUE)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L || !grepl("^[0-9]+$", args[[1L]])) {
    stop("Usage: make rerun CASE=1 OUT=/absolute/new/directory", call. = FALSE)
  }
  seed <- suppressWarnings(as.integer(args[[1L]]))
  accepted <- as.integer(c(1:3, 5, 7:22, 25:26, 28:30))
  if (is.na(seed) || !(seed %in% accepted)) {
    stop("CASE must be one of the 25 retained seeds.", call. = FALSE)
  }
  if (Sys.info()[["sysname"]] != "Linux" ||
      !(Sys.info()[["machine"]] %in% c("x86_64", "amd64"))) {
    stop("Native MFCL requires Linux x86-64.", call. = FALSE)
  }
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg) != 1L) stop("Could not locate this script.")
  script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
  repo <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
  old_wd <- setwd(repo)
  on.exit(setwd(old_wd), add = TRUE)
  raw_output <- args[[2L]]
  if (!startsWith(raw_output, "/") || basename(raw_output) %in% c("", ".", "..")) {
    stop("OUT must be an absolute, new directory.", call. = FALSE)
  }
  parent <- normalizePath(dirname(raw_output), mustWork = TRUE)
  output <- file.path(parent, basename(raw_output))
  if (output == repo || startsWith(paste0(output, "/"), paste0(repo, "/")) ||
      file.exists(output) || isTRUE(nzchar(Sys.readlink(output)))) {
    stop("OUT must be new and outside the repository.", call. = FALSE)
  }

  mfcl_dir <- file.path(repo, "data", "diagnostic", "mfcl")
  common <- c("bet.frq", "bet.ini", "bet.tag", "bet.age_length",
              "bet.reg_scaling", "mfcl.cfg", "mfclo64", "doitall.sh")
  par <- file.path(repo, "data", "diagnostic", "jitter",
                   paste0("jitter_seed_", seed), paste0("jittered_out_", seed, ".par"))
  result_file <- file.path(dirname(par), "jitter_result.rds")
  reference <- file.path(mfcl_dir, "selectivity-models", "F2.csv")
  sources <- c(file.path(mfcl_dir, common), par, result_file, reference)
  if (!all(file.exists(sources)) || any(vapply(sources, function(p) {
    isTRUE(file.info(p)$isdir) || isTRUE(nzchar(Sys.readlink(p)))
  }, logical(1L)))) stop("The original source bundle is incomplete.", call. = FALSE)
  manifest <- file.path(repo, "data", "SHA256SUMS")
  if (!identical(sha256_file(manifest),
                 "c6709429eff695b0fa1247d3da09e9593637df0434384a8b434f0fbc0540a0a8")) {
    stop("The original data checksum manifest differs.", call. = FALSE)
  }
  lines <- readLines(manifest, warn = FALSE)
  if (any(!grepl("^[0-9a-f]{64}  .+$", lines))) stop("Malformed source checksums.")
  pins <- setNames(substr(lines, 1L, 64L), substring(lines, 67L))
  relative <- substring(sources, nchar(repo) + 2L)
  expected <- unname(pins[relative])
  before <- unname(vapply(sources, sha256_file, character(1L)))
  if (anyNA(expected) || !identical(before, expected)) stop("Original source hashes differ.")
  expected_engine <- "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
  if (!identical(sha256_file(file.path(mfcl_dir, "mfclo64")), expected_engine)) {
    stop("The preserved MFCL executable differs.", call. = FALSE)
  }
  source(file.path(repo, "scripts", "validate-embedded-selectivity.R"), local = TRUE)
  selectivity <- validate_embedded_selectivity(file.path(mfcl_dir, "doitall.sh"), reference)
  ini <- numeric_row_after(readLines(file.path(mfcl_dir, "bet.ini"), warn = FALSE),
                           "# sv(29)", "bet.ini")
  if (length(ini) != 1L || abs(ini[[1L]] - 0.90) > 1e-12) stop("INI steepness differs.")
  result <- readRDS(result_file)
  expected_obj <- as.numeric(result$obj_fun)[[1L]]
  expected_grad <- as.numeric(result$max_grad)[[1L]]
  if (!isTRUE(result$run_completed) || !is.finite(expected_obj) ||
      !is.finite(expected_grad) || abs(expected_grad) > 1e-4 ||
      abs(par_scalar(par, "# Objective function value") - expected_obj) > 1e-8 ||
      abs(par_scalar(par, "# Maximum magnitude gradient value") - expected_grad) > 1e-12 ||
      par_scalar(par, "# MULTIFAN-CL compilation version number") != 2279 ||
      par_scalar(par, "# The number of parameters") != 1997) {
    stop("Saved PAR metadata differs from the original retained fit.", call. = FALSE)
  }
  assert_fixed_steepness(par)
  assert_par_selectivity(par, selectivity)

  if (!dir.create(output, mode = "0700")) stop("Could not reserve the new OUT directory.")
  if (!all(file.copy(file.path(mfcl_dir, common), output, copy.mode = TRUE)) ||
      !file.copy(par, file.path(output, "input.par"), copy.mode = TRUE)) {
    stop("Could not stage the original input files.", call. = FALSE)
  }
  staged <- c(file.path(output, common), file.path(output, "input.par"))
  staged_sha <- unname(vapply(staged, sha256_file, character(1L)))
  if (!identical(staged_sha, before[seq_along(staged)])) stop("Staged input hashes differ.")
  controls <- c("1 1 0", "1 190 1", "1 246 1")
  writeLines(controls, file.path(output, "controls.txt"), useBytes = TRUE)
  setwd(output)
  status <- as.integer(system2(file.path(output, "mfclo64"),
      c("bet.frq", "input.par", "evaluated.par", "-file", "-"),
      stdout = file.path(output, "mfcl.log"), stderr = file.path(output, "mfcl.log"),
      input = controls))
  setwd(repo)
  evaluated <- file.path(output, "evaluated.par")
  rep <- file.path(output, "plot-evaluated.par.rep")
  if (!(status %in% c(0L, 3L)) || !file.exists(evaluated) || file.info(evaluated)$size <= 0 ||
      !file.exists(rep) || file.info(rep)$size <= 0 ||
      !identical(unname(vapply(staged, sha256_file, character(1L))), staged_sha) ||
      !identical(unname(vapply(sources, sha256_file, character(1L))), before)) {
    stop("Native outputs failed, or original/staged inputs changed.", call. = FALSE)
  }
  totals <- grep("^[[:space:]]*Total func[[:space:]]+",
                 readLines(file.path(output, "mfcl.log"), warn = FALSE), value = TRUE)
  observed <- suppressWarnings(as.numeric(sub(".*Total func[[:space:]]+", "", tail(totals, 1L))))
  evaluated_obj <- par_scalar(evaluated, "# Objective function value")
  if (length(observed) != 1L || !is.finite(observed) ||
      abs(observed - expected_obj) > 1e-6 || abs(evaluated_obj - expected_obj) > 1e-6 ||
      par_scalar(evaluated, "# The number of parameters") != 1997) {
    stop("Native objective or parameter-count parity failed.", call. = FALSE)
  }
  assert_fixed_steepness(evaluated)
  assert_par_selectivity(evaluated, selectivity)
  utils::write.csv(data.frame(path = basename(staged), sha256 = staged_sha),
                   file.path(output, "input-checksums.csv"), row.names = FALSE)
  utils::write.csv(data.frame(seed = seed, native_status = status,
      input_par_sha256 = sha256_file(par), expected_objective = expected_obj,
      native_objective = observed, evaluated_objective = evaluated_obj,
      objective_abs_diff = abs(observed - expected_obj), mfcl_sha256 = expected_engine,
      evaluated_par_sha256 = sha256_file(evaluated), plot_rep_sha256 = sha256_file(rep),
      scope = "Objective, PAR metadata and nonempty REP; B0/full REP/Hessian/projections unverified"),
      file.path(output, "native-check.csv"), row.names = FALSE)
  message("Saved seed ", seed, " evaluated; outputs retained in ", output)
}

main()
