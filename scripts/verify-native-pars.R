options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) {
  stop("Usage: ./verify-native-pars [MANIFEST.csv|-]", call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Could not locate this validation script.", call. = FALSE)
}
script_file <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
setwd(repo)
source(file.path(repo, "scripts", "validate-embedded-selectivity.R"))

manifest_file <- if (length(args)) args[[1L]] else {
  file.path("data", "diagnostic", "native-par-validation.csv")
}
write_manifest <- !identical(manifest_file, "-")
if (write_manifest) {
  manifest_file <- normalizePath(
    file.path(repo, manifest_file), mustWork = FALSE
  )
  expected_parent <- normalizePath(file.path(repo, "data", "diagnostic"))
  if (!identical(dirname(manifest_file), expected_parent)) {
    stop("The manifest must be written directly under data/diagnostic/.", call. = FALSE)
  }
}

accepted_seeds <- as.integer(c(1:3, 5, 7:22, 25:26, 28:30))
mfcl_dir <- file.path(repo, "data", "diagnostic", "mfcl")
mfcl <- file.path(mfcl_dir, "mfclo64")
common_names <- c(
  "bet.frq", "bet.ini", "bet.tag", "bet.age_length",
  "bet.reg_scaling", "mfcl.cfg", "mfclo64"
)
common_files <- file.path(mfcl_dir, common_names)
doitall <- file.path(mfcl_dir, "doitall.sh")
selectivity_reference <- file.path(mfcl_dir, "selectivity-models", "F2.csv")
if (
  !all(file.exists(c(common_files, doitall, selectivity_reference))) ||
    any(file.access(c(mfcl, doitall), mode = 1L) != 0L)
) {
  stop("The shared native MFCL bundle is incomplete or not executable.", call. = FALSE)
}
embedded_selectivity <- validate_embedded_selectivity(
  doitall,
  selectivity_reference
)

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

ini_values <- numeric_row_after(
  readLines(file.path(mfcl_dir, "bet.ini"), warn = FALSE),
  "# sv(29)",
  file.path(mfcl_dir, "bet.ini")
)
if (length(ini_values) != 1L || abs(ini_values[[1L]] - 0.90) > 1e-12) {
  stop("The public bet.ini must itself contain sv(29)=0.90.", call. = FALSE)
}

par_files <- Sys.glob(file.path(
  repo, "data", "diagnostic", "jitter", "jitter_seed_*", "jittered_out_*.par"
))
par_seeds <- suppressWarnings(as.integer(sub(
  "^jittered_out_([0-9]+)[.]par$", "\\1", basename(par_files)
)))
if (
  length(par_files) != length(accepted_seeds) || anyNA(par_seeds) ||
    !identical(sort(par_seeds), accepted_seeds) || anyDuplicated(par_seeds)
) {
  stop("Recovered PAR files do not match the exact 25 accepted seeds.", call. = FALSE)
}

result_files <- Sys.glob(file.path(
  repo, "data", "diagnostic", "jitter", "jitter_seed_*", "jitter_result.rds"
))
if (length(result_files) != 30L) {
  stop("Expected 30 compact jitter result payloads.", call. = FALSE)
}
results <- lapply(result_files, readRDS)
result_seeds <- vapply(results, function(x) as.integer(x$seed), integer(1L))
if (!identical(sort(result_seeds), 1:30) || anyDuplicated(result_seeds)) {
  stop("Compact jitter result seeds are not exactly 1 through 30.", call. = FALSE)
}
names(results) <- as.character(result_seeds)

scratch_root <- tempfile(pattern = "bet-jitter-native-")
if (!dir.create(scratch_root, mode = "0700")) {
  stop("Could not create native validation scratch directory.", call. = FALSE)
}
scratch_root <- normalizePath(scratch_root, mustWork = TRUE)
expected_tmp <- normalizePath(tempdir(), mustWork = TRUE)
if (
  !identical(dirname(scratch_root), expected_tmp) ||
    !grepl("^bet-jitter-native-[[:alnum:]]+$", basename(scratch_root)) ||
    nzchar(Sys.readlink(scratch_root))
) {
  stop("Refusing an unexpected scratch directory: ", scratch_root, call. = FALSE)
}

safe_remove_run_dir <- function(path, seed) {
  resolved <- normalizePath(path, mustWork = TRUE)
  if (
    !identical(dirname(resolved), scratch_root) ||
      !identical(basename(resolved), paste0("seed-", seed)) ||
      nzchar(Sys.readlink(resolved)) || !isTRUE(file.info(resolved)$isdir)
  ) {
    stop("Refusing to remove an unexpected run directory: ", resolved, call. = FALSE)
  }
  unlink(resolved, recursive = TRUE, force = FALSE)
  if (file.exists(resolved)) {
    stop("Could not remove native validation scratch: ", resolved, call. = FALSE)
  }
}

version_dir <- file.path(scratch_root, "version")
if (!dir.create(version_dir, mode = "0700")) {
  stop("Could not create MFCL version scratch directory.", call. = FALSE)
}
old_wd <- setwd(version_dir)
version_output <- system2(mfcl, "--version", stdout = TRUE, stderr = TRUE)
version_status <- attr(version_output, "status")
setwd(old_wd)
version_match <- regmatches(
  version_output,
  regexpr("[0-9]+([.][0-9]+){3}", version_output)
)
version_match <- unique(version_match[nzchar(version_match)])
if (
  (!is.null(version_status) && version_status != 0L) ||
    length(version_match) != 1L || !identical(version_match, "2.2.7.9")
) {
  stop("The bundled executable is not native MFCL 2.2.7.9.", call. = FALSE)
}
version_resolved <- normalizePath(version_dir, mustWork = TRUE)
if (
  !identical(dirname(version_resolved), scratch_root) ||
    !identical(basename(version_resolved), "version") ||
    nzchar(Sys.readlink(version_resolved))
) {
  stop("Refusing to remove an unexpected version directory.", call. = FALSE)
}
unlink(version_resolved, recursive = TRUE, force = FALSE)
if (file.exists(version_resolved)) {
  stop("Could not remove MFCL version scratch directory.", call. = FALSE)
}

mfcl_sha <- sha256_file(mfcl)
expected_mfcl_sha <- "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
if (!identical(mfcl_sha, expected_mfcl_sha)) {
  stop("The bundled native MFCL executable differs from the verified source.", call. = FALSE)
}
records <- vector("list", length(accepted_seeds))
controls <- c("1 1 0", "1 190 1", "1 246 1")

for (position in seq_along(accepted_seeds)) {
  seed <- accepted_seeds[[position]]
  par_file <- file.path(
    repo, "data", "diagnostic", "jitter", paste0("jitter_seed_", seed),
    paste0("jittered_out_", seed, ".par")
  )
  result <- results[[as.character(seed)]]
  expected_obj <- as.numeric(result$obj_fun)[[1L]]
  expected_grad <- as.numeric(result$max_grad)[[1L]]
  if (
    !isTRUE(result$run_completed) || !is.finite(expected_obj) ||
      !is.finite(expected_grad) || abs(expected_grad) > 1e-4
  ) {
    stop("Seed ", seed, " is not an accepted compact jitter result.", call. = FALSE)
  }

  par_obj <- par_scalar(par_file, "# Objective function value")
  par_grad <- par_scalar(par_file, "# Maximum magnitude gradient value")
  assert_fixed_steepness(par_file)
  assert_par_selectivity(par_file, embedded_selectivity)
  par_compile_version <- par_scalar(
    par_file, "# MULTIFAN-CL compilation version number"
  )
  par_n_parameters <- par_scalar(par_file, "# The number of parameters")
  if (
    abs(par_obj - expected_obj) > 1e-8 ||
      abs(par_grad - expected_grad) > 1e-12 ||
      !identical(par_compile_version, 2279) ||
      !identical(par_n_parameters, 1997)
  ) {
    stop("Recovered PAR metadata differs from the compact result for seed ", seed, ".", call. = FALSE)
  }

  run_dir <- file.path(scratch_root, paste0("seed-", seed))
  if (!dir.create(run_dir, mode = "0700")) {
    stop("Could not create native run directory for seed ", seed, ".", call. = FALSE)
  }
  if (!all(file.copy(common_files, run_dir, copy.mode = TRUE))) {
    stop("Could not stage the native MFCL bundle for seed ", seed, ".", call. = FALSE)
  }
  staged_par <- file.path(run_dir, "input.par")
  if (!file.copy(par_file, staged_par, copy.mode = TRUE)) {
    stop("Could not stage recovered PAR for seed ", seed, ".", call. = FALSE)
  }
  input_sha <- sha256_file(staged_par)

  old_wd <- setwd(run_dir)
  log_file <- file.path(run_dir, "mfcl.log")
  native_status <- system2(
    mfcl,
    c("bet.frq", "input.par", "evaluated.par", "-file", "-"),
    stdout = log_file,
    stderr = log_file,
    input = controls
  )
  setwd(old_wd)
  native_status <- as.integer(native_status)
  evaluated_par <- file.path(run_dir, "evaluated.par")
  plot_rep <- file.path(run_dir, "plot-evaluated.par.rep")
  if (
    !(native_status %in% c(0L, 3L)) ||
      !file.exists(evaluated_par) || file.info(evaluated_par)$size <= 0 ||
      !file.exists(plot_rep) || file.info(plot_rep)$size <= 0 ||
      !identical(sha256_file(staged_par), input_sha)
  ) {
    stop("Native MFCL did not load and evaluate recovered seed ", seed, ".", call. = FALSE)
  }

  log_lines <- readLines(log_file, warn = FALSE)
  total_lines <- grep("^[[:space:]]*Total func[[:space:]]+", log_lines, value = TRUE)
  native_obj <- suppressWarnings(as.numeric(sub(
    ".*Total func[[:space:]]+", "", tail(total_lines, 1L)
  )))
  evaluated_obj <- par_scalar(evaluated_par, "# Objective function value")
  assert_fixed_steepness(evaluated_par)
  assert_par_selectivity(evaluated_par, embedded_selectivity)
  if (
    length(total_lines) < 1L || length(native_obj) != 1L || !is.finite(native_obj) ||
      abs(native_obj - expected_obj) > 1e-6 ||
      abs(evaluated_obj - expected_obj) > 1e-6
  ) {
    stop("Native objective parity failed for recovered seed ", seed, ".", call. = FALSE)
  }

  records[[position]] <- data.frame(
    seed = seed,
    par_file = file.path(
      "data", "diagnostic", "jitter", paste0("jitter_seed_", seed),
      paste0("jittered_out_", seed, ".par")
    ),
    par_sha256 = sha256_file(par_file),
    expected_obj_fun = expected_obj,
    native_obj_fun = native_obj,
    objective_abs_diff = abs(native_obj - expected_obj),
    max_grad = expected_grad,
    native_status = native_status,
    evaluated_par_created = TRUE,
    plot_rep_created = TRUE,
    mfcl_version = version_match,
    mfcl_sha256 = mfcl_sha,
    stringsAsFactors = FALSE
  )
  message(
    sprintf(
      "Native MFCL verified seed %d: objective %.12f (|diff| %.3g), status %d",
      seed, native_obj, abs(native_obj - expected_obj), native_status
    )
  )
  safe_remove_run_dir(run_dir, seed)
}

manifest <- do.call(rbind, records)
if (!identical(as.integer(manifest$seed), accepted_seeds)) {
  stop("Native validation manifest lost the accepted-seed order.", call. = FALSE)
}
if (write_manifest) {
  utils::write.csv(manifest, manifest_file, row.names = FALSE, quote = TRUE)
  message("Wrote ", manifest_file)
}

if (length(list.files(scratch_root, all.files = TRUE, no.. = TRUE))) {
  stop("Native validation scratch directory is unexpectedly non-empty.", call. = FALSE)
}
unlink(scratch_root, recursive = TRUE, force = FALSE)
if (file.exists(scratch_root)) {
  stop("Could not remove native validation scratch root.", call. = FALSE)
}

message(
  "Verified all 25 accepted recovered PAR files with native MFCL ",
  version_match, "."
)
