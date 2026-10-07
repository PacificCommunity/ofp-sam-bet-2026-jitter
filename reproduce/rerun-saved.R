# Evaluate a retained fit and compare annual biomass/depletion and the three
# report endpoints. Whole REP, Hessian and projection identity remain unverified.
options(stringsAsFactors = FALSE)

has_symlink <- function(path) {
  link <- Sys.readlink(path)
  !is.na(link) && nzchar(link)
}

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

require_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

# Read the native text, including the complete row/column shape of each section.
central_rep <- function(path) {
  lines <- readLines(path, warn = FALSE)
  headers <- which(grepl("^[[:space:]]*#", lines))
  labels <- trimws(sub("^[[:space:]]*#[[:space:]]*", "", lines[headers]))
  section <- function(label, rows = 1L, columns = 1L) {
    i <- which(labels == label)
    require_true(length(i) == 1L, paste("REP needs exactly one", label))
    first <- headers[i] + 1L
    last <- if (i < length(headers)) headers[i + 1L] - 1L else length(lines)
    require_true(first <= last, paste("Empty REP section:", label))
    text <- trimws(lines[seq.int(first, last)])
    tokens <- strsplit(text[nzchar(text)], "[[:space:]]+")
    values <- suppressWarnings(as.numeric(unlist(tokens, use.names = FALSE)))
    require_true(length(tokens) == rows && all(lengths(tokens) == columns) &&
                   length(values) == rows * columns && all(is.finite(values)),
                 paste("Invalid REP shape or values:", label))
    matrix(values, nrow = rows, ncol = columns, byrow = TRUE)
  }
  dimension_labels <- c("Number of time periods", "Year 1", "Number of regions",
                        "Number of species", "Number of age classes",
                        "Number of recruitments per year")
  dimensions <- vapply(dimension_labels, function(x) as.numeric(section(x)), numeric(1L))
  require_true(identical(unname(dimensions), c(292, 1952, 5, 1, 40, 4)),
               "Native REP dimensions differ from the retained BET model.")
  sb <- section("Adult biomass", 292L, 5L)
  sb0 <- section("Adult biomass in absence of fishing", 292L, 5L)
  bmsy <- as.numeric(section("Adult biomass at MSY"))
  fmult <- as.numeric(section("F multiplier at MSY"))
  require_true(all(c(sb, sb0, bmsy, fmult) > 0), "Non-positive central native REP values.")
  annual <- function(x) colMeans(matrix(rowSums(x), nrow = 4L))
  sb <- annual(sb); sb0 <- annual(sb0)
  list(dimensions = dimensions, bmsy = bmsy, fmult = fmult,
       annual = data.frame(year = 1952:2024, adult_biomass = sb,
         adult_biomass_nofish = sb0, spawning_potential = sb / 1000,
         depletion = sb / sb0))
}

report_windows <- function(path) {
  lines <- readLines(path, warn = FALSE)
  # The second parest_flags block is historical, not the current controls.
  historical <- which(trimws(lines) == "# Historical_flags")
  require_true(length(historical) == 1L, "Missing current/historical PAR boundary.")
  current <- lines[seq_len(historical - 1L)]
  flags <- numeric_row_after(current, "# The parest_flags", path)
  age <- numeric_row_after(current, "# age flags", path)
  require_true(length(flags) >= 60L && length(age) >= 155L, "Incomplete recent-period flags.")
  sb_years <- if (flags[59L] <= 0) 4 else flags[59L]
  sb0_years <- if (flags[60L] <= 0) 10 else flags[60L]
  require_true(identical(c(sb_years, sb0_years, age[148L], age[155L]), c(4, 10, 20, 4)),
               "PAR recent-period controls differ from report/validate.R.")
  c(recent_depletion = "2021-2024 / 2014-2023",
    sb_recent_sbmsy = "2021-2024", f_recent_fmsy = "2020-2023")
}

reference_values <- function(result, derived, stock, endpoints, seed) {
  years <- 1952:2024
  q <- result$derived_quantities
  require_true(is.data.frame(q) && all(c("seed", "year", "adult_biomass",
      "spawning_potential", "depletion") %in% names(q)) && nrow(q) == 73L &&
      identical(as.numeric(q$year), as.numeric(years)) && all(q$seed == seed),
      "Saved annual model/years differ.")
  for (field in c("adult_biomass", "spawning_potential", "depletion")) {
    require_true(is.numeric(q[[field]]) && all(is.finite(q[[field]]) & q[[field]] > 0),
                 paste("Invalid saved annual", field))
  }
  require_true(all(abs(q$adult_biomass / 1000 - q$spawning_potential) <=
                   1e-10 * pmax(1, abs(q$spawning_potential))), "Saved SB units differ.")
  annual_reference <- function(data, column, label) {
    require_true(is.data.frame(data) && all(c("seed", "year", column, "value",
      "is_reference", "is_base_fit_reference") %in% names(data)), "Incomplete annual reference.")
    x <- data[data$seed %in% seed & data[[column]] %in% label, , drop = FALSE]
    require_true(nrow(x) == 73L && identical(as.numeric(x$year), as.numeric(years)) &&
      all(!x$is_reference & !x$is_base_fit_reference) && all(is.finite(x$value) & x$value > 0),
      paste("Invalid annual reference:", label))
    x$value
  }
  compare <- function(x, y, label) {
    require_true(all(abs(x-y) <= 1e-10 * pmax(1, abs(y))), paste("Saved references differ:", label))
  }
  compare(q$spawning_potential, annual_reference(derived, "quantity", "Spawning potential"), "SB")
  compare(q$depletion, annual_reference(derived, "quantity", "Depletion"), "depletion")
  compare(q$depletion, annual_reference(stock, "metric", "annual_depletion"), "stock-status depletion")
  metrics <- c("recent_depletion", "sb_recent_sbmsy", "f_recent_fmsy")
  require_true(is.data.frame(endpoints) && all(c("seed", "metric", "unit", "window", "value",
      "is_reference", "is_base_fit_reference") %in% names(endpoints)), "Incomplete endpoint reference.")
  e <- endpoints[endpoints$seed %in% seed, , drop = FALSE]
  require_true(nrow(e) == 3L && !anyDuplicated(e$metric) && setequal(e$metric, metrics) &&
      all(!e$is_reference & !e$is_base_fit_reference) && all(e$unit == "ratio") &&
      all(is.finite(e$value) & e$value > 0), "Invalid saved endpoints.")
  e <- e[match(metrics, e$metric), , drop = FALSE]
  require_true(identical(as.character(e$window), c("2021-2024 / 2014-2023", "2021-2024", "2020-2023")),
      "Saved endpoint windows differ.")
  sb0 <- q$adult_biomass / q$depletion
  expected_depletion <- mean(q$adult_biomass[years %in% 2021:2024]) / mean(sb0[years %in% 2014:2023])
  compare(expected_depletion, e$value[e$metric == "recent_depletion"], "recent depletion")
  list(annual = data.frame(year = years, adult_biomass = q$adult_biomass,
      adult_biomass_nofish = sb0, spawning_potential = q$spawning_potential, depletion = q$depletion),
       endpoints = e)
}

compare_central <- function(report, reference, seed) {
  expected <- reference$annual; actual <- report$annual
  require_true(identical(actual$year, expected$year), "Native annual years differ.")
  annual_diff <- 0
  for (field in c("adult_biomass", "adult_biomass_nofish", "spawning_potential", "depletion")) {
    delta <- abs(actual[[field]] - expected[[field]])
    require_true(all(delta <= 1e-10 * pmax(1, abs(expected[[field]]))),
                 paste("Native annual values differ:", field))
    annual_diff <- max(annual_diff, delta)
  }
  # Match report/validate.R: ratio of means, and scalar native BMSY/Fmult.
  sb_recent <- mean(actual$adult_biomass[actual$year %in% 2021:2024])
  sb0_recent <- mean(actual$adult_biomass_nofish[actual$year %in% 2014:2023])
  values <- c(recent_depletion = sb_recent / sb0_recent,
              sb_recent_sbmsy = sb_recent / report$bmsy, f_recent_fmsy = 1 / report$fmult)
  e <- reference$endpoints
  delta <- abs(unname(values) - e$value)
  require_true(all(delta <= 5e-7 * pmax(1, abs(e$value))), "Native management endpoints differ.")
  e$native_value <- unname(values); e$abs_diff <- delta
  actual$seed <- seed
  list(annual = actual, endpoints = e, annual_max_abs_diff = annual_diff,
       endpoint_max_abs_diff = max(delta))
}

native_log <- function(path, parameters = 1997) {
  lines <- readLines(path, warn = FALSE)
  controls <- grep("^[[:space:]]*optfile\\.cpp[[:space:]]+", lines, value = TRUE)
  ceilings <- 0L; convergence <- 0L
  for (line in controls) {
    fields <- strsplit(trimws(sub("^[[:space:]]*optfile\\.cpp[[:space:]]+", "", line)), "[[:space:]]+")[[1L]]
    require_true(length(fields) >= 3L && all(grepl("^[-+]?[0-9]+$", fields[1:3])), "Malformed native control.")
    control <- as.numeric(fields[1:3])
    if (identical(control[1:2], c(1, 1))) {
      require_true(control[3L] == 1, "Native function-evaluation ceiling differs."); ceilings <- ceilings + 1L
    }
    if (identical(control[1:2], c(1, 50))) {
      require_true(control[3L] == 0, "Native convergence control differs."); convergence <- convergence + 1L
    }
  }
  counters <- lines[grepl("variables;", lines, fixed = TRUE) & grepl("function[[:space:]]+evaluation", lines)]
  pattern <- "^[[:space:]]*([0-9]+)[[:space:]]+variables;[[:space:]]+iteration[[:space:]]+([0-9]+);[[:space:]]+function[[:space:]]+evaluation[[:space:]]+([0-9]+)[[:space:]]*$"
  for (line in counters) {
    line <- sub("^[[:space:]]*Initial statistics:[[:space:]]*", "", line)
    fields <- regmatches(line, regexec(pattern, line))[[1L]]
    require_true(length(fields) == 4L && identical(as.numeric(fields[2:4]), c(parameters, 0, 0)),
                 "Native parameter/iteration/function counter differs.")
  }
  require_true(ceilings == 1L && convergence == 1L && length(counters) > 0L,
               "Native controls or zero-counter evidence absent.")
  totals <- grep("^[[:space:]]*Total func[[:space:]]+[^[:space:]]+[[:space:]]*$", lines, value = TRUE)
  require_true(length(totals) > 0L, "Native objective absent.")
  objective <- suppressWarnings(as.numeric(sub("^[[:space:]]*Total func[[:space:]]+", "", totals[1L])))
  require_true(length(objective) == 1L && is.finite(objective), "Non-finite native objective.")
  list(objective = objective, counters = counters)
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
      file.exists(output) || has_symlink(output)) {
    stop("OUT must be new and outside the repository.", call. = FALSE)
  }

  mfcl_dir <- file.path(repo, "data", "diagnostic", "mfcl")
  common <- c("bet.frq", "bet.ini", "bet.tag", "bet.age_length",
              "bet.reg_scaling", "mfcl.cfg", "mfclo64", "doitall.sh")
  par <- file.path(repo, "data", "diagnostic", "jitter",
                   paste0("jitter_seed_", seed), paste0("jittered_out_", seed, ".par"))
  result_file <- file.path(dirname(par), "jitter_result.rds")
  reference <- file.path(mfcl_dir, "selectivity-models", "F2.csv")
  annual_files <- file.path(repo, "data", "diagnostic",
    c("jitter-derived-timeseries.rds", "jitter-stock-status-timeseries.rds", "jitter-stock-status-endpoints.rds"))
  sources <- c(file.path(mfcl_dir, common), par, result_file, reference, annual_files)
  if (!all(file.exists(sources)) || any(vapply(sources, function(p) {
    isTRUE(file.info(p)$isdir) || has_symlink(p)
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
  report_windows(par)
  saved <- reference_values(result, readRDS(annual_files[1L]),
    readRDS(annual_files[2L]), readRDS(annual_files[3L]), seed)

  if (!dir.create(output, mode = "0700")) stop("Could not reserve the new OUT directory.")
  if (!all(file.copy(file.path(mfcl_dir, common), output, copy.mode = TRUE)) ||
      !file.copy(par, file.path(output, "input.par"), copy.mode = TRUE)) {
    stop("Could not stage the original input files.", call. = FALSE)
  }
  staged <- c(file.path(output, common), file.path(output, "input.par"))
  staged_sha <- unname(vapply(staged, sha256_file, character(1L)))
  if (!identical(staged_sha, before[seq_along(staged)])) stop("Staged input hashes differ.")
  controls <- c("1 1 1", "1 50 0", "1 246 1")
  writeLines(controls, file.path(output, "controls.txt"), useBytes = TRUE)
  setwd(output)
  status <- as.integer(system2(file.path(output, "mfclo64"),
      c("bet.frq", "input.par", "evaluated.par", "-file", "-"),
      stdout = file.path(output, "mfcl.log"), stderr = file.path(output, "mfcl.log"),
      input = controls, timeout = 180L))
  setwd(repo)
  evaluated <- file.path(output, "evaluated.par")
  rep <- file.path(output, "plot-evaluated.par.rep")
  if (!(status %in% c(0L, 3L)) || !file.exists(evaluated) || file.info(evaluated)$size <= 0 ||
      !file.exists(rep) || file.info(rep)$size <= 0 ||
      !identical(unname(vapply(staged, sha256_file, character(1L))), staged_sha) ||
      !identical(unname(vapply(sources, sha256_file, character(1L))), before)) {
    stop("Native outputs failed, or original/staged inputs changed.", call. = FALSE)
  }
  logged <- native_log(file.path(output, "mfcl.log"))
  observed <- logged$objective
  evaluated_obj <- par_scalar(evaluated, "# Objective function value")
  if (length(observed) != 1L || !is.finite(observed) ||
      abs(observed - expected_obj) > 1e-6 || abs(evaluated_obj - expected_obj) > 1e-6 ||
      par_scalar(evaluated, "# The number of parameters") != 1997) {
    stop("Native objective or parameter-count parity failed.", call. = FALSE)
  }
  assert_fixed_steepness(evaluated)
  assert_par_selectivity(evaluated, selectivity)
  report_windows(evaluated)
  compared <- compare_central(central_rep(rep), saved, seed)
  utils::write.csv(compared$annual, file.path(output, "central-results.csv"), row.names = FALSE)
  utils::write.csv(compared$endpoints, file.path(output, "management-quantities.csv"), row.names = FALSE)
  writeLines(logged$counters, file.path(output, "native-counters.txt"), useBytes = TRUE)
  utils::write.csv(data.frame(path = basename(staged), sha256 = staged_sha),
                   file.path(output, "input-checksums.csv"), row.names = FALSE)
  utils::write.csv(data.frame(seed = seed, native_status = status,
      input_par_sha256 = sha256_file(par), expected_objective = expected_obj,
      native_objective = observed, evaluated_objective = evaluated_obj,
      objective_abs_diff = abs(observed - expected_obj),
      annual_max_abs_diff = compared$annual_max_abs_diff,
      endpoint_max_abs_diff = compared$endpoint_max_abs_diff,
      annual_rows = nrow(compared$annual), endpoint_rows = nrow(compared$endpoints),
      counter_records = length(logged$counters), iterations = 0L, function_evaluations = 0L,
      source_and_staged_inputs_unchanged = TRUE,
      reference_manifest_sha256 = sha256_file(manifest),
      saved_result_sha256 = sha256_file(result_file),
      mfcl_sha256 = expected_engine,
      evaluated_par_sha256 = sha256_file(evaluated), plot_rep_sha256 = sha256_file(rep),
      scope = "Objective, zero counters, annual SB/SBF0/depletion and report endpoints; whole REP/MSY yield/Hessian/projections unverified"),
      file.path(output, "native-check.csv"), row.names = FALSE)
  message("Saved seed ", seed, " evaluated; outputs retained in ", output)
}

if (sys.nframe() == 0L) main()
