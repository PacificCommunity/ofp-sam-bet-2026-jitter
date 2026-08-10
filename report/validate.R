source("scripts/validate-embedded-selectivity.R")

model_dir <- "data/diagnostic"
payload_file <- file.path(model_dir, "model_payload.rds")
regional_file <- file.path(model_dir, "jitter-regional-timeseries.rds")
stock_status_file <- file.path(model_dir, "jitter-stock-status-timeseries.rds")
stock_status_endpoint_file <- file.path(
  model_dir,
  "jitter-stock-status-endpoints.rds"
)
result_files <- list.files(
  file.path(model_dir, "jitter"),
  pattern = "^jitter_result[.]rds$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(
  file.exists(payload_file),
  file.exists(regional_file),
  file.exists(stock_status_file),
  file.exists(stock_status_endpoint_file)
)
if (length(result_files) != 30L) {
  stop("Expected exactly 30 jitter result payloads; found ", length(result_files), ".", call. = FALSE)
}

results <- lapply(result_files, readRDS)
seeds <- vapply(results, function(x) as.integer(x$seed), integer(1))
if (!identical(sort(seeds), 1:30)) {
  stop("Jitter seeds must be exactly 1 through 30.", call. = FALSE)
}

completed <- vapply(results, function(x) isTRUE(x$run_completed), logical(1))
max_grad <- vapply(results, function(x) {
  value <- suppressWarnings(as.numeric(x$max_grad)[1])
  if (length(value) == 0L) NA_real_ else value
}, numeric(1))
included <- completed & is.finite(max_grad) & abs(max_grad) <= 1e-4
included_seeds <- sort(seeds[included])
expected_seeds <- as.integer(c(1:3, 5, 7:22, 25:26, 28:30))
if (!identical(included_seeds, expected_seeds)) {
  stop("The included jitter set does not match the verified 25-fit set.", call. = FALSE)
}
if (
  sum(completed) != 26L ||
    !identical(sort(seeds[!completed]), as.integer(c(4, 23, 24, 27))) ||
    !identical(sort(seeds[completed & !included]), 6L)
) {
  stop("The 30 attempted runs must resolve to 26 completed and the exact 25 retained seeds.", call. = FALSE)
}

sha256_file <- function(path) {
  output <- system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop("sha256sum failed for ", path, ".", call. = FALSE)
  }
  sub("[[:space:]].*$", "", output[[1L]])
}

# The native executable, common case files and fitted starting point are the
# exact files used to produce the retained jitter fits.  Keep these anchors in
# executable validation code as well as in data/SHA256SUMS.
bundle_hashes <- c(
  "data/diagnostic/mfcl/mfclo64" = "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0",
  "data/diagnostic/mfcl/bet.frq" = "d0d84f0a498e6a62681f2a58ffc1ba53dab9e3d6af856b4ad1fd907196250004",
  "data/diagnostic/mfcl/bet.ini" = "fbd064c3d0ccb4d2e1b9beb06fe3eacf0180677821e6a1773d20b308474d984e",
  "data/diagnostic/mfcl/bet.tag" = "b140e66eb52f2b7e022ef2c562134f8bc9baf3dede18ce95283a001acd2b013f",
  "data/diagnostic/mfcl/bet.age_length" = "426859b825bd815aa69c8d97c9dd93097027ed1eb6b9e444d88b69562097a00c",
  "data/diagnostic/mfcl/bet.reg_scaling" = "5f047ddb4053d1f6df9ace18e85e440b11553de246d024ce8138b427f5f9f7e3",
  "data/diagnostic/mfcl/mfcl.cfg" = "2ec8a291fae62c6f37541aec1de37444626d42b3290b371bb42b63d510034eae",
  "data/diagnostic/mfcl/doitall.sh" = "ad8ca660b6d84f9bbd1d8024f616a5bd66047a44ecc2c6e8b5c61c6be089fc5c",
  "data/diagnostic/mfcl/model-inputs/S0.90-F2.conf" = "1a0d0fffec49c033100f3e9c76bfd05a7e3ed4ddfd701221f7199659dfcd9c11",
  "data/diagnostic/mfcl/selectivity-models/F2.csv" = "790e21a01054349a20f4fbbb7db926f6452d059344815a3a9d6a5de51db3310a",
  "data/diagnostic/reproduction/fitted-reference/final.par" = "21dcaea9db8c89ddc8c29fa3c3a5e514b50bef6e26587c168c00c05f35fbebc3",
  "data/diagnostic/reproduction/fitted-reference/indepvar.rpt" = "5792c57d7bcce5e679faa5adac63e6d20886fc478f03a29277814616e313b490",
  "data/diagnostic/reproduction/phase1-reference/mfk_phase1_baseline.par" = "da7f8afd374bbb02906ca40dfc6f77e25eb18615434fe5ac3e2a54976648d6c0"
)
if (!all(file.exists(names(bundle_hashes)))) {
  stop("The exact native MFCL reproduction bundle is incomplete.", call. = FALSE)
}
expected_mfcl_files <- sort(sub(
  "^data/diagnostic/mfcl/", "", names(bundle_hashes)[
    grepl("^data/diagnostic/mfcl/", names(bundle_hashes))
  ]
))
observed_mfcl_files <- sort(list.files(
  "data/diagnostic/mfcl", recursive = TRUE, all.files = FALSE
))
if (!identical(observed_mfcl_files, expected_mfcl_files)) {
  stop("The shared native MFCL directory contains missing or unexpected files.", call. = FALSE)
}
observed_bundle_hashes <- vapply(names(bundle_hashes), sha256_file, character(1L))
if (!identical(unname(observed_bundle_hashes), unname(bundle_hashes))) {
  stop("A native MFCL reproduction file differs from its verified source.", call. = FALSE)
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

assert_fixed_steepness <- function(lines, source) {
  growth <- numeric_row_after(lines, "# Seasonal growth parameters", source)
  age_flags <- numeric_row_after(lines, "# age flags", source)
  if (
    length(growth) < 29L || abs(growth[[29L]] - 0.90) > 1e-12 ||
      length(age_flags) < 162L || age_flags[[162L]] != 0
  ) {
    stop(source, " is not fixed at sv(29)=0.90 with age flag 162=0.", call. = FALSE)
  }
  invisible(TRUE)
}

model_config <- readLines(
  "data/diagnostic/mfcl/model-inputs/S0.90-F2.conf", warn = FALSE
)
if (
  sum(model_config == "MODEL_ID=S0.90-F2") != 1L ||
    sum(model_config == "STEEPNESS=0.90") != 1L ||
    sum(model_config == "SELECTIVITY_MODEL=F2") != 1L ||
    sum(model_config == "SELECTIVITY_INPUT=selectivity-models/F2.csv") != 1L
) {
  stop("The documentation reference must identify S0.90-F2 at steepness 0.90.", call. = FALSE)
}
ini_values <- numeric_row_after(
  readLines("data/diagnostic/mfcl/bet.ini", warn = FALSE),
  "# sv(29)",
  "data/diagnostic/mfcl/bet.ini"
)
if (length(ini_values) != 1L || abs(ini_values[[1L]] - 0.90) > 1e-12) {
  stop("The public bet.ini must itself contain sv(29)=0.90.", call. = FALSE)
}
if (
  file.access("data/diagnostic/mfcl/mfclo64", mode = 1L) != 0L ||
    file.access("data/diagnostic/mfcl/doitall.sh", mode = 1L) != 0L
) {
  stop("The bundled native MFCL executable and doitall.sh must be executable.", call. = FALSE)
}
doitall_lines <- readLines("data/diagnostic/mfcl/doitall.sh", warn = FALSE)
embedded_selectivity <- validate_embedded_selectivity(
  "data/diagnostic/mfcl/doitall.sh",
  "data/diagnostic/mfcl/selectivity-models/F2.csv"
)
if (
  !any(grepl(
    "phase10_11_convergence=${BET_PHASE10_11_CONVERGENCE:--4}",
    doitall_lines,
    fixed = TRUE
  )) ||
    sum(trimws(doitall_lines) == "1 50 $phase10_11_convergence") != 2L ||
    sum(trimws(doitall_lines) == "cp bet.ini bet.model.ini") != 1L ||
    any(grepl(
      "model-inputs/|selectivity-models/|SELECTIVITY_INPUT|SELECTIVITY_REFERENCE",
      doitall_lines
    ))
) {
  stop(
    "doitall.sh must be self-contained for model/selectivity controls, use the public INI unchanged, and apply 1e-4 to both final phases.",
    call. = FALSE
  )
}

plans_file <- "data/diagnostic/reproduction/jitter-plans.rds"
plans <- readRDS(plans_file)
if (
  !identical(plans$schema, "ofp-sam-bet-2026-jitter-plans.v1") ||
    !identical(plans$design, "structure_aware_single_cv_v1") ||
    !identical(as.numeric(plans$cv), 0.1) ||
    !identical(as.integer(plans$seeds), expected_seeds) ||
    !identical(plans$mfclkit_commit, "c8d80c7d915441dff16dca101be6f452d0fb3482") ||
    !identical(names(plans$plans), as.character(expected_seeds))
) {
  stop("The exact 25-seed jitter plan has unexpected provenance or settings.", call. = FALSE)
}
required_plan_columns <- c(
  "Index", "Var_name", "family", "before", "after", "target",
  "jitter_method", "jitter_space", "nominal_cv", "seed"
)
for (seed in expected_seeds) {
  plan <- plans$plans[[as.character(seed)]]
  if (
    !is.data.frame(plan) || nrow(plan) != 1997L ||
      !all(required_plan_columns %in% names(plan)) ||
      !identical(sort(as.integer(plan$Index)), seq_len(1997L)) ||
      !all(as.integer(plan$seed) == seed) ||
      !all(is.finite(plan$before)) || !all(is.finite(plan$after)) ||
      !all(plan$nominal_cv == 0.1)
  ) {
    stop("The exact jitter plan is invalid for seed ", seed, ".", call. = FALSE)
  }
}

expected_phase1_files <- sort(c(
  "fitted_active_xinit.rpt", "indepvar.rpt", "jitter_deferred_mapping.rds",
  "jitter_deferred_xinit.rpt", "jitter_fitted_active_mapping.rds",
  "jitter_mask_coverage.csv", "jitter_phase1_mapping.rds",
  "mfk_phase1_baseline.par", "phase1_reference.rds", "phase1_xinit.rpt",
  "xinit.rpt"
))
observed_phase1_files <- sort(list.files(
  "data/diagnostic/reproduction/phase1-reference",
  recursive = FALSE,
  all.files = FALSE
))
if (!identical(observed_phase1_files, expected_phase1_files)) {
  stop("The public Phase-1 reference set contains missing or unexpected files.", call. = FALSE)
}
if (!identical(
  sort(list.files(
    "data/diagnostic/reproduction/fitted-reference",
    recursive = FALSE,
    all.files = FALSE
  )),
  c("final.par", "indepvar.rpt")
)) {
  stop("The fitted reproduction reference must contain only final.par and indepvar.rpt.", call. = FALSE)
}

par_files <- list.files(
  file.path(model_dir, "jitter"),
  pattern = "^jittered_out_[0-9]+[.]par$",
  recursive = TRUE,
  full.names = TRUE
)
par_seeds <- suppressWarnings(as.integer(sub(
  "^jittered_out_([0-9]+)[.]par$", "\\1", basename(par_files)
)))
if (
  length(par_files) != 25L || anyNA(par_seeds) || anyDuplicated(par_seeds) ||
    !identical(sort(par_seeds), expected_seeds)
) {
  stop("Recovered last PAR files must be exactly the 25 retained seeds.", call. = FALSE)
}
semantic_par_files <- c(
  par_files,
  "data/diagnostic/reproduction/fitted-reference/final.par",
  "data/diagnostic/reproduction/phase1-reference/mfk_phase1_baseline.par"
)
for (par_file in semantic_par_files) {
  assert_fixed_steepness(readLines(par_file, warn = FALSE), par_file)
  assert_par_selectivity(par_file, embedded_selectivity)
}

native_manifest_file <- file.path(model_dir, "native-par-validation.csv")
native_manifest <- utils::read.csv(native_manifest_file, check.names = FALSE)
expected_par_paths <- file.path(
  "data", "diagnostic", "jitter", paste0("jitter_seed_", expected_seeds),
  paste0("jittered_out_", expected_seeds, ".par")
)
required_native_columns <- c(
  "seed", "par_file", "par_sha256", "expected_obj_fun", "native_obj_fun",
  "objective_abs_diff", "max_grad", "native_status",
  "evaluated_par_created", "plot_rep_created", "mfcl_version", "mfcl_sha256"
)
if (
  !all(required_native_columns %in% names(native_manifest)) ||
    !identical(as.integer(native_manifest$seed), expected_seeds) ||
    !identical(as.character(native_manifest$par_file), expected_par_paths) ||
    !all(native_manifest$native_status %in% c(0L, 3L)) ||
    !all(native_manifest$evaluated_par_created %in% TRUE) ||
    !all(native_manifest$plot_rep_created %in% TRUE) ||
    !all(native_manifest$mfcl_version == "2.2.7.9") ||
    !all(native_manifest$mfcl_sha256 == bundle_hashes[["data/diagnostic/mfcl/mfclo64"]]) ||
    any(native_manifest$objective_abs_diff > 1e-6)
) {
  stop("The native MFCL validation manifest is incomplete or invalid.", call. = FALSE)
}
manifest_results <- match(native_manifest$seed, seeds)
if (
  !isTRUE(all.equal(
    native_manifest$expected_obj_fun,
    vapply(results[manifest_results], function(x) as.numeric(x$obj_fun)[[1L]], numeric(1L)),
    tolerance = 1e-10,
    check.attributes = FALSE
  )) ||
    !isTRUE(all.equal(
      native_manifest$max_grad,
      vapply(results[manifest_results], function(x) as.numeric(x$max_grad)[[1L]], numeric(1L)),
      tolerance = 1e-12,
      check.attributes = FALSE
    )) ||
    !identical(
      as.character(native_manifest$par_sha256),
      unname(vapply(expected_par_paths, sha256_file, character(1L)))
    )
) {
  stop("Recovered PAR files do not match their compact results and native audit.", call. = FALSE)
}

regional <- readRDS(regional_file)
required_columns <- c(
  "year", "region", "quantity", "value", "seed", "is_reference", "display_label"
)
if (!all(required_columns %in% names(regional))) {
  stop("The regional payload is missing required columns.", call. = FALSE)
}
regional_seeds <- sort(unique(as.integer(regional$seed[!is.na(regional$seed)])))
if (!identical(regional_seeds, expected_seeds)) {
  stop("The regional payload does not contain exactly the verified 25 fits.", call. = FALSE)
}
required_quantities <- c(
  "Regional depletion", "Regional recruitment", "Regional recruitment deviation"
)
if (!all(required_quantities %in% unique(regional$quantity))) {
  stop("The regional payload is missing required quantities.", call. = FALSE)
}

stock_status <- readRDS(stock_status_file)
required_stock_status_columns <- c(
  "scenario", "model_label", "run", "seed", "display_label",
  "is_reference", "is_base_fit_reference", "metric", "quantity",
  "year", "value", "series_type"
)
if (!all(required_stock_status_columns %in% names(stock_status))) {
  stop("The stock-status payload is missing required columns.", call. = FALSE)
}
stock_status_seeds <- sort(unique(as.integer(stock_status$seed[!is.na(stock_status$seed)])))
if (!identical(stock_status_seeds, expected_seeds)) {
  stop("The stock-status payload does not contain exactly the verified 25 fits.", call. = FALSE)
}
required_stock_status_metrics <- c(
  "annual_depletion", "annual_sb_sbmsy", "annual_f_fmsy"
)
if (!all(required_stock_status_metrics %in% unique(stock_status$metric))) {
  stop("The stock-status payload is missing required metrics.", call. = FALSE)
}

stock_status_endpoints <- readRDS(stock_status_endpoint_file)
required_endpoint_columns <- c(
  "scenario", "model_label", "run", "seed", "display_label",
  "is_reference", "is_base_fit_reference", "metric", "quantity",
  "unit", "window", "source", "value"
)
if (!all(required_endpoint_columns %in% names(stock_status_endpoints))) {
  stop("The recent stock-status endpoint payload is missing required columns.", call. = FALSE)
}
if (nrow(stock_status_endpoints) != 78L) {
  stop("Expected 78 recent stock-status endpoint rows.", call. = FALSE)
}
endpoint_seeds <- sort(unique(as.integer(
  stock_status_endpoints$seed[!is.na(stock_status_endpoints$seed)]
)))
if (!identical(endpoint_seeds, expected_seeds)) {
  stop("The recent stock-status endpoints do not contain exactly the verified 25 fits.", call. = FALSE)
}
expected_endpoint_windows <- c(
  recent_depletion = "2021-2024 / 2014-2023",
  sb_recent_sbmsy = "2021-2024",
  f_recent_fmsy = "2020-2023"
)
if (!identical(
  sort(unique(stock_status_endpoints$metric)),
  sort(names(expected_endpoint_windows))
)) {
  stop("The recent stock-status endpoints contain unexpected metrics.", call. = FALSE)
}
for (metric in names(expected_endpoint_windows)) {
  windows <- unique(stock_status_endpoints$window[
    stock_status_endpoints$metric == metric
  ])
  if (!identical(windows, unname(expected_endpoint_windows[[metric]]))) {
    stop("Unexpected reporting window for ", metric, ".", call. = FALSE)
  }
}
if (any(!is.finite(stock_status_endpoints$value))) {
  stop("The recent stock-status endpoint payload contains non-finite values.", call. = FALSE)
}
reference_endpoints <- stock_status_endpoints[
  stock_status_endpoints$is_reference %in% TRUE,
  ,
  drop = FALSE
]
if (nrow(reference_endpoints) != 3L) {
  stop("Expected one unjittered Diagnostic model value for each endpoint.", call. = FALSE)
}

# Reconstruct the unjittered endpoints from the embedded native MFCL objects.
# This deliberately audits the assessment calculation rather than averaging
# the annual ratios drawn in the figure.
if (!requireNamespace("FLR4MFCL", quietly = TRUE)) {
  stop("FLR4MFCL is required to audit the native MFCL endpoints.", call. = FALSE)
}
model_payload <- readRDS(payload_file)
embedded_par <- model_payload$artifacts$files$par
if (!is.list(embedded_par) || !is.raw(embedded_par$bytes)) {
  stop("The compact model payload is missing its embedded final PAR.", call. = FALSE)
}
embedded_par_bytes <- if (identical(embedded_par$compression, "none")) {
  embedded_par$bytes
} else {
  memDecompress(embedded_par$bytes, type = embedded_par$compression)
}
assert_fixed_steepness(
  strsplit(rawToChar(embedded_par_bytes), "\n", fixed = TRUE)[[1L]],
  "data/diagnostic/model_payload.rds embedded final.par"
)
unpack_object <- function(role) {
  object <- model_payload$object_cache$objects[[role]]
  if (!is.list(object) || !is.raw(object$bytes)) {
    stop("The compact model payload is missing its ", role, " object.", call. = FALSE)
  }
  compression <- as.character(object$compression[[1L]])
  bytes <- if (identical(compression, "none")) {
    object$bytes
  } else {
    memDecompress(object$bytes, type = compression)
  }
  unserialize(bytes)
}
par_object <- unpack_object("ParOut")
rep_object <- unpack_object("RepOut")
flags <- methods::slot(par_object, "flags")
flag_value <- function(flagtype, flag, fallback) {
  value <- suppressWarnings(as.numeric(flags$value[
    flags$flagtype == flagtype & flags$flag == flag
  ]))
  value <- value[is.finite(value)]
  if (length(value)) value[[1L]] else fallback
}
adult_biomass <- as.array(methods::slot(rep_object, "adultBiomass"))
adult_biomass_nofish <- as.array(
  methods::slot(rep_object, "adultBiomass_nofish")
)
periods_per_year <- dim(adult_biomass)[[4L]]
sb_recent_years <- flag_value(1, 59, 0)
sbf0_recent_years <- flag_value(1, 60, 0)
if (sb_recent_years <= 0) sb_recent_years <- 4
if (sbf0_recent_years <= 0) sbf0_recent_years <- 10
f_recent_periods <- flag_value(2, 148, 5 * periods_per_year)
f_omit_periods <- flag_value(2, 155, periods_per_year)
if (
  periods_per_year != 4L ||
    sb_recent_years != 4L ||
    sbf0_recent_years != 10L ||
    f_recent_periods != 20L ||
    f_omit_periods != 4L
) {
  stop("The native MFCL recent-period controls do not match the audited BET settings.", call. = FALSE)
}
years <- suppressWarnings(as.integer(dimnames(adult_biomass)$year))
terminal_year <- max(years, na.rm = TRUE)
sb_years <- seq.int(terminal_year - sb_recent_years + 1L, terminal_year)
sbf0_years <- seq.int(terminal_year - sbf0_recent_years, terminal_year - 1L)
f_year_count <- (f_recent_periods - f_omit_periods) / periods_per_year
f_lag_years <- f_omit_periods / periods_per_year
f_years <- seq.int(
  terminal_year - f_lag_years - f_year_count + 1L,
  terminal_year - f_lag_years
)
audited_windows <- c(
  recent_depletion = paste0(
    min(sb_years), "-", max(sb_years), " / ",
    min(sbf0_years), "-", max(sbf0_years)
  ),
  sb_recent_sbmsy = paste0(min(sb_years), "-", max(sb_years)),
  f_recent_fmsy = paste0(min(f_years), "-", max(f_years))
)
if (!identical(audited_windows, expected_endpoint_windows)) {
  stop("The native MFCL controls produced unexpected reporting windows.", call. = FALSE)
}
annual_total <- function(values) {
  year_period <- apply(values, c(2L, 4L), sum)
  stats::setNames(rowMeans(year_period), years)
}
sb <- annual_total(adult_biomass)
sbf0 <- annual_total(adult_biomass_nofish)
sb_recent <- mean(sb[as.character(sb_years)])
sbf0_recent <- mean(sbf0[as.character(sbf0_years)])
bmsy <- suppressWarnings(as.numeric(methods::slot(rep_object, "BMSY"))[[1L]])
fmult <- suppressWarnings(as.numeric(methods::slot(rep_object, "Fmult"))[[1L]])
audited_values <- c(
  recent_depletion = sb_recent / sbf0_recent,
  sb_recent_sbmsy = sb_recent / bmsy,
  f_recent_fmsy = 1 / fmult
)
stored_values <- stats::setNames(
  reference_endpoints$value,
  reference_endpoints$metric
)[names(audited_values)]
if (!isTRUE(all.equal(
  unname(stored_values),
  unname(audited_values),
  tolerance = 5e-7,
  check.attributes = FALSE
))) {
  stop("The stored Diagnostic model endpoints do not match the native MFCL calculation.", call. = FALSE)
}

collect_text <- function(x) {
  values <- character()
  visit <- function(y) {
    if (is.character(y)) values <<- c(values, y)
    if (is.factor(y)) values <<- c(values, levels(y))
    if (is.list(y)) {
      values <<- c(values, names(y))
      for (item in y) visit(item)
    } else if (isS4(y)) {
      for (name in methods::slotNames(y)) visit(methods::slot(y, name))
    }
  }
  visit(x)
  values
}

public_patterns <- c(
  "internal absolute path" = "/(home|var/lib/condor|kflow)/",
  "credential-like text" = "(ghp_|github_pat_|token[=:]|password[=:]|secret[=:])",
  "internal job number" = "Job[ #]+[0-9]{4,}",
  "internal host" = "corp[.]spc[.]int"
)
public_files <- c(
  payload_file,
  regional_file,
  stock_status_file,
  stock_status_endpoint_file,
  result_files,
  list.files(
    "data/diagnostic/reproduction",
    pattern = "[.]rds$",
    recursive = TRUE,
    full.names = TRUE
  )
)
for (file in public_files) {
  text <- collect_text(readRDS(file))
  for (label in names(public_patterns)) {
    if (any(grepl(public_patterns[[label]], text, ignore.case = TRUE, perl = TRUE), na.rm = TRUE)) {
      stop("Public payload contains ", label, ": ", file, call. = FALSE)
    }
  }
}

message(
  "Validated 30 attempted jitter payloads: 26 completed and the exact 25-seed ",
  "MGC <= 1.0e-4 set was retained. Recovered PAR, native MFCL, BET endpoint ",
  "and public-data hygiene checks passed."
)
