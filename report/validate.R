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
included <- completed & is.finite(max_grad) & max_grad <= 1e-4
included_seeds <- sort(seeds[included])
expected_seeds <- as.integer(c(1:3, 5, 7:22, 25:26, 28:30))
if (!identical(included_seeds, expected_seeds)) {
  stop("The included jitter set does not match the verified 25-fit set.", call. = FALSE)
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
  result_files
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
  "Validated 30 jitter payloads: 25 included at MGC <= 1.0e-4; ",
  "five excluded. Native BET endpoint and public-data hygiene checks passed."
)
