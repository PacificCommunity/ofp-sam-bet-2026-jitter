model_dir <- "data/S0.90-F2-tau2-fixed"
payload_file <- file.path(model_dir, "model_payload.rds")
regional_file <- file.path(model_dir, "jitter-regional-timeseries.rds")
stock_status_file <- file.path(model_dir, "jitter-stock-status-timeseries.rds")
result_files <- list.files(
  file.path(model_dir, "jitter"),
  pattern = "^jitter_result[.]rds$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(file.exists(payload_file), file.exists(regional_file), file.exists(stock_status_file))
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
public_files <- c(payload_file, regional_file, stock_status_file, result_files)
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
  "five excluded. Public-data hygiene checks passed."
)
