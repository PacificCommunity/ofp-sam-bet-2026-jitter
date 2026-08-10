options(stringsAsFactors = FALSE, warn = 1)

accepted_seeds <- as.integer(c(1:3, 5, 7:22, 25:26, 28:30))
jitter_cv <- 0.1
phase10_convergence <- -4L
phase11_convergence <- -4L

expected_image <- paste0(
  "ghcr.io/pacificcommunity/tuna-flow-private:v2.7@sha256:",
  "4fee4c40cb6439ff920b1dd233a84bf19d5cc0e37278c99ceff3fd79cb9c8852"
)
expected_mfclkit_sha <- "44abaaa05692db7ae3e0ec0e52250c51714d1e50"
expected_flr4mfcl_sha <- "5a29a9b3246bd19dcff350ded7e0e5099145da5e"
expected_mfclkit_version <- "0.0.0.9040"
expected_flr4mfcl_version <- "1.8.0"
reference_mfclkit_sha <- "c8d80c7d915441dff16dca101be6f452d0fb3482"
expected_mfcl_sha <- "f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0"
expected_phase0_semantic_sha <- "a4274d7440cf563890197fb59b67898484369f90b9bc1764ab8bcb55e3d4fd0a"
expected_phase1_sha <- "da7f8afd374bbb02906ca40dfc6f77e25eb18615434fe5ac3e2a54976648d6c0"

usage <- function(status = 0L) {
  text <- paste(
    "Usage: Rscript scripts/reproduce-jitter.R",
    "--repo /read-only/repo --output /fresh/output [--run]"
  )
  cat(text, "\n", file = if (status == 0L) stdout() else stderr())
  quit(save = "no", status = status)
}

args <- commandArgs(trailingOnly = TRUE)
repo_arg <- output_arg <- NULL
run_native <- FALSE
i <- 1L
while (i <= length(args)) {
  arg <- args[[i]]
  if (identical(arg, "--run")) {
    run_native <- TRUE
  } else if (identical(arg, "--repo") || identical(arg, "--output")) {
    if (i == length(args)) usage(2L)
    i <- i + 1L
    if (identical(arg, "--repo")) repo_arg <- args[[i]] else output_arg <- args[[i]]
  } else if (arg %in% c("-h", "--help")) {
    usage(0L)
  } else {
    stop("Unknown argument: ", arg, call. = FALSE)
  }
  i <- i + 1L
}
if (is.null(repo_arg) || is.null(output_arg)) usage(2L)

repo <- normalizePath(repo_arg, winslash = "/", mustWork = TRUE)
output <- normalizePath(output_arg, winslash = "/", mustWork = TRUE)
source(file.path(repo, "scripts", "validate-embedded-selectivity.R"))
if (identical(repo, output) || startsWith(paste0(output, "/"), paste0(repo, "/"))) {
  stop("The output directory must be outside the repository.", call. = FALSE)
}
if (nzchar(Sys.readlink(output)) || !isTRUE(file.info(output)$isdir)) {
  stop("The output path must be a real directory.", call. = FALSE)
}
if (length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  stop("The output directory must be fresh and empty.", call. = FALSE)
}

assert_mount_mode <- function(path, mode) {
  mountinfo <- readLines("/proc/self/mountinfo", warn = FALSE)
  fields <- strsplit(sub(" - .*$", "", mountinfo), "[[:space:]]+")
  matches <- vapply(fields, function(row) {
    length(row) >= 6L && identical(row[[5L]], path)
  }, logical(1L))
  if (sum(matches) != 1L) {
    stop("Could not identify the dedicated container mount for ", path, ".",
         call. = FALSE)
  }
  options <- strsplit(fields[[which(matches)]][[6L]], ",", fixed = TRUE)[[1L]]
  if (!mode %in% options) {
    stop("Container mount ", path, " must be ", mode, ".", call. = FALSE)
  }
  invisible(TRUE)
}

assert_mount_mode(repo, "ro")
assert_mount_mode(output, "rw")

sha256_file <- function(path) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir)) {
    stop("Missing required file: ", path, call. = FALSE)
  }
  unname(tools::sha256sum(path))
}

assert_sha <- function(path, expected) {
  actual <- sha256_file(path)
  if (!identical(actual, unname(expected))) {
    stop(
      "SHA-256 mismatch for ", path, ": expected ", expected,
      ", observed ", actual, call. = FALSE
    )
  }
  invisible(actual)
}

phase0_semantic_sha <- function(path) {
  lines <- readLines(path, warn = FALSE)
  begin <- which(trimws(lines) == "# movement matrices")
  finish <- which(trimws(lines) == "# age dependent movement coefficients")
  if (length(begin) != 1L || length(finish) != 1L || finish <= begin) {
    stop("Phase 0 has an invalid movement-matrix section: ", path, call. = FALSE)
  }
  rows <- seq.int(begin + 1L, finish - 1L)
  for (row in rows) {
    if (startsWith(trimws(lines[[row]]), "#")) next
    tokens <- strsplit(trimws(lines[[row]]), "[[:space:]]+")[[1L]]
    values <- suppressWarnings(as.numeric(tokens))
    if (anyNA(values)) {
      stop("Phase 0 has a nonnumeric movement-matrix row: ", row, call. = FALSE)
    }
    subnormal <- abs(values) < .Machine$double.xmin
    tokens[subnormal] <- "0.00000000000000e+00"
    lines[[row]] <- paste0(" ", paste(tokens, collapse = " "))
  }
  canonical <- tempfile("bet-phase0-", fileext = ".par")
  on.exit(unlink(canonical, force = TRUE), add = TRUE)
  writeLines(lines, canonical, useBytes = TRUE)
  sha256_file(canonical)
}

package_sha <- function(package) {
  description <- packageDescription(package)
  sha <- description[["RemoteSha"]]
  if (is.null(sha) || !nzchar(sha)) sha <- description[["GithubSHA1"]]
  if (is.null(sha) || !nzchar(sha)) NA_character_ else as.character(sha)
}

assert_package_sha <- function(package, expected) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Required runtime package is absent: ", package, call. = FALSE)
  }
  actual <- package_sha(package)
  if (!identical(actual, expected)) {
    stop(
      package, " runtime SHA mismatch: expected ", expected,
      ", observed ", actual, call. = FALSE
    )
  }
  invisible(actual)
}

assert_package_version <- function(package, expected) {
  actual <- as.character(utils::packageVersion(package))
  if (!identical(actual, expected)) {
    stop(
      package, " runtime version mismatch: expected ", expected,
      ", observed ", actual, call. = FALSE
    )
  }
  invisible(actual)
}

assert_equal_scalar <- function(actual, expected, label) {
  if (length(actual) != 1L || is.na(actual) || !identical(actual, expected)) {
    stop(label, " is not the required value.", call. = FALSE)
  }
}

assert_numeric <- function(actual, expected, label, tolerance = 1e-12) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  if (length(actual) != length(expected)) {
    stop(label, " has a different length.", call. = FALSE)
  }
  missing_match <- is.na(actual) == is.na(expected)
  finite <- !is.na(actual) & !is.na(expected)
  within <- rep(TRUE, length(actual))
  within[finite] <- abs(actual[finite] - expected[finite]) <=
    tolerance * pmax(1, abs(expected[finite]))
  if (!all(missing_match & within)) {
    first <- which(!(missing_match & within))[[1L]]
    stop(
      label, " differs at row ", first, ": expected ", expected[[first]],
      ", observed ", actual[[first]], call. = FALSE
    )
  }
  invisible(TRUE)
}

assert_columns <- function(actual, expected, columns, label,
                           numeric_tolerance = 1e-12) {
  if (!is.data.frame(actual) || !is.data.frame(expected) ||
      nrow(actual) != nrow(expected)) {
    stop(label, " does not have the expected rows.", call. = FALSE)
  }
  absent <- setdiff(columns, intersect(names(actual), names(expected)))
  if (length(absent)) {
    stop(label, " is missing columns: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  for (column in columns) {
    lhs <- actual[[column]]
    rhs <- expected[[column]]
    if (is.numeric(lhs) || is.integer(lhs) || is.numeric(rhs) || is.integer(rhs)) {
      assert_numeric(lhs, rhs, paste0(label, "$", column), numeric_tolerance)
    } else if (!identical(lhs, rhs)) {
      stop(label, "$", column, " differs.", call. = FALSE)
    }
  }
  invisible(TRUE)
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
    stop("PAR has an invalid scalar after ", label, ": ", path, call. = FALSE)
  }
  value
}

par_flag <- function(path, index) {
  lines <- readLines(path, warn = FALSE)
  headers <- which(trimws(lines) == "# The parest_flags")
  if (!length(headers)) stop("PAR has no parest_flags section.", call. = FALSE)
  selected <- vapply(headers, function(header) {
    value_row <- header + 1L
    while (value_row <= length(lines) && !nzchar(trimws(lines[[value_row]]))) {
      value_row <- value_row + 1L
    }
    values <- scan(text = lines[[value_row]], quiet = TRUE)
    if (length(values) < index) {
      stop("PAR parest_flags is too short.", call. = FALSE)
    }
    values[[index]]
  }, numeric(1L))
  if (length(unique(selected)) != 1L) {
    stop("PAR parest_flags copies disagree.", call. = FALSE)
  }
  selected[[1L]]
}

assert_phase1_flags <- function(actual_path, expected_path) {
  actual <- FLR4MFCL::read.MFCLPar(actual_path)
  expected <- FLR4MFCL::read.MFCLPar(expected_path)
  flag_slots <- c(
    "flags", "control_flags", "tag_fish_rep_flags",
    "catch_dev_coffs_flag", "historic_flags"
  )
  absent <- setdiff(flag_slots, intersect(
    methods::slotNames(actual), methods::slotNames(expected)
  ))
  if (length(absent)) {
    stop("Phase 1 PAR is missing flag slots: ", paste(absent, collapse = ", "),
         call. = FALSE)
  }
  for (slot in flag_slots) {
    if (!identical(methods::slot(actual, slot), methods::slot(expected, slot))) {
      stop("Phase 1 PAR flag slot differs: ", slot, call. = FALSE)
    }
  }
  invisible(TRUE)
}

copy_required <- function(source, target, mode = NULL) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE, mode = "0700")
  if (!file.copy(source, target, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)) {
    stop("Could not stage ", source, call. = FALSE)
  }
  if (!is.null(mode)) Sys.chmod(target, mode = mode)
  invisible(target)
}

image <- Sys.getenv("BET_JITTER_IMAGE", unset = "")
assert_equal_scalar(image, expected_image, "Container image reference")
assert_equal_scalar(R.version$major, "4", "R major version")
assert_equal_scalar(R.version$minor, "6.0", "R minor version")
assert_package_version("mfclkit", expected_mfclkit_version)
assert_package_sha("mfclkit", expected_mfclkit_sha)
assert_package_version("FLR4MFCL", expected_flr4mfcl_version)
assert_package_sha("FLR4MFCL", expected_flr4mfcl_sha)

RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
assert_equal_scalar(RNGkind()[[1L]], "Mersenne-Twister", "R RNG kind")
assert_equal_scalar(RNGkind()[[2L]], "Inversion", "R normal RNG kind")
assert_equal_scalar(RNGkind()[[3L]], "Rejection", "R sample RNG kind")

mfcl <- "/home/mfcl/mfclo64"
assert_sha(mfcl, expected_mfcl_sha)
if (file.access(mfcl, mode = 1L) != 0L) {
  stop("The pinned MFCL executable is not executable.", call. = FALSE)
}

runtime_model_relative <- c(
  "bet.frq",
  "bet.ini",
  "bet.tag",
  "bet.age_length",
  "bet.reg_scaling",
  "mfcl.cfg",
  "doitall.sh"
)
documentation_reference_relative <- c(
  "model-inputs/S0.90-F2.conf",
  "selectivity-models/F2.csv"
)
model_relative <- c(runtime_model_relative, documentation_reference_relative)
model_expected_sha <- c(
  "d0d84f0a498e6a62681f2a58ffc1ba53dab9e3d6af856b4ad1fd907196250004",
  "fbd064c3d0ccb4d2e1b9beb06fe3eacf0180677821e6a1773d20b308474d984e",
  "b140e66eb52f2b7e022ef2c562134f8bc9baf3dede18ce95283a001acd2b013f",
  "426859b825bd815aa69c8d97c9dd93097027ed1eb6b9e444d88b69562097a00c",
  "5f047ddb4053d1f6df9ace18e85e440b11553de246d024ce8138b427f5f9f7e3",
  "2ec8a291fae62c6f37541aec1de37444626d42b3290b371bb42b63d510034eae",
  "ad8ca660b6d84f9bbd1d8024f616a5bd66047a44ecc2c6e8b5c61c6be089fc5c",
  "1a0d0fffec49c033100f3e9c76bfd05a7e3ed4ddfd701221f7199659dfcd9c11",
  "790e21a01054349a20f4fbbb7db926f6452d059344815a3a9d6a5de51db3310a"
)
names(model_expected_sha) <- model_relative
model_root <- file.path(repo, "data", "diagnostic", "mfcl")
repository_mfcl <- file.path(model_root, "mfclo64")
assert_sha(repository_mfcl, expected_mfcl_sha)
model_source <- file.path(model_root, model_relative)
for (index in seq_along(model_source)) {
  assert_sha(model_source[[index]], model_expected_sha[[index]])
}
embedded_selectivity <- validate_embedded_selectivity(
  file.path(model_root, "doitall.sh"),
  file.path(model_root, "selectivity-models", "F2.csv")
)

reference_root <- file.path(repo, "data", "diagnostic", "reproduction")
fitted_par_source <- file.path(reference_root, "fitted-reference", "final.par")
fitted_indepvar_source <- file.path(reference_root, "fitted-reference", "indepvar.rpt")
phase1_source <- file.path(reference_root, "phase1-reference", "mfk_phase1_baseline.par")
plans_source <- file.path(reference_root, "jitter-plans.rds")
phase1_artifact_expected_sha <- c(
  "fitted_active_xinit.rpt" = "cc723c20b1a7b3e71cb32e45cc9f9d8edecdbea9906be8ddf315cdadf36af771",
  "indepvar.rpt" = "460bd04bf8200b68d5023debc1534258fa2b2797d95c1c8dc7f1c4b8fbb2f2f1",
  "jitter_deferred_mapping.rds" = "01054f1d881ed9dabfdd2634dd48418286dcca217b53be4960f4f6331df43b1d",
  "jitter_deferred_xinit.rpt" = "c5c128e3576e00ef6192227d58a7719f9b3020d9176ca3d99e56bf4107c0a60a",
  "jitter_fitted_active_mapping.rds" = "829e341b2af1a7715069f30f1f93675543a2805f4c480007ea2c4ac3fcbb7d5a",
  "jitter_mask_coverage.csv" = "c9b0f1e85c0946004fb723df2cc4298b1a3445fb6a7f221dfe7a0f6eb964eab3",
  "jitter_phase1_mapping.rds" = "926d60f0040529d0e4ef8c65f918da21c75bd80b98064898489ed1edd7a43bc2",
  "mfk_phase1_baseline.par" = expected_phase1_sha,
  "phase1_reference.rds" = "9a7481c58359abf7e769af1775d5f0b7a0f8d75b81988c1bf4ce2c2cfe4e694f",
  "phase1_xinit.rpt" = "4e5da360aee944923284df58b336846a669085d0ea1180aa29b7a8faba6e8c48",
  "xinit.rpt" = "4e5da360aee944923284df58b336846a669085d0ea1180aa29b7a8faba6e8c48"
)
reference_expected_sha <- c(
  "fitted-reference/final.par" = "21dcaea9db8c89ddc8c29fa3c3a5e514b50bef6e26587c168c00c05f35fbebc3",
  "fitted-reference/indepvar.rpt" = "5792c57d7bcce5e679faa5adac63e6d20886fc478f03a29277814616e313b490",
  stats::setNames(
    unname(phase1_artifact_expected_sha),
    file.path("phase1-reference", names(phase1_artifact_expected_sha))
  ),
  "jitter-plans.rds" = "8c05a9e616de9c7f0192682d5bf09d831498c4aa9b16ebc5af59b6bb43957239"
)
for (relative in names(reference_expected_sha)) {
  assert_sha(file.path(reference_root, relative), reference_expected_sha[[relative]])
}
assert_par_selectivity(fitted_par_source, embedded_selectivity)
assert_par_selectivity(phase1_source, embedded_selectivity)

phase1_reference_dir <- file.path(reference_root, "phase1-reference")
phase1_mapping_files <- c(
  "jitter_fitted_active_mapping.rds",
  "jitter_phase1_mapping.rds",
  "jitter_deferred_mapping.rds"
)
phase1_reference_mappings <- stats::setNames(
  lapply(phase1_mapping_files, function(name) {
    readRDS(file.path(phase1_reference_dir, name))
  }),
  phase1_mapping_files
)
expected_mapping_rows <- c(1997L, 1989L, 8L)
if (!identical(
  vapply(phase1_reference_mappings, nrow, integer(1L)),
  stats::setNames(expected_mapping_rows, phase1_mapping_files)
)) {
  stop("Reference Phase 1 semantic mappings are incomplete.", call. = FALSE)
}

plans <- readRDS(plans_source)
assert_equal_scalar(plans$schema, "ofp-sam-bet-2026-jitter-plans.v1", "Plan schema")
assert_equal_scalar(plans$design, "structure_aware_single_cv_v1", "Jitter design")
assert_numeric(plans$cv, jitter_cv, "Jitter CV", tolerance = 0)
if (!identical(as.integer(plans$seeds), accepted_seeds)) {
  stop("Reference plans are not the exact 25 accepted seeds.", call. = FALSE)
}
assert_equal_scalar(plans$mfclkit_commit, reference_mfclkit_sha, "Reference-plan mfclkit SHA")
if (!identical(names(plans$plans), as.character(accepted_seeds))) {
  stop("Reference plans are not in accepted-seed order.", call. = FALSE)
}

expected_family_counts <- c(
  diff_coffs = 56L,
  fish_pars23 = 8L,
  log_mean_recruitment = 1L,
  recruit_dev = 285L,
  region_parameters_row1 = 5L,
  regional_recruitment_variation = 1455L,
  seasonal_growth_sv21 = 1L,
  selectivity_coff = 169L,
  tag_fish_rep_group = 12L,
  var_coff = 2L,
  vb_coff = 3L
)
expected_method_counts <- c(
  bounded_simplex_logratio = 5L,
  mean_preserving_log_multiplier = 286L,
  mean_preserving_scaled_beta = 1706L
)
plan_columns <- c(
  "Index", "Var_name", "family", "before", "after", "L_bound", "U_bound",
  "target", "p1", "p2", "p3", "p4", "jitter_method", "jitter_space",
  "jitter_scale", "jitter_center", "jitter_standardized_shift",
  "jitter_effective_cv", "jitter_max_safe_cv", "jitter_cv_limited",
  "jitter_safe_cv_cap_enabled", "boundary_adjusted", "nominal_cv", "seed"
)

reference_results <- vector("list", length(accepted_seeds))
names(reference_results) <- as.character(accepted_seeds)
for (seed in accepted_seeds) {
  key <- as.character(seed)
  plan <- plans$plans[[key]]
  if (!is.data.frame(plan) || nrow(plan) != 1997L ||
      !all(plan_columns %in% names(plan)) ||
      !identical(sort(as.integer(plan$Index)), 1:1997) ||
      anyDuplicated(plan$Index) || !all(as.integer(plan$seed) == seed)) {
    stop("Reference plan is incomplete for seed ", seed, ".", call. = FALSE)
  }
  assert_numeric(plan$nominal_cv, rep(jitter_cv, nrow(plan)),
                 paste0("seed ", seed, " nominal CV"), tolerance = 0)
  family_counts <- table(plan$family)
  method_counts <- table(plan$jitter_method)
  if (!identical(as.integer(family_counts[names(expected_family_counts)]),
                 as.integer(expected_family_counts)) ||
      !identical(as.integer(method_counts[names(expected_method_counts)]),
                 as.integer(expected_method_counts))) {
    stop("Reference plan family/method counts differ for seed ", seed, ".", call. = FALSE)
  }
  deferred <- which(plan$family == "fish_pars23")
  if (!identical(deferred, 1990:1997) ||
      !identical(as.integer(plan$Index[deferred]), 1967:1974)) {
    stop("Reference deferred plan order differs for seed ", seed, ".", call. = FALSE)
  }

  result_file <- file.path(
    repo, "data", "diagnostic", "jitter", paste0("jitter_seed_", seed),
    "jitter_result.rds"
  )
  result <- readRDS(result_file)
  if (!identical(as.integer(result$seed), seed) || !isTRUE(result$run_completed) ||
      !is.finite(result$obj_fun) || !is.finite(result$max_grad) ||
      abs(result$max_grad) > 1e-4) {
    stop("Reference result is not an accepted fit for seed ", seed, ".", call. = FALSE)
  }
  payload_plan <- result$parameter_changes$labels
  assert_columns(payload_plan, plan, plan_columns,
                 paste0("seed ", seed, " recovered jitter plan"), 1e-13)
  reference_results[[key]] <- result
}

# Recompute the eight Phase-2 proposal values without running Phase 2. This
# verifies the deferred RNG stream as part of prepare-only mode.
jitter_value <- getFromNamespace("mfk_jitter_indepvar_value", "mfclkit")
family_scales <- getFromNamespace("mfk_jitter_family_scales", "mfclkit")
deferred_seed <- function(seed, phase = 2L) {
  value <- (as.double(seed) * 104729 + as.double(phase) * 1009 + 17) %% 2147483000
  as.integer(max(1, value))
}
for (seed in accepted_seeds) {
  plan <- plans$plans[[as.character(seed)]]
  deferred <- plan[plan$family == "fish_pars23", , drop = FALSE]
  set.seed(deferred_seed(seed, 2L))
  scales <- family_scales(data.frame(
    family = deferred$family,
    target = deferred$target,
    Estimate = deferred$before,
    stringsAsFactors = FALSE
  ))
  proposal <- vapply(seq_len(nrow(deferred)), function(row) {
    jitter_value(
      value = deferred$before[[row]], cv = jitter_cv,
      lower = deferred$L_bound[[row]], upper = deferred$U_bound[[row]],
      family = deferred$family[[row]], target = deferred$target[[row]],
      family_scale = scales[[row]], details = TRUE
    )$value
  }, numeric(1L))
  assert_numeric(proposal, deferred$after,
                 paste0("seed ", seed, " deferred jitter plan"), 1e-13)
}

case_dir <- file.path(output, "case")
reference_dir <- file.path(output, "reference")
model_dir <- file.path(output, "work")
dir.create(case_dir, recursive = TRUE, mode = "0700")
dir.create(reference_dir, recursive = TRUE, mode = "0700")
dir.create(model_dir, recursive = TRUE, mode = "0700")
for (relative in runtime_model_relative) {
  index <- match(relative, model_relative)
  target <- file.path(case_dir, relative)
  copy_required(
    model_source[[index]], target,
    mode = if (identical(relative, "doitall.sh")) "0755" else NULL
  )
}
observed_case_files <- sort(list.files(case_dir, recursive = TRUE, all.files = FALSE))
if (!identical(observed_case_files, sort(runtime_model_relative))) {
  stop(
    "The fresh native case must contain only the five bet files, mfcl.cfg, ",
    "and doitall.sh; model .conf and selectivity .csv files are audit-only.",
    call. = FALSE
  )
}
fitted_par <- file.path(reference_dir, "final.par")
fitted_indepvar <- file.path(reference_dir, "indepvar.rpt")
phase1_reference <- file.path(reference_dir, "mfk_phase1_baseline.par")
copy_required(fitted_par_source, fitted_par)
copy_required(fitted_indepvar_source, fitted_indepvar)
copy_required(phase1_source, phase1_reference)

Sys.setenv(
  PROGRAM_PATH = mfcl,
  MODEL_ID = "S0.90-F2",
  BET_PHASE10_11_CONVERGENCE = as.character(phase10_convergence),
  BET_JITTER_MAX_EVALS = "5000",
  MFK_RUN_MESSAGES = "false"
)

# The recipe's first optimization input is an audited transformation of the
# makepar output rather than the makepar output itself. mfclkit requires that
# handoff to exist at preflight, so create it from the raw inputs first. The
# Phase-1-only recipe then performs the same Phase 0 again before Phase 1.
phase0_log <- file.path(output, "phase0-preflight.log")
old_wd <- setwd(case_dir)
phase0_status <- system2(
  "./doitall.sh",
  stdout = phase0_log,
  stderr = phase0_log,
  env = c(
    paste0("PROGRAM_PATH=", mfcl),
    "MODEL_ID=S0.90-F2",
    paste0("BET_PHASE10_11_CONVERGENCE=", phase10_convergence),
    "MODEL_STOP_AFTER_PHASE0=1"
  )
)
setwd(old_wd)
if (!identical(as.integer(phase0_status), 0L)) {
  stop("The model-specific Phase 0 preflight failed; see ", phase0_log, ".",
       call. = FALSE)
}
phase0_par <- file.path(case_dir, "00.fixed.par")
phase0_raw_sha <- sha256_file(phase0_par)
phase0_normalized_sha <- phase0_semantic_sha(phase0_par)
if (!identical(phase0_normalized_sha, expected_phase0_semantic_sha)) {
  stop(
    "Phase 0 semantic SHA-256 mismatch: expected ",
    expected_phase0_semantic_sha, ", observed ", phase0_normalized_sha,
    call. = FALSE
  )
}

suppressPackageStartupMessages(library(mfclkit))
backend <- mfk_native_backend(program_path = mfcl, project_dir = model_dir)
message(
  if (run_native) {
    "Executing the 25 accepted native jitter fits."
  } else {
    "Preparing the 25 accepted jitter starts; native Phase 2-11 fits are disabled."
  }
)
result <- mfk_run_jitter_phase1_doitall(
  backend = backend,
  input_dir = case_dir,
  model_dir = model_dir,
  seeds = accepted_seeds,
  cv = jitter_cv,
  par = fitted_par,
  jitter_args = list(),
  run = run_native,
  hessian = FALSE,
  keep_run_result = FALSE,
  tag_mixing_fix = "auto",
  n_mixing_periods = 2L,
  allow_new_ini_version_write = FALSE,
  require_indepvar = TRUE,
  reuse_phase1 = FALSE,
  base_stage = "phase1",
  strict_active_mask = FALSE,
  fitted_indepvar = fitted_indepvar,
  convergence_exponent = phase11_convergence,
  parallel = FALSE,
  run_messages = FALSE
)
if (!is.list(result) || length(result) != length(accepted_seeds)) {
  stop("mfclkit did not return exactly 25 results.", call. = FALSE)
}
result_seeds <- vapply(result, function(x) as.integer(x$seed), integer(1L))
if (!identical(result_seeds, accepted_seeds)) {
  stop("mfclkit result order is not the accepted-seed order.", call. = FALSE)
}

generated_phase1 <- file.path(
  model_dir, "jitter", "_phase1_reference", "mfk_phase1_baseline.par"
)
generated_phase1_sha <- sha256_file(generated_phase1)
assert_phase1_flags(generated_phase1, phase1_source)
generated_phase1_dir <- dirname(generated_phase1)
for (name in phase1_mapping_files) {
  actual_mapping <- readRDS(file.path(generated_phase1_dir, name))
  expected_mapping <- phase1_reference_mappings[[name]]
  assert_columns(
    actual_mapping, expected_mapping, names(expected_mapping),
    paste0("fresh Phase 1 ", name), numeric_tolerance = 1e-12
  )
}

phase_controls <- function(script, input_par, output_par) {
  lines <- readLines(script, warn = FALSE)
  pattern <- paste0(
    "(^|[[:space:]])", gsub("[.]", "[.]", input_par),
    "[[:space:]]+", gsub("[.]", "[.]", output_par),
    "([[:space:]]|$)"
  )
  start <- grep(pattern, lines)
  if (length(start) != 1L || !grepl("<<-?", lines[[start]])) {
    stop("Could not identify ", input_par, " -> ", output_par,
         " in ", script, call. = FALSE)
  }
  marker <- regmatches(lines[[start]], regexpr("<<-?[^[:space:]]+", lines[[start]]))
  delimiter <- sub("^<<-?", "", marker)
  delimiter <- sub("^[\"']", "", delimiter)
  delimiter <- sub("[\"']$", "", delimiter)
  following <- if (start < length(lines)) (start + 1L):length(lines) else integer()
  end <- following[trimws(lines[following]) == delimiter]
  if (!length(end)) stop("Missing heredoc delimiter in ", script, call. = FALSE)
  body <- lines[(start + 1L):(end[[1L]] - 1L)]
  rows <- lapply(body, function(line) {
    tokens <- strsplit(trimws(sub("#.*$", "", line)), "[[:space:]]+")[[1L]]
    if (length(tokens) < 3L || !nzchar(tokens[[1L]])) return(NULL)
    starts <- seq.int(1L, length(tokens) - 2L, by = 3L)
    do.call(rbind, lapply(starts, function(position) {
      data.frame(
        flagtype = suppressWarnings(as.integer(tokens[[position]])),
        flag = suppressWarnings(as.integer(tokens[[position + 1L]])),
        value = tokens[[position + 2L]], stringsAsFactors = FALSE
      )
    }))
  })
  rows <- Filter(function(x) is.data.frame(x) && nrow(x), rows)
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}

resolve_control <- function(value) {
  if (identical(value, "$phase10_11_convergence") ||
      identical(value, "${phase10_11_convergence}")) {
    return(as.numeric(Sys.getenv("BET_PHASE10_11_CONVERGENCE")))
  }
  suppressWarnings(as.numeric(value))
}

validation <- vector("list", length(accepted_seeds))
for (position in seq_along(accepted_seeds)) {
  seed <- accepted_seeds[[position]]
  key <- as.character(seed)
  info <- result[[position]]
  expected_plan <- plans$plans[[key]]
  seed_dir <- file.path(model_dir, "jitter", paste0("jitter_seed_", seed))
  script <- file.path(seed_dir, "doitall_jitter.sh")
  controls10 <- phase_controls(script, "09.par", "10.par")
  controls11 <- phase_controls(script, "10.par", "11.par")
  get_control <- function(controls, flag) {
    row <- which(controls$flagtype == 1L & controls$flag == flag)
    if (length(row) != 1L) {
      stop("Expected one parest flag ", flag, " control for seed ", seed, ".", call. = FALSE)
    }
    resolve_control(controls$value[[row]])
  }
  p10_mgc <- get_control(controls10, 50L)
  p11_mgc <- get_control(controls11, 50L)
  p10_evals <- get_control(controls10, 1L)
  p11_evals <- get_control(controls11, 1L)
  if (!identical(p10_mgc, as.numeric(phase10_convergence)) ||
      !identical(p11_mgc, as.numeric(phase11_convergence)) ||
      !identical(p10_evals, 10000) || !identical(p11_evals, 5000)) {
    stop("Phase 10/11 controls are not the strict reproduction contract for seed ",
         seed, ".", call. = FALSE)
  }

  actual_plan <- info$parameter_changes$labels
  expected_initial <- expected_plan[expected_plan$family != "fish_pars23", , drop = FALSE]
  if (run_native) {
    assert_columns(actual_plan, expected_plan, plan_columns,
                   paste0("seed ", seed, " generated jitter plan"), 1e-12)
  } else {
    assert_columns(actual_plan, expected_initial, plan_columns,
                   paste0("seed ", seed, " generated Phase-1 jitter plan"), 1e-12)
  }

  expected_result <- reference_results[[key]]
  objective <- gradient <- objective_difference <- gradient_difference <- NA_real_
  output_par <- output_sha <- NA_character_
  final_parameter_max_abs_difference <- NA_real_
  if (run_native) {
    if (!isTRUE(info$state$run_completed)) {
      stop("Native jitter fit did not complete for seed ", seed, ".", call. = FALSE)
    }
    objective <- as.numeric(info$state$obj_fun)
    gradient <- abs(as.numeric(info$state$max_grad))
    objective_difference <- abs(objective - as.numeric(expected_result$obj_fun))
    gradient_difference <- abs(gradient - abs(as.numeric(expected_result$max_grad)))
    if (!is.finite(objective) || !is.finite(gradient) || gradient > 1e-4 ||
        objective_difference > 1e-8 || gradient_difference > 1e-12) {
      stop("Objective/MGC numerical validation failed for seed ", seed, ".", call. = FALSE)
    }

    final_actual <- info$fitted_parameter_changes$labels
    final_expected <- expected_result$fitted_parameter_changes$labels
    final_columns <- c("Index", "Var_name", "family", "after")
    assert_columns(final_actual, final_expected, final_columns,
                   paste0("seed ", seed, " fitted parameters"), 1e-11)
    final_parameter_max_abs_difference <- max(
      abs(as.numeric(final_actual$after) - as.numeric(final_expected$after)),
      na.rm = TRUE
    )

    output_par <- file.path(seed_dir, info$output_par)
    if (!file.exists(output_par)) {
      stop("Native output PAR is absent for seed ", seed, ".", call. = FALSE)
    }
    if (!identical(par_flag(output_par, 50L), as.numeric(phase11_convergence))) {
      stop("Final PAR does not retain strict parest flag 50 for seed ", seed, ".", call. = FALSE)
    }
    par_objective <- par_scalar(output_par, "# Objective function value")
    par_gradient <- abs(par_scalar(output_par, "# Maximum magnitude gradient value"))
    if (abs(par_objective - objective) > 1e-8 || abs(par_gradient - gradient) > 1e-12) {
      stop("Final PAR metadata does not match the run result for seed ", seed, ".", call. = FALSE)
    }
    output_sha <- sha256_file(output_par)
  }

  validation[[position]] <- data.frame(
    seed = seed,
    mode = if (run_native) "full-native" else "prepare-only",
    jitter_plan_rows = nrow(expected_plan),
    phase1_plan_rows = sum(expected_plan$family != "fish_pars23"),
    deferred_plan_rows = sum(expected_plan$family == "fish_pars23"),
    phase1_raw_sha256 = generated_phase1_sha,
    phase1_reference_raw_sha256 = expected_phase1_sha,
    phase1_semantic_identity_count = 1997L,
    phase10_flag50 = p10_mgc,
    phase11_flag50 = p11_mgc,
    objective = objective,
    objective_abs_difference = objective_difference,
    max_grad = gradient,
    max_grad_abs_difference = gradient_difference,
    final_parameter_max_abs_difference = final_parameter_max_abs_difference,
    output_par = if (is.na(output_par)) NA_character_ else sub(
      paste0("^", output, "/?"), "", output_par
    ),
    output_par_sha256 = output_sha,
    stringsAsFactors = FALSE
  )
}
validation <- do.call(rbind, validation)

seed_dirs <- list.dirs(file.path(model_dir, "jitter"), recursive = FALSE, full.names = FALSE)
seed_dirs <- seed_dirs[grepl("^jitter_seed_[0-9]+$", seed_dirs)]
observed_seeds <- sort(as.integer(sub("^jitter_seed_", "", seed_dirs)))
if (!identical(observed_seeds, accepted_seeds)) {
  stop("Prepared directories are not exactly the 25 accepted seeds.", call. = FALSE)
}

reference_roles <- c(
  "fitted-mask-par", "fitted-mask-indepvar",
  rep("phase1-reference", length(phase1_artifact_expected_sha)),
  "jitter-plan-reference"
)
input_manifest <- rbind(
  data.frame(
    role = c(
      rep("native-runtime-input", length(runtime_model_relative)),
      rep("documentation-reference", length(documentation_reference_relative))
    ),
    path = file.path("data/diagnostic/mfcl", model_relative),
    sha256 = unname(model_expected_sha), stringsAsFactors = FALSE
  ),
  data.frame(
    role = reference_roles,
    path = file.path("data/diagnostic/reproduction", names(reference_expected_sha)),
    sha256 = unname(reference_expected_sha), stringsAsFactors = FALSE
  ),
  data.frame(
    role = c("repository-mfcl-executable", "runtime-mfcl-executable"),
    path = c("data/diagnostic/mfcl/mfclo64", mfcl),
    sha256 = rep(expected_mfcl_sha, 2L), stringsAsFactors = FALSE
  )
)
utils::write.csv(input_manifest, file.path(output, "input-manifest.csv"), row.names = FALSE)
utils::write.csv(validation, file.path(output, "validation.csv"), row.names = FALSE)
saveRDS(result, file.path(output, "mfclkit-result.rds"), compress = "xz")
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"), useBytes = TRUE)

manifest <- data.frame(
  key = c(
    "schema", "mode", "container_image", "R_version", "mfclkit_sha",
    "mfclkit_version", "FLR4MFCL_sha", "FLR4MFCL_version",
    "MFCL_sha256", "MFCL_version", "reference_mfclkit_sha",
    "seeds", "seed_count", "jitter_cv", "phase10_flag50", "phase11_flag50",
    "phase0_raw_sha256", "phase0_semantic_sha256",
    "phase1_generated_raw_sha256", "phase1_reference_raw_sha256",
    "phase1_semantic_identity_count", "jitter_plan_sha256", "status"
  ),
  value = c(
    "ofp-sam-bet-2026-jitter-reproduction.v1",
    if (run_native) "full-native" else "prepare-only",
    expected_image,
    paste(R.version$major, R.version$minor, sep = "."),
    expected_mfclkit_sha,
    expected_mfclkit_version,
    expected_flr4mfcl_sha,
    expected_flr4mfcl_version,
    expected_mfcl_sha,
    "2.2.7.9",
    reference_mfclkit_sha,
    paste(accepted_seeds, collapse = " "),
    as.character(length(accepted_seeds)),
    format(jitter_cv, scientific = FALSE),
    as.character(phase10_convergence),
    as.character(phase11_convergence),
    phase0_raw_sha,
    expected_phase0_semantic_sha,
    generated_phase1_sha,
    expected_phase1_sha,
    "1997",
    reference_expected_sha[["jitter-plans.rds"]],
    "validated"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(manifest, file.path(output, "reproduction-manifest.csv"), row.names = FALSE)

message(
  if (run_native) {
    "All 25 accepted jitter fits passed plan, parameter, objective, and MGC validation."
  } else {
    "Prepared and validated the exact 25 accepted jitter starts and Phase-1 baseline."
  }
)
