expected_selectivity_reference_sha256 <-
  "790e21a01054349a20f4fbbb7db926f6452d059344815a3a9d6a5de51db3310a"

validate_embedded_selectivity <- function(doitall, reference_csv) {
  doitall <- normalizePath(doitall, winslash = "/", mustWork = TRUE)
  reference_csv <- normalizePath(reference_csv, winslash = "/", mustWork = TRUE)

  reference_sha <- unname(tools::sha256sum(reference_csv))
  if (!identical(reference_sha, expected_selectivity_reference_sha256)) {
    stop(
      "The F2 selectivity reference CSV is not the checksum-locked file.",
      call. = FALSE
    )
  }

  output <- system2(
    doitall,
    stdout = TRUE,
    stderr = TRUE,
    env = "SELECTIVITY_PRINT_CONTROLS=1"
  )
  status <- attr(output, "status")
  if ((!is.null(status) && status != 0L) || length(output) != 198L) {
    stop(
      "doitall.sh must expose exactly 198 embedded selectivity controls ",
      "(165 in Phase 1 and 33 in Phase 5).",
      call. = FALSE
    )
  }

  parse_control <- function(line) {
    fields <- strsplit(
      trimws(sub("[[:space:]]+#.*$", "", line)),
      "[[:space:]]+"
    )[[1L]]
    if (length(fields) != 3L || !grepl("^-[0-9]+$", fields[[1L]])) {
      stop("Invalid embedded selectivity control: ", line, call. = FALSE)
    }
    values <- suppressWarnings(as.integer(fields))
    if (anyNA(values)) {
      stop("Nonnumeric embedded selectivity control: ", line, call. = FALSE)
    }
    c(fishery = -values[[1L]], flag = values[[2L]], value = values[[3L]])
  }
  embedded <- as.data.frame(
    do.call(rbind, lapply(output, parse_control)),
    stringsAsFactors = FALSE
  )
  embedded[] <- lapply(embedded, as.integer)

  reference <- utils::read.csv(
    reference_csv,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  expected_columns <- c(
    "fishery", "fishery_name", "flag16", "flag24", "flag56", "flag57", "flag61"
  )
  if (
    !identical(names(reference), expected_columns) ||
      nrow(reference) != 33L ||
      !identical(as.integer(reference$fishery), seq_len(33L))
  ) {
    stop("The F2 selectivity reference must contain the exact 33 fisheries.", call. = FALSE)
  }

  flag_columns <- c("flag16", "flag24", "flag56", "flag57", "flag61")
  expected_text <- unlist(lapply(seq_len(nrow(reference)), function(row) {
    fishery <- as.integer(reference$fishery[[row]])
    name <- as.character(reference$fishery_name[[row]])
    c(
      sprintf(
        "  -%d 16 %d  # %s flag 16 from explicit input",
        fishery, as.integer(reference$flag16[[row]]), name
      ),
      sprintf(
        "  -%d 24 %d  # %s selectivity-sharing group",
        fishery, as.integer(reference$flag24[[row]]), name
      ),
      sprintf(
        "  -%d 56 %d  # %s selectivity-penalty weight",
        fishery, as.integer(reference$flag56[[row]]), name
      ),
      sprintf(
        "  -%d 57 %d  # %s selectivity form",
        fishery, as.integer(reference$flag57[[row]]), name
      ),
      sprintf(
        "  -%d 61 %d  # %s spline-node count",
        fishery, as.integer(reference$flag61[[row]]), name
      )
    )
  }), use.names = FALSE)
  expected_text <- c(
    expected_text,
    vapply(seq_len(nrow(reference)), function(row) {
      sprintf(
        "  -%d 24 %d  # %s final selectivity-sharing group",
        as.integer(reference$fishery[[row]]),
        as.integer(reference$flag24[[row]]),
        as.character(reference$fishery_name[[row]])
      )
    }, character(1L))
  )
  if (!identical(output, expected_text)) {
    stop(
      "The 198 literal doitall control lines are not an exact rendering of ",
      "the checksum-locked F2 reference.",
      call. = FALSE
    )
  }

  expected_phase1 <- data.frame(
    fishery = rep(as.integer(reference$fishery), each = length(flag_columns)),
    flag = rep(c(16L, 24L, 56L, 57L, 61L), times = nrow(reference)),
    value = as.integer(as.vector(t(as.matrix(reference[flag_columns])))),
    stringsAsFactors = FALSE
  )
  expected_phase5 <- data.frame(
    fishery = as.integer(reference$fishery),
    flag = rep(24L, nrow(reference)),
    value = as.integer(reference$flag24),
    stringsAsFactors = FALSE
  )
  expected <- rbind(expected_phase1, expected_phase5)

  if (!identical(embedded, expected)) {
    mismatch <- which(
      embedded$fishery != expected$fishery |
        embedded$flag != expected$flag |
        embedded$value != expected$value
    )
    row <- if (length(mismatch)) mismatch[[1L]] else NA_integer_
    stop(
      "Embedded doitall selectivity controls differ from the checksum-locked ",
      "F2 reference", if (is.na(row)) "." else paste0(" at control ", row, "."),
      call. = FALSE
    )
  }

  final <- reference[c("fishery", flag_columns)]
  final[] <- lapply(final, as.integer)
  invisible(final)
}

assert_par_selectivity <- function(path, expected) {
  lines <- readLines(path, warn = FALSE)
  header <- which(trimws(lines) == "# fish flags")
  if (!length(header)) {
    stop("PAR has no fish-flags section: ", path, call. = FALSE)
  }
  start <- header[[1L]] + 1L
  rows <- lines[seq.int(start, min(start + 32L, length(lines)))]
  observed <- lapply(rows, function(line) scan(text = line, quiet = TRUE))
  if (length(observed) != 33L || any(lengths(observed) < 61L)) {
    stop("PAR does not contain 33 complete fish-flag rows: ", path, call. = FALSE)
  }
  observed <- do.call(rbind, lapply(observed, function(row) {
    as.integer(row[c(16L, 24L, 56L, 57L, 61L)])
  }))
  expected_values <- as.matrix(expected[c(
    "flag16", "flag24", "flag56", "flag57", "flag61"
  )])
  storage.mode(expected_values) <- "integer"
  if (!identical(unname(observed), unname(expected_values))) {
    stop("PAR selectivity flags differ from embedded F2 controls: ", path, call. = FALSE)
  }
  invisible(TRUE)
}
