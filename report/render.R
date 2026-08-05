model_dir <- "data/S0.90-F2-tau2-fixed"
output_dir <- "results"
regional_file <- file.path(model_dir, "jitter-regional-timeseries.rds")
stock_status_file <- file.path(model_dir, "jitter-stock-status-timeseries.rds")

if (!requireNamespace("mfclshiny", quietly = TRUE)) {
  stop("Install the pinned mfclshiny report runtime before rendering.", call. = FALSE)
}

provenance <- data.frame(
  model_label = "Diagnostic model",
  jitter_id = "jitter_seed",
  model_id = "",
  model_job = "",
  jitter_job = "",
  stringsAsFactors = FALSE
)

if (!file.exists(regional_file)) {
  stop("The compact regional jitter payload is missing.", call. = FALSE)
}
regional_data <- readRDS(regional_file)
if (!file.exists(stock_status_file)) {
  stop("The compact stock-status jitter payload is missing.", call. = FALSE)
}
stock_status_data <- readRDS(stock_status_file)

mfclshiny::build_jitter_report(
  model_dir = model_dir,
  output_dir = output_dir,
  title = "Diagnostic model jitter",
  provenance = provenance,
  regional = TRUE,
  regional_quantities = c("depletion", "recruitment_deviation"),
  regional_data = regional_data,
  stock_status_data = stock_status_data,
  trajectory_style = "distribution",
  reference_label = "Diagnostic model (unjittered)",
  show_objective_reference_line = FALSE,
  grad_reference = 1e-4,
  formats = c("png", "pdf"),
  width = 11,
  height = 7,
  dpi = 300,
  render_html = TRUE,
  show_management_table = FALSE
)
