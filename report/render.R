options(stringsAsFactors = FALSE)

required_packages <- c("ggplot2", "dplyr", "scales", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install the public report dependencies: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

model_dir <- "data/diagnostic"
output_dir <- "results"
figure_dir <- file.path(output_dir, "figures")
table_dir <- file.path(output_dir, "tables")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

read_payload <- function(name) {
  path <- file.path(model_dir, name)
  if (!file.exists(path)) stop("Missing public report payload: ", path, call. = FALSE)
  readRDS(path)
}

diagnostics <- read_payload("jitter-diagnostics.rds")
derived <- read_payload("jitter-derived-timeseries.rds")
regional <- read_payload("jitter-regional-timeseries.rds")
stock_status <- read_payload("jitter-stock-status-timeseries.rds")
endpoints <- read_payload("jitter-stock-status-endpoints.rds")

threshold <- 1e-4
diagnostics$converged <- diagnostics$run_completed %in% TRUE &
  is.finite(diagnostics$max_grad) & abs(diagnostics$max_grad) <= threshold
accepted_seeds <- diagnostics$seed[diagnostics$converged]

keep_accepted <- function(data) {
  data[
    data$is_reference %in% TRUE |
      data$is_base_fit_reference %in% TRUE |
      data$seed %in% accepted_seeds,
    ,
    drop = FALSE
  ]
}

derived <- keep_accepted(derived)
regional <- keep_accepted(regional)
stock_status <- keep_accepted(stock_status)
endpoints <- keep_accepted(endpoints)

diagnostic_obj <- 90814.9
diagnostic_mgc <- 9.68e-5

theme_report <- function(base_size = 11.5) {
  ggplot2::theme_bw(base_size = base_size, base_family = "serif") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E2E8EC", linewidth = 0.28),
      panel.border = ggplot2::element_rect(colour = "#263238", fill = NA, linewidth = 0.45),
      strip.background = ggplot2::element_rect(fill = "#E8F1F4", colour = "#B7C9D0", linewidth = 0.4),
      strip.text = ggplot2::element_text(face = "bold", colour = "#172B3A", size = base_size),
      axis.title = ggplot2::element_text(face = "bold", colour = "#172B3A"),
      axis.text = ggplot2::element_text(colour = "#334E5C"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.key.width = grid::unit(1.25, "cm"),
      plot.margin = ggplot2::margin(8, 12, 8, 10)
    )
}

save_plot <- function(plot, stem, width, height) {
  png <- file.path(figure_dir, paste0(stem, ".png"))
  pdf <- file.path(figure_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(png, plot, width = width, height = height, units = "in", dpi = 300, bg = "white")
  ggplot2::ggsave(pdf, plot, width = width, height = height, units = "in", bg = "white")
  invisible(c(png = png, pdf = pdf))
}

quantile_summary <- function(data) {
  data |>
    dplyr::filter(!.data$is_reference, !.data$is_base_fit_reference, is.finite(.data$value)) |>
    dplyr::group_by(.data$quantity, .data$year) |>
    dplyr::summarise(
      lower = stats::quantile(.data$value, 0.10, na.rm = TRUE, names = FALSE),
      median = stats::median(.data$value, na.rm = TRUE),
      upper = stats::quantile(.data$value, 0.90, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
}

line_colours <- c(
  "Included jitter fit" = "#7F8C91",
  "Central 80% jitter interval" = "#8ECAD5",
  "Jitter median" = "#164C63",
  "Diagnostic model (unjittered)" = "#C62828"
)

# Objective-function diagnostic: only the 25 accepted fits are displayed.
diag_plot_data <- diagnostics[diagnostics$converged, , drop = FALSE]
diagnostic_plot <- ggplot2::ggplot(
  diag_plot_data,
  ggplot2::aes(x = .data$max_grad, y = .data$obj_fun)
) +
  ggplot2::geom_point(colour = "#2C7F91", fill = "#78B8C6", shape = 21, size = 3.1, stroke = 0.65) +
  ggplot2::geom_point(
    data = data.frame(max_grad = diagnostic_mgc, obj_fun = diagnostic_obj),
    colour = "#C62828", fill = "#C62828", shape = 23, size = 4.0, stroke = 0.6
  ) +
  ggplot2::scale_x_continuous(labels = scales::label_scientific(digits = 2)) +
  ggplot2::scale_y_continuous(labels = scales::label_comma()) +
  ggplot2::labs(
    x = "Maximum gradient component (MGC)",
    y = "Objective function value"
  ) +
  theme_report(12.5) +
  ggplot2::theme(legend.position = "none")
save_plot(diagnostic_plot, "jitter-diagnostics-diagnostic-model", 7.2, 5.2)

# Four core annual trajectories.
derived$quantity <- factor(
  derived$quantity,
  levels = c("Depletion", "Fishing mortality", "Recruitment", "Spawning potential")
)
derived_individual <- derived[
  !derived$is_reference & !derived$is_base_fit_reference,
  ,
  drop = FALSE
]
derived_reference <- derived[derived$is_reference %in% TRUE, , drop = FALSE]
derived_summary <- quantile_summary(derived)
derived_plot <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = derived_summary,
    ggplot2::aes(x = .data$year, ymin = .data$lower, ymax = .data$upper, fill = "Central 80% jitter interval"),
    alpha = 0.48, colour = NA
  ) +
  ggplot2::geom_line(
    data = derived_individual,
    ggplot2::aes(x = .data$year, y = .data$value, group = interaction(.data$quantity, .data$run), colour = "Included jitter fit"),
    linewidth = 0.32, alpha = 0.38
  ) +
  ggplot2::geom_line(
    data = derived_summary,
    ggplot2::aes(x = .data$year, y = .data$median, colour = "Jitter median"),
    linewidth = 0.95
  ) +
  ggplot2::geom_line(
    data = derived_reference,
    ggplot2::aes(x = .data$year, y = .data$value, group = .data$quantity, colour = "Diagnostic model (unjittered)"),
    linewidth = 1.05
  ) +
  ggplot2::facet_wrap(~quantity, scales = "free_y", ncol = 2) +
  ggplot2::scale_colour_manual(values = line_colours, breaks = names(line_colours)[c(1, 3, 4)]) +
  ggplot2::scale_fill_manual(values = line_colours["Central 80% jitter interval"]) +
  ggplot2::scale_y_continuous(limits = function(x) c(0, max(x, na.rm = TRUE) * 1.04), expand = ggplot2::expansion(mult = c(0, 0.02))) +
  ggplot2::labs(x = "Year", y = NULL) +
  theme_report(11.5)
save_plot(derived_plot, "jitter-derived-diagnostic-model", 11, 6.2)

# WCPFC BET stock-status trajectories and recent-period endpoint distributions.
status_labels <- c(
  "SB/SBF=0" = "SB/SB[F==0]",
  "SB/SBMSY" = "SB/SB[MSY]",
  "F/FMSY" = "F/F[MSY]"
)
status_levels <- names(status_labels)
stock_status$quantity <- factor(stock_status$quantity, levels = status_levels)
endpoints$quantity <- factor(
  endpoints$quantity,
  levels = c("SBrecent/SBF=0", "SBrecent/SBMSY", "Frecent/FMSY")
)
endpoint_to_status <- c(
  "SBrecent/SBF=0" = "SB/SBF=0",
  "SBrecent/SBMSY" = "SB/SBMSY",
  "Frecent/FMSY" = "F/FMSY"
)
endpoints$status_quantity <- factor(
  unname(endpoint_to_status[as.character(endpoints$quantity)]),
  levels = status_levels
)
status_individual <- stock_status[
  !stock_status$is_reference & !stock_status$is_base_fit_reference,
  ,
  drop = FALSE
]
status_reference <- stock_status[stock_status$is_reference %in% TRUE, , drop = FALSE]
status_summary <- quantile_summary(stock_status)
recent_x <- max(stock_status$year, na.rm = TRUE) + 7
endpoint_jitter <- endpoints[!endpoints$is_reference & !endpoints$is_base_fit_reference, , drop = FALSE]
endpoint_reference <- endpoints[endpoints$is_reference %in% TRUE, , drop = FALSE]
endpoint_jitter$year <- recent_x
endpoint_reference$year <- recent_x
names(endpoint_jitter)[names(endpoint_jitter) == "status_quantity"] <- "status_plot"
names(endpoint_reference)[names(endpoint_reference) == "status_quantity"] <- "status_plot"
endpoint_jitter$quantity <- endpoint_jitter$status_plot
endpoint_reference$quantity <- endpoint_reference$status_plot

reference_lines <- data.frame(
  quantity = factor(status_levels, levels = status_levels),
  y = c(0.2, 1, 1)
)
lrp_label <- data.frame(
  quantity = factor("SB/SBF=0", levels = status_levels),
  year = min(stock_status$year, na.rm = TRUE) + 2,
  value = 0.2,
  label = "LRP"
)

status_plot <- ggplot2::ggplot() +
  ggplot2::geom_hline(
    data = reference_lines,
    ggplot2::aes(yintercept = .data$y),
    colour = "#B64040", linetype = "dashed", linewidth = 0.65
  ) +
  ggplot2::geom_text(
    data = lrp_label,
    ggplot2::aes(x = .data$year, y = .data$value, label = .data$label),
    colour = "#B64040", fontface = "bold", hjust = 0, vjust = -0.35, size = 3.3
  ) +
  ggplot2::geom_ribbon(
    data = status_summary,
    ggplot2::aes(x = .data$year, ymin = .data$lower, ymax = .data$upper, fill = "Central 80% jitter interval"),
    alpha = 0.58, colour = NA
  ) +
  ggplot2::geom_line(
    data = status_individual,
    ggplot2::aes(x = .data$year, y = .data$value, group = interaction(.data$quantity, .data$run), colour = "Included jitter fit"),
    linewidth = 0.30, alpha = 0.34
  ) +
  ggplot2::geom_line(
    data = status_summary,
    ggplot2::aes(x = .data$year, y = .data$median, colour = "Jitter median"),
    linewidth = 0.95
  ) +
  ggplot2::geom_line(
    data = status_reference,
    ggplot2::aes(x = .data$year, y = .data$value, group = .data$quantity, colour = "Diagnostic model (unjittered)"),
    linewidth = 1.05
  ) +
  ggplot2::geom_vline(xintercept = recent_x - 3.0, colour = "#A9BAC2", linetype = "dashed", linewidth = 0.48) +
  ggplot2::geom_boxplot(
    data = endpoint_jitter,
    ggplot2::aes(x = .data$year, y = .data$value, group = .data$quantity),
    width = 2.6, fill = "#91CED7", colour = "#174F63", alpha = 0.78,
    linewidth = 0.62, outlier.shape = NA
  ) +
  ggplot2::geom_point(
    data = endpoint_reference,
    ggplot2::aes(x = .data$year, y = .data$value, colour = "Diagnostic model (unjittered)"),
    shape = 23, fill = "#C62828", size = 3.2, stroke = 0.55
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(quantity), scales = "free_y", switch = "y",
    labeller = ggplot2::labeller(quantity = ggplot2::as_labeller(status_labels, default = ggplot2::label_parsed))
  ) +
  ggplot2::scale_colour_manual(values = line_colours, breaks = names(line_colours)[c(1, 3, 4)]) +
  ggplot2::scale_fill_manual(values = line_colours["Central 80% jitter interval"]) +
  ggplot2::scale_x_continuous(
    breaks = c(seq(1960, 2020, 20), recent_x),
    labels = c(as.character(seq(1960, 2020, 20)), "Recent"),
    expand = ggplot2::expansion(mult = c(0.012, 0.008))
  ) +
  ggplot2::scale_y_continuous(
    limits = function(x) c(0, max(x, na.rm = TRUE) * 1.04),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::labs(x = "Year", y = NULL) +
  theme_report(11.5) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", margin = ggplot2::margin(r = 10)),
    panel.spacing.y = grid::unit(0.65, "lines")
  )
save_plot(status_plot, "jitter-stock-status-diagnostic-model", 11, 7.2)

regional_labels <- c(
  "Regional depletion" = expression(SB/SB[F==0]),
  "Regional recruitment deviation" = "Recruitment deviation"
)
for (regional_quantity in names(regional_labels)) {
  regional_data <- regional[regional$quantity == regional_quantity, , drop = FALSE]
  regional_data$quantity <- regional_quantity
  regional_data$region <- factor(
    regional_data$region,
    levels = c("Region 1", "Region 2", "Region 3", "Region 4", "Region 5", "All regions")
  )
  regional_individual <- regional_data[
    !regional_data$is_reference & !regional_data$is_base_fit_reference,
    ,
    drop = FALSE
  ]
  regional_reference <- regional_data[regional_data$is_reference %in% TRUE, , drop = FALSE]
  regional_summary <- regional_individual |>
    dplyr::filter(is.finite(.data$value)) |>
    dplyr::group_by(.data$region, .data$year) |>
    dplyr::summarise(
      lower = stats::quantile(.data$value, 0.10, na.rm = TRUE, names = FALSE),
      median = stats::median(.data$value, na.rm = TRUE),
      upper = stats::quantile(.data$value, 0.90, na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )
  regional_plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = regional_summary,
      ggplot2::aes(x = .data$year, ymin = .data$lower, ymax = .data$upper, fill = "Central 80% jitter interval"),
      alpha = 0.52, colour = NA
    ) +
    ggplot2::geom_line(
      data = regional_individual,
      ggplot2::aes(x = .data$year, y = .data$value, group = interaction(.data$region, .data$run), colour = "Included jitter fit"),
      linewidth = 0.30, alpha = 0.34
    ) +
    ggplot2::geom_line(
      data = regional_summary,
      ggplot2::aes(x = .data$year, y = .data$median, colour = "Jitter median"),
      linewidth = 0.92
    ) +
    ggplot2::geom_line(
      data = regional_reference,
      ggplot2::aes(x = .data$year, y = .data$value, group = .data$region, colour = "Diagnostic model (unjittered)"),
      linewidth = 1.02
    ) +
    ggplot2::facet_wrap(~region, ncol = 3, scales = if (regional_quantity == "Regional recruitment deviation") "free_y" else "fixed") +
    ggplot2::scale_colour_manual(values = line_colours, breaks = names(line_colours)[c(1, 3, 4)]) +
    ggplot2::scale_fill_manual(values = line_colours["Central 80% jitter interval"]) +
    ggplot2::labs(x = "Year", y = regional_labels[[regional_quantity]]) +
    theme_report(11.2)
  if (regional_quantity == "Regional depletion") {
    regional_plot <- regional_plot +
      ggplot2::scale_y_continuous(limits = c(0, NA), expand = ggplot2::expansion(mult = c(0, 0.035)))
    stem <- "jitter-regional-depletion-diagnostic-model"
  } else {
    regional_plot <- regional_plot +
      ggplot2::geom_hline(yintercept = 0, colour = "#64748B", linewidth = 0.45)
    stem <- "jitter-regional-recruitment_deviation-diagnostic-model"
  }
  save_plot(regional_plot, stem, 11, 7.5)
}

# Public tables: one accepted-fit table with Word and LaTeX copy controls.
accepted_table <- diag_plot_data[order(diag_plot_data$seed), c("seed", "obj_fun", "delta_obj", "max_grad")]
names(accepted_table) <- c("Run", "Objective function value", "Delta objective", "MGC")
utils::write.csv(accepted_table, file.path(table_dir, "jitter-diagnostic-model.csv"), row.names = FALSE)
utils::write.csv(regional, file.path(table_dir, "jitter-regional-timeseries.csv"), row.names = FALSE)
utils::write.csv(stock_status, file.path(table_dir, "jitter-stock-status-timeseries.csv"), row.names = FALSE)
utils::write.csv(endpoints, file.path(table_dir, "jitter-stock-status-quantities.csv"), row.names = FALSE)

format_num <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
format_mgc <- function(x) formatC(x, format = "e", digits = 2)
table_rows_html <- paste0(
  "<tr><td>", accepted_table$Run, "</td><td>", format_num(accepted_table[[2]]),
  "</td><td>", format_num(accepted_table[[3]]), "</td><td>", format_mgc(accepted_table[[4]]),
  "</td></tr>", collapse = "\n"
)
latex_rows <- paste0(
  accepted_table$Run, " & ", format_num(accepted_table[[2]]), " & ",
  format_num(accepted_table[[3]]), " & ", format_mgc(accepted_table[[4]]), " \\\\",
  collapse = "\n"
)
latex_table <- paste0(
  "\\begin{table}[htbp]\n\\centering\n\\caption{MFCL jitter results for the Diagnostic model. Of 30 jitter runs, 25 met the convergence criterion $\\mathrm{MGC} \\leq 1.0 \\times 10^{-4}$. The Diagnostic model without jitter had an objective function value of 90,814.9 and an MGC of $9.68 \\times 10^{-5}$.}\n",
  "\\label{tab:jitter-diagnostic-model}\n\\small\n\\setlength{\\tabcolsep}{7pt}\n\\renewcommand{\\arraystretch}{1.08}\n",
  "\\begin{tabular}{rrrr}\n\\toprule\nRun & Objective function value & $\\Delta$ objective & MGC \\\\\n\\midrule\n",
  latex_rows,
  "\n\\bottomrule\n\\end{tabular}\n\\end{table}\n"
)
writeLines(latex_table, file.path(table_dir, "jitter-diagnostic-model.tex"), useBytes = TRUE)

image_uri <- function(path) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  raw <- readBin(con, what = "raw", n = file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

figure_specs <- list(
  list(
    file = "jitter-diagnostics-diagnostic-model.png",
    caption = paste0(
      "Objective function value and maximum gradient component for the 25 converged jitter fits. ",
      "The red diamond is the Diagnostic model without jitter. Five of the 30 completed jitter runs did not meet ",
      "the convergence criterion MGC ≤ 1.0 × 10⁻⁴ and are not shown."
    )
  ),
  list(
    file = "jitter-derived-diagnostic-model.png",
    caption = paste0(
      "Annual depletion, fishing mortality, recruitment and spawning potential for converged jitter fits. ",
      "Grey-blue lines are individual fits, the shaded band is the pointwise central 80% jitter interval, ",
      "the dark line is the jitter median and the red line is the Diagnostic model without jitter."
    )
  ),
  list(
    file = "jitter-stock-status-diagnostic-model.png",
    caption = paste0(
      "Annual stock-status trajectories and recent-period distributions for the Diagnostic model jitter analysis. ",
      "Recent boxes summarise the 25 converged fits; the red diamonds are the Diagnostic model without jitter. ",
      "SBrecent uses 2021–2024, SBF=0 uses 2014–2023, and Frecent uses 2020–2023. ",
      "The dashed line in the depletion panel marks the limit reference point."
    )
  ),
  list(
    file = "jitter-regional-depletion-diagnostic-model.png",
    caption = paste0(
      "Regional spawning depletion for converged jitter fits. Grey-blue lines are individual fits, ",
      "the shaded band is the pointwise central 80% jitter interval, the dark line is the jitter median, ",
      "and the red line is the Diagnostic model without jitter."
    )
  ),
  list(
    file = "jitter-regional-recruitment_deviation-diagnostic-model.png",
    caption = paste0(
      "Regional recruitment deviations for converged jitter fits. Grey-blue lines are individual fits, ",
      "the shaded band is the pointwise central 80% jitter interval, the dark line is the jitter median, ",
      "and the red line is the Diagnostic model without jitter."
    )
  )
)

figure_html <- paste(vapply(figure_specs, function(spec) {
  path <- file.path(figure_dir, spec$file)
  paste0(
    "<figure class='paper-page'><img src='", image_uri(path), "' alt='", spec$file, "'>",
    "<figcaption><b>Figure XX.</b> ", spec$caption, "</figcaption></figure>"
  )
}, character(1)), collapse = "\n")

word_table_text <- paste(
  c(
    paste(names(accepted_table), collapse = "\t"),
    apply(accepted_table, 1, function(row) paste(row, collapse = "\t"))
  ),
  collapse = "\n"
)

html <- paste0(
  "<!doctype html><html><head><meta charset='utf-8'><meta name='viewport' content='width=device-width,initial-scale=1'>",
  "<title>Diagnostic model jitter</title><style>",
  "body{margin:0;background:#eef3f5;color:#172b3a;font-family:Arial,sans-serif}header{background:#103c56;color:#fff;padding:22px 5vw}",
  "header h1{margin:0;font-family:Georgia,serif;font-size:30px}header p{margin:7px 0 0;color:#d8e8ef}",
  ".tabs{display:flex;gap:8px;padding:14px 5vw;background:#dde8ec;position:sticky;top:0;z-index:5}.tab{border:1px solid #9db4be;background:#f7fbfc;padding:10px 16px;font-weight:700;cursor:pointer}.tab.active{background:#176c80;color:#fff}",
  "main{max-width:1220px;margin:22px auto;padding:0 18px}.panel{display:none}.panel.active{display:block}.card{background:#fff;border:1px solid #cad8de;padding:22px;margin-bottom:22px}",
  ".summary{display:grid;grid-template-columns:repeat(3,1fr);gap:14px}.metric{background:#f2f7f8;border-left:5px solid #176c80;padding:16px}.metric b{font-size:25px;display:block}",
  "figure{margin:0}.paper-page{background:#fff;border:1px solid #cad8de;padding:18px 22px;margin:0 auto 28px;box-sizing:border-box}.paper-page img{width:100%;height:auto;display:block}.paper-page figcaption{font-family:Georgia,serif;font-size:14px;line-height:1.45;margin-top:12px;color:#263238}",
  "table{border-collapse:collapse;width:100%;font-family:Georgia,serif;font-size:14px}th,td{padding:7px 9px;border-bottom:1px solid #d5dfe3;text-align:right}th{background:#e8f1f4}th:first-child,td:first-child{text-align:center}",
  ".buttons{display:flex;gap:10px;margin:0 0 14px}.buttons button{background:#176c80;color:white;border:0;padding:9px 13px;font-weight:700;cursor:pointer}",
  "@media print{body{background:#fff}.tabs{display:none}header{background:#fff;color:#000;padding:0 0 10mm}.panel{display:block}.card{border:0}.paper-page{width:277mm;min-height:190mm;border:0;padding:8mm 10mm;page-break-after:always}.paper-page img{max-height:155mm;object-fit:contain}.paper-page figcaption{font-size:10pt}.summary{display:none}main{max-width:none;margin:0;padding:0}@page{size:A4 landscape;margin:10mm}}",
  "</style></head><body><header><h1>Diagnostic model jitter</h1><p>Job 21641 · 30 completed runs · 25 converged</p></header>",
  "<nav class='tabs'><button class='tab active' data-target='overview'>Overview</button><button class='tab' data-target='figures'>Figures and tables</button></nav><main>",
  "<section id='overview' class='panel active'><div class='summary'><div class='metric'><b>30</b>completed jitter runs</div><div class='metric'><b>25</b>MGC ≤ 1.0 × 10⁻⁴</div><div class='metric'><b>80%</b>pointwise 10th–90th interval</div></div>",
  "<div class='card'><h2>Analysis</h2><p>The report uses the embedded public payload only; no MFCL fit is rerun. The 25 fits meeting the stated MGC criterion are used in all distributions. The unjittered Diagnostic model is shown in red.</p></div></section>",
  "<section id='figures' class='panel'>", figure_html,
  "<div class='card'><div class='buttons'><button onclick=\"copyText('wordData')\">Copy table for Word</button><button onclick=\"copyText('latexData')\">Copy LaTeX</button></div>",
  "<p><b>Table XX.</b> MFCL jitter results for the Diagnostic model. Of 30 jitter runs, 25 met the convergence criterion MGC ≤ 1.0 × 10⁻⁴. The Diagnostic model without jitter had an objective function value of 90,814.9 and an MGC of 9.68 × 10⁻⁵.</p>",
  "<table><thead><tr><th>Run</th><th>Objective function value</th><th>Δ objective</th><th>MGC</th></tr></thead><tbody>", table_rows_html, "</tbody></table>",
  "<textarea id='wordData' hidden>", word_table_text, "</textarea><textarea id='latexData' hidden>", latex_table, "</textarea></div></section></main>",
  "<script>document.querySelectorAll('.tab').forEach(b=>b.onclick=()=>{document.querySelectorAll('.tab,.panel').forEach(x=>x.classList.remove('active'));b.classList.add('active');document.getElementById(b.dataset.target).classList.add('active')});function copyText(id){navigator.clipboard.writeText(document.getElementById(id).value)}</script>",
  "</body></html>"
)
writeLines(html, file.path(output_dir, "jitter-report.html"), useBytes = TRUE)

cat(
  "Rendered public Diagnostic model jitter report with ",
  nrow(diag_plot_data),
  " accepted fits and no MFCL rerun.\n",
  sep = ""
)
