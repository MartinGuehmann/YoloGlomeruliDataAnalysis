#################
# Setup         #
#################

# Fail fast on errors, unless in interactive mode then go to debugging
if (interactive() || Sys.getenv("DEBUG") == "true") {
  options(error = recover)
} else {
  options(error = function() {
    traceback(2)
    quit(status = 1)
  })
}

# Load libraries
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(FSA)
library(stringr)

# Set working directory to script directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  # In RStudio: Path of the active script
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  # Outside RStudio: Path via Rscript arguments
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("--file=", "", file_arg)))
  } else {
    # Interactive or no file given: Current working directory
    script_dir <- getwd()
  }
}

# Set working directory
setwd(script_dir)
cat("Working directory set to:", getwd(), "\n")

# Set output directory
output_dir <- paste0(script_dir, "/output")

####################
# Helper functions #
####################

file_safe_name <- function(x) {
  
  # Replace spaces, @, and other non-alphanumeric characters with underscores
  x <- gsub("[^[:alnum:]]+", "_", x)
  
  # Collapse multiple underscores
  x <- gsub("_+", "_", x)
  
  # Trim leading/trailing underscores
  x <- gsub("^_|_$", "", x)
}

create_parent_dir <- function(path) {

  parent_dir <- dirname(path)
  if (parent_dir != "." && !dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

#############
# Functions #
#############

#' Analyze experimental data with non-parametric tests and annotated plots
#'
#' This function performs a full workflow for experimental metrics, including:
#' 
#' 1. Filtering data to a specified epoch range and removing specified outliers.
#' 2. Computing medians per SuperRank and median-of-medians per experiment.
#' 3. Performing Kruskal-Wallis tests across experiments (groups).
#' 4. Performing Dunn's post-hoc test with Bonferroni correction for multiple comparisons.
#' 5. Optionally performing Mann-Whitney U tests for 2-group subsets, saved in a separate Excel sheet.
#' 6. Calculating effect sizes (r) and significance labels.
#' 7. Generating boxplots with optional median annotations and raw median points.
#' 8. Saving Excel sheets and plots (PDF and SVG) to disk.
#'
#' @param data A data frame containing raw experimental data. Must include columns for `versuch` (experiment name), `Epoche` (epoch number), `SuperRank` (replicate rank), and the metric specified in `metric`.
#' @param experiments A character vector of experiment names (`versuch`) to include in the analysis. This defines the order of experiments in plots and tests.
#' @param symbol_map A named list mapping experiment names to plotting symbols (e.g., letters or codes). Each element name must match an entry in `experiments`. Example: `list("Small"="A", "Medium"="B")`.
#' @param symbol_config A data frame configuring symbol display. Must contain at least the following columns:
#'   \describe{
#'     \item{symbol}{Symbol code, matching values in `symbol_map`.}
#'     \item{shape}{Integer or character representing the shape for ggplot2 `scale_shape_manual`.}
#'     \item{y}{Numeric y-axis position for placing symbols above boxplots.}
#'   }
#'   Each `symbol` in `symbol_map` must have a corresponding row in `symbol_config`.
#' @param metric A string specifying the column name of the metric to analyze.
#' @param metric_name A string used as the y-axis label in plots.
#' @param plot_title Optional string for the plot title (default `"Plot Title"`).
#' @param base_file_name Base file name (without extension) for saved plots and Excel sheets (default `"plot"`).
#' @param mode Optional plotting mode. `"default"` creates plain boxplots, `"annotated"` adds median-of-medians points and labels, `"red_raw_medians"` overlays raw median points in red.
#' @param outlier_filter Optional list specifying outliers to remove, with elements `versuch` and `SuperRank` (default `list(versuch="001", SuperRank=6)`).
#' @param epoch_range Numeric vector of length 2 specifying the start and end epochs to include (default `c(290, 299)`).
#'
#' @return Invisibly returns the ggplot object.  
#'         Saves the following files to disk (prefix given by `base_file_name`):
#'         \itemize{
#'           \item Excel sheets:
#'             \describe{
#'               \item{Kruskal-Wallis}{Contains test statistic, degrees of freedom, and p-value.}
#'               \item{Dunn Test}{Contains pairwise comparisons split into `Group1` and `Group2`, with Z-statistic, raw and adjusted p-values, sample size, effect size r, significance, and effect size strength.}
#'               \item{Mann-Whitney}{Optional sheet created only if exactly 2 groups are analyzed, with `Group1`, `Median1`, `Group2`, `Median2`, W-statistic, effect size r, p-value, significance, and method.}
#'             }
#'           \item Plots: PDF and SVG boxplots with optional annotations as defined by `mode`.
#'         }
#'
#' @details
#' The function is designed to handle multiple experiments (groups) robustly:
#' - Kruskal-Wallis and Dunn's post-hoc tests are always performed across the selected experiments.
#'   - For comparisons with exactly two groups, the Kruskal-Wallis test produces a p-value identical to the Mann-Whitney U test.
#'   - Dunn's test similarly produces the same p-value as Mann-Whitney for a single pair of groups.
#' - For 2-group comparisons, an additional Mann-Whitney U test is performed and stored in a separate sheet for clarity.
#' - For 3 or more groups, only Kruskal-Wallis and Dunn's tests are used for statistical inference.
#' - Effect sizes (r) are computed as Z / sqrt(n), where n is the number of observations.
#' - Plot symbols and annotations are configured via `symbol_map` and `symbol_config`.
#'
#' @examples
#' # Run analysis on a small subset
#' analyze_data(
#'   data = my_data,
#'   experiments = c("Small", "Medium", "Medium+Aug"),
#'   symbol_map = list("Small"="A", "Medium"="B", "Medium+Aug"="C"),
#'   symbol_config = symbol_config_df,
#'   metric = "accuracy",
#'   metric_name = "Accuracy",
#'   plot_title = "YOLO Training Performance"
#' )
#'
#' @seealso \code{\link[stats]{kruskal.test}}, \code{\link[FSA]{dunnTest}}, \code{\link[stats]{wilcox.test}}
#' @import ggplot2
#' @importFrom dplyr group_by summarise mutate
#' @importFrom tidyr stack
#' @importFrom openxlsx write.xlsx
#' @importFrom FSA dunnTest
#' @export
analyze_data <- function(data,
                         experiments,
                         symbol_map,
                         symbol_config,
                         metric,
                         metric_name,
                         plot_title = "Plot Title",
                         base_file_name = "plot", # File name without extension
                         mode = "default",
                         outlier_filter = list(versuch="001", SuperRank=6),
                         epoch_range = c(290, 299)) {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_file_name)

  # Step 1: Filter data to include only the last 10 epochs
  subdata <- subset(data, Epoche >= epoch_range[1] & Epoche <= epoch_range[2])

  # Step 2: Remove outliers (specific versuch and SuperRank)
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(subdata$versuch == outlier_filter$versuch & 
                           subdata$SuperRank == outlier_filter$SuperRank), ]
  }

  # Step 3: Keep only valid versuch levels and factorize
  subdata <- subdata[subdata$versuch %in% experiments, ]
  subdata$versuch <- factor(subdata$versuch , levels=experiments)

  # Step 4: Ensure all experiments have a corresponding symbol mapping
  missing <- setdiff(experiments, names(symbol_map))

  if (length(missing) > 0) {
    stop(paste("Missing symbol_map entries for:", paste(missing, collapse=", ")))
  }

  # Step 5: Create symbol dataset (independent of main data)
  symbol_df <- stack(symbol_map)
  colnames(symbol_df) <- c("symbol", "versuch")
  # Keep only relevant versuch
  symbol_df <- symbol_df[symbol_df$versuch %in% experiments, ]
  # Join with symbol_config (safe: no duplication issue here)
  symbol_df <- merge(symbol_df, symbol_config, by = "symbol", all.x = TRUE)
  # Ensure factor levels match plot
  symbol_df$symbol <- factor(symbol_df$symbol, levels = symbol_config$symbol)
  symbol_df$versuch <- factor(symbol_df$versuch, levels = experiments)
  # Make the legend order more robust
  legend_order_df <- symbol_df[order(symbol_df$versuch), ]
  legend_symbols <- unique(legend_order_df$symbol)

  # Step 6: Safety checks
  missing <- setdiff(symbol_df$symbol, symbol_config$symbol)
  if (length(missing) > 0) {
    stop("Missing symbol_config entries for: ", paste(missing, collapse = ", "))
  }
  if (any(is.na(symbol_df$y))) {
    stop("Some symbols have no y-position defined")
  }

  # Step 7: Compute median per SuperRank for each versuch
  form <- as.formula(paste(metric, "~ versuch + SuperRank"))
  subdata_median <- aggregate(form, data = subdata, median)

  # Step 8: Compute median of medians per versuch
  form <- as.formula(
    paste(metric, "~ versuch")
  )
  subdata_median_median <- aggregate(form, data = subdata_median, median)

  # Step 9: Ensure median datasets have correct factor levels
  subdata_median$versuch <- factor(subdata_median$versuch, levels = experiments)
  subdata_median_median$versuch <- factor(subdata_median_median$versuch, levels = experiments)

  # Step 10a: Kruskal-Wallis test across versuch
  kruskal_result <- kruskal.test(form, data = subdata)
  kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                           parameter = kruskal_result$parameter,
                           p.value = kruskal_result$p.value,
                           method = kruskal_result$method,
                           data.name = kruskal_result$data.name)

  # Step 10b: Optional Mann-Whitney U test for exactly 2 groups
  group_count <- length(unique(subdata$versuch))
  if(group_count == 2) {

    groups <- levels(subdata$versuch)
    x <- subdata[[metric]][subdata$versuch == groups[1]]
    y <- subdata[[metric]][subdata$versuch == groups[2]]

    mw_result <- wilcox.test(x, y, exact = FALSE)

    # Effect size r = Z / sqrt(N)
    # Calculate Z approximation for Wilcoxon
    W <- as.numeric(mw_result$statistic)
    n <- length(x) + length(y)
    # approximate Z using normal approximation
    # Note: wilcox.test does not return Z, so we compute it:
    U <- W
    mu_U <- length(x)*length(y)/2
    sigma_U <- sqrt(length(x)*length(y)*(length(x)+length(y)+1)/12)
    Z <- (U - mu_U)/sigma_U
    r <- Z / sqrt(n)

    # Create data frame with separate group columns
    mann_whitney_df <- data.frame(
      Group1 = groups[1],
      Median1 = median(x),
      Group2 = groups[2],
      Median2 = median(y),
      W = W,
      r = r,
      p.value = mw_result$p.value,
      significant = ifelse(mw_result$p.value < 0.05, "Yes", "No"),
      method = mw_result$method
    )

  } else {
    mann_whitney_df <- NULL
  }

  # Step 11: Dunn's pairwise post-hoc test with Bonferroni correction
  dunn_result <- dunnTest(form, data=subdata, method="bonferroni")

  # Step 12: Calculate effect size r and add significance
  # n is the number of observation, one observation from each epoch, per experiment per repetitions
  n <- nrow(model.frame(form, data = subdata))
  dunn_result$res$r <- dunn_result$res$Z / sqrt(n)
  alpha <- 0.05
  dunn_result$res$significant <- ifelse(dunn_result$res$P.adj < alpha, "Yes", "No")

  # Step 13: Add strength of effect size
  dunn_result$res <- dunn_result$res %>%
    mutate(effect_size_strength = case_when(
      abs(r) < 0.1 ~ "nelectable",
      abs(r) < 0.3 ~ "small",
      abs(r) < 0.5 ~ "medium",
      TRUE         ~ "big"
    ))

  # Step 14: Add sample size column, reorder columns, and split Comparison into Group1/Group2
  dunn_result$res$n <- n
  # Split 'Comparison' into two separate columns
  dunn_result$res <- dunn_result$res %>%
    tidyr::separate(Comparison, into = c("Group1", "Group2"), sep = " - ") %>%
    # Reorder columns for clarity
    select(Group1, Group2, Z, P.unadj, P.adj, n, r, significant, effect_size_strength)

  # Step 15: Export Kruskal-Wallis, Dunn, and optional Mann-Whitney results to excel
  data_to_write <- list("Kruskal-Wallis" = kruskal_df, "Dunn Test" = dunn_result$res)
  if(!is.null(mann_whitney_df)) {
    data_to_write[["Mann-Whitney"]] <- mann_whitney_df
  }
  write_xlsx(data_to_write, paste0(base_file_name, ".xlsx"))

  # Step 16: Create Plot
  p <- ggplot(subdata_median, aes(x = versuch, y = .data[[metric]])) + 
    geom_boxplot(outlier.colour = "black", outlier.size = 0.2, width = 0.6) +
    geom_point(
      data = symbol_df,
      aes(x = versuch, y = y, shape = symbol),
      size = 2.2
    ) +
    scale_shape_manual(
      values = setNames(symbol_config$shape, symbol_config$symbol),
      breaks = legend_symbols
    ) +
    labs(
      shape = "",
      x     = "Experiment",
      y     = metric_name
    ) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(
      limits       = c(0, 1.25),
      expand       = c(0, 0),
      breaks       = seq(0, 1, 0.1),
      minor_breaks = seq(0, 1, 0.01)
    ) +
    ggtitle(plot_title) +
    theme(
      plot.title = element_text(color = "black", size = 9),
      axis.text.x = element_text(size = 6),
      panel.grid.major.y = element_line(colour = "grey60", size = 0.2),
      panel.grid.minor.y = element_blank()
    )

  # Step 17: Add optional annotations
  if (isTRUE(mode != "default")) {

    max_df <- subdata_median %>%
      dplyr::group_by(versuch) %>%
      dplyr::summarise(y_max = max(.data[[metric]], na.rm = TRUE))

    label_offset_factor <- 0.08

    offset <- label_offset_factor * diff(range(subdata_median[[metric]], na.rm = TRUE))
    max_df$y_label <- max_df$y_max + offset

    max_df$median_value <- subdata_median_median[[metric]][
      match(max_df$versuch, subdata_median_median$versuch)
    ]

      if (mode == "annotated") {
      p <- p +
        # Blue median-of-medians points
        stat_summary(
          fun = median,
          geom = "point",
          color = "blue",
          size = 2.5
        ) +
        # Blue median labels
        geom_text(
          data = max_df,
          aes(x = versuch, y = y_label, label = round(median_value, 3)),
          color = "blue",
          size = 3
        )
    }
    if (mode == "annotated" || mode == "red_raw_medians" ) {
      p <- p +
        # Red raw medians
        geom_point(
          data = subdata_median,
          aes(x = versuch, y = .data[[metric]]),
          color = "red",
          size = 0.5
        )
    }
  }

  # Step 18: Save plot as PDF and SVG
  ggsave(
    filename = paste0(base_file_name, ".pdf"),
    plot = p,
    height = 5,
    width = 5
  )
  ggsave(
    filename = paste0(base_file_name, ".svg"),
    plot = p,
    height = 5,
    width = 5
  )
}

run_linear_model <- function(data,
                             experiments,
                             metric,
                             metric_name,
                             base_file_name,
                             outlier_filter = list(versuch="001", SuperRank=6),
                             epoch_range = c(290, 299)) {

  # ----------------------------
  # Step 0: Create output folder
  #         if it does not exist
  # ----------------------------
  create_parent_dir(base_file_name)

  # ----------------------------
  # Step 1: Filter epochs
  # ----------------------------
  subdata <- subset(data, Epoche >= epoch_range[1] & Epoche <= epoch_range[2])

  # ----------------------------
  # Step 2: Remove outliers
  # ----------------------------
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(
      subdata$versuch == outlier_filter$versuch &
        subdata$SuperRank == outlier_filter$SuperRank
    ), ]
  }

  # ----------------------------
  # Step 3: Keep valid versuch levels
  # ----------------------------
  subdata$versuch <- factor(subdata$versuch, levels = experiments)

  # ----------------------------
  # Step 4: Sanity checks
  # ----------------------------
  if (nrow(subdata) == 0) stop("subdata is empty after filtering")

  if (!metric %in% colnames(subdata)) {
    stop(paste("Metric column not found:", metric))
  }

  # Check for NA in key columns
  if (any(is.na(subdata[[metric]]))) {
    stop("NA values found in metric column after filtering")
  }

  # ----------------------------
  # Step 5: Aggregation
  # ----------------------------
  form_agg <- as.formula(
    paste(metric, "~ TrainTiny + TinyAug + Syn + SynAug + version + Versuch")
  )

  result <- aggregate(form_agg, data = subdata, FUN = median)

  # Check aggregation integrity
  expected_groups <- length(unique(subdata$versuch))
  actual_groups <- nrow(result)

  if (actual_groups == 0) stop("Aggregation produced empty result")

  # ----------------------------
  # Step 6: Model matrices (consistency check)
  # ----------------------------
  lm_formula  <- as.formula(paste(metric, "~ TrainTiny + TinyAug + Syn + SynAug"))
  lmi_formula <- as.formula(paste(metric, "~ TrainTiny * TinyAug * Syn * SynAug"))

  # Build model matrices explicitly (important check)
  X_lm  <- model.matrix(lm_formula, data = result)
  X_lmi <- model.matrix(lmi_formula, data = result)

  if (any(is.na(X_lm)) || any(is.na(X_lmi))) {
    stop("NA values detected in model matrices")
  }

  # ----------------------------
  # Step 7: Fit models
  # ----------------------------
  lm_model  <- lm(lm_formula, data = result)
  lmi_model <- lm(lmi_formula, data = result)

  # ----------------------------
  # Step 8: Coefficients
  # ----------------------------
  coefs <- summary(lmi_model)$coefficients

  coef_data <- data.frame(
    `Training data set combinations` = rownames(coefs),
    Coefficients  = coefs[, 1],
    `Std. Error`  = coefs[, 2],
    `t-value`     = coefs[, 3],
    "Pr(>|t|)"    = coefs[, 4],
    check.names   = FALSE
  )

  coef_data$TermType <- dplyr::case_when(
    coef_data$`Training data set combinations` == "(Intercept)" ~ "Intercept",
    !grepl(":", coef_data$`Training data set combinations`) ~ "Main Effect",
    TRUE ~ paste0(
      stringr::str_count(coef_data$`Training data set combinations`, ":") + 1,
      "-way Interaction"
    )
  )
  coef_data$TermType <- factor(
    coef_data$TermType,
    levels = c(
      "Intercept",
      "Main Effect",
      "2-way Interaction",
      "3-way Interaction",
      "4-way Interaction"
    )
  )
  # Keep model order, but group by type
  coef_data$`Training data set combinations` <- factor(
    coef_data$`Training data set combinations`,
    levels = coef_data$`Training data set combinations`
  )
  coef_data$Group <- ifelse(coef_data$TermType == "Interaction", 2,
                            ifelse(coef_data$TermType == "Main Effect", 1, 0))

  coef_data <- coef_data[order(coef_data$TermType), ]

  # ----------------------------
  # Step 9: File naming
  # ----------------------------
  xlsx_name <- paste0(base_file_name, metric, "_results.xlsx")
  pdf_name  <- paste0(base_file_name, metric, "_coefficents.pdf")
  svg_name  <- paste0(base_file_name, metric, "_coefficents.svg")

  # ----------------------------
  # Step 10: Output consistency check
  # ----------------------------
  if (!is.finite(AIC(lm_model)) || !is.finite(AIC(lmi_model))) {
    stop("AIC computation returned non-finite values")
  }

  # ----------------------------
  # Step 11: Save Excel
  # ----------------------------
  write_xlsx(
    list("linear_model_results" = cbind(
      coef_data,
      AIC_lm1 = AIC(lm_model),
      AIC_lmi = AIC(lmi_model)
    )),
    path = xlsx_name
  )

  # ----------------------------
  # Step 12: Plot
  # ----------------------------
  p <- ggplot(coef_data, aes(
    x = `Training data set combinations`,
    y = Coefficients,
    fill = TermType
  )) +
    geom_bar(stat = "identity") +
    geom_errorbar(
      aes(
        ymin = Coefficients - `Std. Error`,
        ymax = Coefficients + `Std. Error`
      ),
      width = 0.2
    ) +
    scale_fill_manual(values = c(
      "Intercept" = "gray70",
      "Main Effect" = "steelblue",
      "2-way Interaction" = "#fdae61",
      "3-way Interaction" = "#f46d43",
      "4-way Interaction" = "#d73027"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 9)
    ) +
    ggtitle(title) +
    labs(
      x     = "Training dataset combinations",
      y     = "Coefficients"
    ) +
    ggtitle(paste("Coefficients of the linear interaction model:", metric_name))

  ggsave(
    filename = pdf_name,
    plot = p,
    height = 5,
    width = 5
  )
  ggsave(
    filename = svg_name,
    plot = p,
    height = 5,
    width = 5
  )

  pdf_name_facet <- paste0(base_file_name, metric, "_coefficents_FACET.pdf")

  p_facet <- ggplot(coef_data, aes(
    x = `Training data set combinations`,
    y = Coefficients,
    fill = TermType
  )) +
    geom_bar(stat = "identity") +
    geom_errorbar(
      aes(
        ymin = Coefficients - `Std. Error`,
        ymax = Coefficients + `Std. Error`
      ),
      width = 0.2
    ) +
    facet_wrap(~ TermType, scales = "free_x") +
    scale_fill_manual(values = c(
      "Intercept" = "gray70",
      "Main Effect" = "steelblue",
      "2-way Interaction" = "#fdae61",
      "3-way Interaction" = "#f46d43",
      "4-way Interaction" = "#d73027"
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 9),
      legend.position = "none"
    ) +
    labs(
      x     = "Training dataset combinations",
      y     = "Coefficients"
    ) +
    ggtitle(paste("Faceted coefficients of the linear interaction model:", metric_name))

  ggsave(
    filename = pdf_name_facet,
    plot = p_facet,
    height = 6,
    width = 8
  )

  # ----------------------------
  # Step 13: Return diagnostics
  # ----------------------------
  invisible(list(
    lm = lm_model,
    lmi = lmi_model,
    coef = coef_data,
    n_rows_subdata = nrow(subdata),
    n_rows_result = nrow(result)
  ))
}

#' Plot Training Time vs. Number of Images
#'
#' @description
#' Creates a scatter plot of training time (in hours) versus the
#' number of training images, including a linear regression line
#' and its formula. The plot is saved as both PDF and SVG files.
#'
#' @param traing_times Numeric vector.
#'   Training durations in hours (y-axis values).
#'
#' @param image_numbers Numeric vector.
#'   Number of training images (x-axis values).
#'
#' @param output_dir Character string.
#'   Directory where the output files will be saved.
#'   The directory will be created if it does not exist.
#'
#' @return
#' No return value. The function is called for its side effect of
#' saving plot files to disk.
#'
#' @details
#' \itemize{
#'   \item Points are plotted as unfilled red circles.
#'   \item A linear regression model (\code{lm}) is fitted and drawn as a line.
#'   \item The regression formula is displayed inside the plot area,
#'         slightly below the regression line.
#'   \item Axis ticks are fixed:
#'     \itemize{
#'       \item X-axis: every 10,000 images
#'       \item Y-axis: every 20 hours
#'     }
#'   \item Plot size is 5x5 inches.
#'   \item The regression line is extended to reach the plot boundaries
#'         while avoiding clipping of data points.
#' }
#'
#' @examples
#' # Example usage:
#' traing_times <- c(10, 20, 35, 50)
#' image_numbers <- c(5000, 10000, 15000, 20000)
#' plot_training_times(traing_times, image_numbers, tempdir())
#'
#' @import ggplot2
#' @export
plot_training_times <- function(traing_times, image_numbers, output_dir)
{
  # Build output file names
  output_file_name_pdf <- paste0(output_dir, "/NumberOfImages_TraingTime.pdf")
  output_file_name_svg <- paste0(output_dir, "/NumberOfImages_TraingTime.svg")

  # Ensure the parent directory exists
  create_parent_dir(output_file_name_pdf)

  # Create a data frame for ggplot
  df <- data.frame(
    Images = image_numbers,
    Time_h = traing_times
  )

  # Fit linear model for regression line
  fit <- lm(Time_h ~ Images, data = df)
  formula_text <- paste0("f(x) = ", round(coef(fit)[2], 3), " x + ", round(coef(fit)[1], 3))

  # Compute dynamic text position: just below regression line
  pred_vals <- predict(fit, newdata = data.frame(Images = df$Images))
  # X position: 35% along the x-axis
  x_pos <- min(df$Images) + 0.35 * (max(df$Images) - min(df$Images))

  # Y position: just below regression line at x_pos
  y_line_at_x <- coef(fit)[1] + coef(fit)[2] * x_pos
  y_pos <- y_line_at_x - 0.05 * (max(pred_vals) - min(pred_vals))  # 5% below the line

  # Compute regression line points extended into padding
  x_range <- max(df$Images) - min(df$Images)
  x_line <- c(
    min(df$Images) - 0.05 * x_range,
    max(df$Images) + 0.05 * x_range
  )
  y_line <- coef(fit)[1] + coef(fit)[2] * x_line
  line_df <- data.frame(Images = x_line, Time_h = y_line)

  # Compute training time per 10,000 images
  time_per_10000 <- coef(fit)[2] * 10000
  efficiency_text <- paste0("ca. ", round(time_per_10000, 2), " h / 10,000 images")

  # Dynamic Y position for efficiency annotation: slightly below regression formula
  y_eff_pos <- y_pos - 0.08 * (max(pred_vals) - min(pred_vals))  # 8% below formula

  # Create ggplot
  p <- ggplot(df, aes(x = Images, y = Time_h)) +
    geom_point(shape = 1, color = "red", size = 2) +                                           # Unfilled circles
    geom_line(data = line_df, aes(x = Images, y = Time_h), color = "black", linewidth = 0.5) + # Regression line
    annotate(
      "text",
      x = x_pos,
      y = y_pos,
      label = formula_text,
      hjust = 0,
      size = 3
    ) +
    annotate(
      "text",
      x = x_pos,
      y = y_eff_pos,
      label = efficiency_text,
      hjust = 0,
      size = 3,
      color = "black"
    ) +
    labs(
      x = "Number of training images",
      y = "Training time (h)",
      title = "Dependency: Training time vs training data set size"
    ) +
    scale_x_continuous(
      breaks = seq(0, max(df$Images) + 10000, by = 10000),   # X ticks every 10000
      expand = c(0, 0)                                       # <-- remove padding
    ) +
    scale_y_continuous(
      breaks = seq(0, max(df$Time_h) + 20, by = 20),         # Regression line
      expand = c(0, 0)                                       # <-- remove padding
    ) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 9)
    )

  # Save plot as PDF and SVG
  ggsave(filename = output_file_name_pdf, plot = p, height = 5, width = 5, units = "in") # Set the "default" units explicitly. Quite weired that inchi-binchies are the default.
  ggsave(filename = output_file_name_svg, plot = p, height = 5, width = 5, units = "in")
}

plot_experiment <- function(data,
                            experiment_id, 
                            metric,
                            metric_name,
                            title,
                            filename,
                            outlier_filter = list(versuch="001", SuperRank=6)) {

  # Filter data
  subdata <- subset(data, versuch == experiment_id)
  
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(subdata$versuch == outlier_filter$versuch & 
                           subdata$SuperRank == outlier_filter$SuperRank), ]
  }

  # Create plot
  p <- ggplot(subdata) +
    geom_point(aes(x = Epoche, y = .data[[metric]]), size = 0.1) +
    facet_wrap(~ SuperRank, nrow = 3) +
    ggtitle(title) +
    labs(
      x     = "Epoch",
      y     = metric_name
    ) +
    theme(plot.title = element_text(color = "black", size = 9))

  # Create parent directory if that does not exist
  create_parent_dir(filename)
  # Save to PDF
  pdf(filename, height = 5, width = 5)
  print(p)
  dev.off()
}

plot_histogram_normality <- function(
    data,
    metric,
    metric_name,
    base_filename,
    versuche,
    epoch_range = c(290, 299),
    axis_text_size = 5
) {

  create_parent_dir(base_filename)
  # Filter data
  subdata_all <- subset(
    data,
    Epoche >= epoch_range[1] &
      Epoche <= epoch_range[2] &
      versuch %in% versuche
  )

  # ---------------------------
  # Shapiro-Wilk test per versuch
  # ---------------------------
  shapiro_df <- do.call(
    rbind,
    lapply(split(subdata_all[[metric]], subdata_all$versuch), function(x) {

      # Shapiro requires at least 3 values
      if (length(x) < 3) {
        return(data.frame(W = NA, p_value = NA))
      }

      test <- shapiro.test(x)

      data.frame(
        W = as.numeric(test$statistic),
        p_value = test$p.value
      )
    })
  )

  shapiro_df$versuch <- rownames(shapiro_df)
  rownames(shapiro_df) <- NULL

  # Add significance column
  alpha <- 0.05
  shapiro_df$normal <- ifelse(shapiro_df$p_value > alpha, "Yes", "No")

  # ---------------------------
  # Write Excel file
  # ---------------------------
  write_xlsx(
    list("Shapiro-Wilk" = shapiro_df),
    path = paste0(base_filename, "_Shapiro_Wilk_", metric, ".xlsx")
  )

  # ---------------------------
  # Plot
  # ---------------------------
  p <- ggplot(subdata_all, aes(x = .data[[metric]])) +
    geom_histogram(binwidth = 0.01) +
    facet_wrap(~versuch) +
    ggtitle(
      paste0(
        "Histogram of ", metric_name,
        "-values for all experiments of the last 10 Epochs"
      )
    ) +
    labs(
      x     = metric_name,
      y     = "Count"
    ) +
    theme(
      plot.title = element_text(color = "black", size = 9),
      axis.text.x = element_text(size = axis_text_size)
    )

  # Plot
  pdf(paste0(base_filename, "_histogram_", metric, ".pdf"), height = 5, width = 5)
  print(p)
  dev.off()

  # Return results for further use
  return(shapiro_df)
}

#########################################################################################################################

#############################################
# Data loading, preparation, and definition #
#############################################

#########################################################################################################################

# ------------------------------------------------------------
# Filename / Folder Parsing Reference (current rules)
#
# Rule:
# 1. Extract experiment ID ('versuch') from the parent folder name of the file e.g. results.txt
#    → capture the first 3 digits in the folder name
#    → converted to numeric as 'Versuch'
# 2. Extract version from the remaining digits in the folder name
#    → if no remaining digits, default to 1
# 3. Epoch extraction from the Epoch column is separate
#
# Examples:
# Folder / Filename                     | versuch | Versuch | version | Notes
# --------------------------------------|---------|---------|---------|-----------------------------------------------
# "runs/train/yolov7-00110/results.txt" | "001"   | 1       | 10      | experiment: first 3 digits, version: remaining digits
# "runs/train/yolov7-0049/results.txt"  | "004"   | 4       | 9       | 3-digit experiment + 1-digit version
# "runs/train/yolov7-005/results.txt"   | "005"   | 5       | 1       | Version missing → defaults to 1
# "runs/train/yolov7-00501/results.txt" | "005"   | 5       | 1       | Explicit "01" → same as default
# "runs/train/yolov7-00110/shshsgd"     | "001"   | 1       | 10      | filename can be anything; only folder matters
#
# Notes:
# - Only the **parent folder** of the file is used for parsing; full path or filename does not matter
# - Regex: first 3 digits = experiment, remaining digits = version
# - Version digits default to 1 if missing
# - Works regardless of directory depth, folder prefix, or arbitrary filename
# - Changing the folder naming convention may require updating the regex
# ------------------------------------------------------------

# Load data
Design <- read_excel("Design2.xlsx")

data1 <- read.csv("allresults_header_tab_final_v002.txt", sep = "\t")

# Extract variables from filename + Epoch
data1 <- data1 %>%
  mutate(
    # Extract the folder containing the results.txt file
    folder = basename(dirname(filename)),                 # For instance yolov7-00110"

    # Regex capture: first 3 digits = experiment, remaining digits = version
    matches = str_match(folder, "(\\d{3})(\\d*)"),       # First 3 digits = experiment, remaining digits = version

    # Experiment ID
    versuch = matches[,2],                               # First 3 digits as string
    Versuch = as.numeric(versuch),                       # Numeric conversion

    # Version number
    version = matches[,3],                               # Remaining digits after first 3
    version = ifelse(version == "", "1", version),       # Default to 1 if missing
    version = as.integer(version),                       # Numeric conversion

    # Epoch parsing
    Epoche = str_split(Epoch, "/") %>% 
      sapply(function(x) x[1]) %>% 
      as.integer()
  ) %>%
  select(-matches, -folder)                              # Remove temporary helper columns used for parsing
# Examples:
# "yolov7-00110" → versuch=1, version=10
# "yolov7-0049"  → versuch=4, version=9
# "005"          → versuch=5, version=1 (default)
# "yolo8-00501"  → versuch=5, version=1 (explicit 01, same as default)

# Merge with design
data1 <- data1 %>%
  inner_join(Design, by = "Versuch")

# Compute max epoch per (versuch, version)
data1df <- data1 %>%
  group_by(versuch, version) %>%
  mutate(MaxValue = max(Epoche, na.rm = TRUE)) %>%
  ungroup() %>%

  # Filter long enough runs
  filter(MaxValue > 290) %>%

  # Rank versions within experiment
  group_by(versuch) %>%
  mutate(SuperRank = dense_rank(version)) %>%
  ungroup()

# Quick sanity check
count(data1df, versuch)
summary(data1df$Epoche)

#########################################################################################################################

# Mapping of versuch to symbols
symbol_map_experiments <- list(
  "001" = c("TrainSmall"),
  "003" = c("TrainBig"),
  "004" = c("Glo"),
  "012" = c("Glo", "GloAug"),
  "006" = c("TrainTiny"),
  "014" = c("TinyAug"),
  "005" = c("Syn"),
  "015" = c("SynAug"),
  "007" = c("TrainTiny","TinyAug"),
  "016" = c("TinyAug","Syn"),
  "009" = c("Syn","SynAug"),
  "008" = c("TrainTiny","Syn"),
  "017" = c("TinyAug","SynAug"),
  "018" = c("TrainTiny","SynAug"),
  "013" = c("TrainTiny","TinyAug","Syn"),
  "019" = c("TinyAug","Syn","SynAug"),
  "010" = c("TrainTiny","Syn","SynAug"),
  "020" = c("TrainTiny","TinyAug","SynAug"),
  "011" = c("TrainTiny","TinyAug","Syn","SynAug")
)

# Define y-positions for the symbols and their shapes
symbol_config <- data.frame(
  symbol = c("TrainSmall", "TrainBig", "Glo", "GloAug", "TrainTiny", "TinyAug", "Syn", "SynAug"),
  y      = c(1.20, 1.15,   1.10,       1.05, 1.20,     1.15, 1.10,     1.05),
  shape  = c(4   , 3   ,   1   ,       2   , 8   ,     7   , 6   ,     5   ),
  stringsAsFactors = FALSE
)

# Define the metrics
metrics <- c(
  mAP_50    = "mAP@50",
  mAP_95    = "mAP@50-95",
  precision = "Precision",
  recall    = "Recall"
)

# Define the plot types
# Use Unix directory separators, Windows has no problem with that
plot_types <- c(
  default         = "",
  annotated       = "Annotated/",
  red_raw_medians = "RedRawMedians/"
)

# Define the jobs for the box plots
jobs <- list(
  list(
    name        = "all",
    title       = "All 19 experiments: ",
    experiments = c("001", "003", "004", "012", "006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010", "020", "011")
  ),
  list(
    name        = "augmented",
    title       = "Original and classical augmented images: ",
    experiments = c("001", "003", "004", "012")
  ),
  list(
    name        = "size",
    title       = "Training data set sizes 2: ",
    experiments = c("006", "001","003")
  ),
  list(
    name        = "combinations",
    title       = "All combinations of augmentations: ",
    experiments = c("006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")
  ),
  list(
    name        = "small_augmented",
    title       = "Augmentations of small datasets: ",
    experiments = c("006", "014", "007")
  ),
  list(
    name        = "small_syn_augmented",
    title       = "Synthetic augmentations of small datasets: ",
    experiments = c("006", "005", "008")
  ),
  list(
    name        = "small_combinations",
    title       = "Classical and synthetic augmentations of small datasets: ",
    experiments = c("006", "007", "008", "013", "011")
  ),
  list(
    name        = "size_to_lower",
    title       = "Training data set sizes: ",
    experiments = c("003", "001","006")
  ),
  list(
    name        = "non_annotated_removed",
    title       = "Removing images without glomeruli: ",
    experiments = c("003", "004")
  ),
  list(
    name        = "add_augmented1",
    title       = "Conventional data augmentation 1: ",
    experiments = c("004", "012")
  ),
  list(
    name        = "add_augmented2",
    title       = "Conventional data augmentation 2: ",
    experiments = c("006", "014", "005", "015")
  ),
  list(
    name        = "add_augmented3",
    title       = "Conventional data augmentation 3: ",
    experiments = c("006", "014", "005", "015", "004", "012")
  ),
  list(
    name        = "combinations_only",
    title       = "Only combinations of augmentations: ",
    experiments = c("007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")
  )
)

# Make vectors for training time and number of images, hard encoded, come from outside.
# Would be better to have it in its own file.
traing_times  <- c(9.544, 113.401, 21.871, 4.854, 3.099, 2.856, 4.936, 11.607, 11.489, 12.316, 100.733, 5.162, 2.808, 9.625, 5.067, 9.924, 9.77, 11.583, 9.911)
image_numbers <- c(3410, 53908, 8855, 800, 15, 75, 815, 4000, 4015, 4075, 44275, 875, 60, 3200, 860, 3260, 3215, 4060, 3275)

#########################################################################################################################

##############################
# Data analysis and plotting #
##############################

#########################################################################################################################

# Check for normal distribution by histograms and Shapiro-Wilk test

experiments <- c(
  "001","003","004","005","006","007","008","009","010",
  "011","012","013","014","015","016","017","018","019","020"
)

for (metric in names(metrics)) {
  metric_name <- metrics[[metric]]
  plot_histogram_normality(
    data = data1df,
    metric = metric,
    metric_name = metric_name,
    base_filename = paste0(output_dir, "/Histograms/AllExperiments"),
    versuche = experiments
  )
}

#########################################################################################################################

# Plot the relation of training time and number of training images
plot_training_times(traing_times, image_numbers, output_dir)

#########################################################################################################################

for (metric in names(metrics)) {
  for(plot_type in names(plot_types)) {
      for(job in jobs) {
        metric_name     <- metrics[[metric]]
        plot_title      <- paste0(job$title, metrics[[metric]], " of the last 10 epochs")
        base_file_name  <- paste0(output_dir, "/", metric, "/", plot_types[[plot_type]], file_safe_name(plot_title))

        if(metric == "mAP_50" && job$name == "all") {
          analyze_data(
            data1df,
            job$experiments,
            symbol_map_experiments,
            symbol_config,
            metric,
            metric_name,
            plot_title,
            base_file_name,
            plot_type,
            NULL
          )
        }
        else {
          analyze_data(
            data1df,
            job$experiments,
            symbol_map_experiments,
            symbol_config,
            metric,
            metric_name,
            plot_title,
            base_file_name,
            plot_type
          )
        }
    }
  }
}

#########################################################################################################################

# Multiple linear regression for the models for all metrics to determine what has the most effect
experiments <- c("006", "014", "005",
                 "015", "007", "016", "009", "008",
                 "017", "018", "013", "019", "010",
                 "020", "011")

base_file_name <- paste0(output_dir, "/LinearModels/LinearModel_")

for (metric in names(metrics)) {
  run_linear_model(data1df, experiments, metric, metrics[[metric]], base_file_name)
}

#########################################################################################################################

# Plot all the values for all epochs for all metrics
for (experiment_id in names(symbol_map_experiments)) {

  for (metric in names(metrics)) {

    metric_name <- metrics[[metric]]
    plot_experiment(
      data = data1df,
      experiment_id = experiment_id,
      metric = metric,
      metric_name = metric_name,
      title = paste0("Experiment ", experiment_id, ": ", metric_name),
      filename = paste0(output_dir, "/", metric, "/MetricCurves/", "Experiment_", experiment_id, "_", metric, ".pdf"),
    )

  }
}

#########################################################################################################################
