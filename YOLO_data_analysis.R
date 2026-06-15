############################################################
# Title: Glomeruli detection in my tissue sections
#
# Description:
# <Short high-level description of what this script does,
# e.g. "This script performs preprocessing, statistical
# modeling, and visualization of experimental results.">
#
# Author: <August Pfeiffer, Martin Gühmann>
# Date: <2026-MM-DD>
#
# Purpose:
# <Explain the goal of the script in 2–4 sentences.
# Example: This script analyzes experimental runs across
# multiple conditions, fits linear and interaction models,
# and generates summary plots and statistical outputs.>
#
# Input:
# - allresults_header_tab_final_v002.txt
# - Design2.xlsx
# - data: <source of data, file format, expected structure>
# - required columns: <list key columns briefly>
#
# Output:
# - <list generated files: plots, tables, models, etc.>
#
# Workflow:
# 1. Load and prepare data
# 2. Filter / clean dataset
# 3. Run statistical analysis / models
# 4. Generate plots
# 5. Save outputs
#
# Notes:
# - <any assumptions, e.g. epoch ranges, outlier handling>
# - <important parameter choices like alpha = 0.05>
# - <known limitations if any>
#
############################################################

#################
# Setup         #
#################

# options(warn = 2)  # Treat warnings as errors

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
library(openxlsx)
library(reshape2)
library(patchwork)
library(ggpubr)

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

#' Convert a string to a file-safe name (without extension)
#'
#' This helper function takes a character string and converts it into a file-system-safe
#' name by replacing spaces and non-alphanumeric characters with underscores. Multiple
#' consecutive underscores are collapsed into one, and leading/trailing underscores are removed.
#' 
#' Note: This function does **not** preserve file extensions. Extensions should be added
#' after sanitizing the main name.
#'
#' @param x A character string to be sanitized for use as a file name.
#'
#' @return A character string safe to use as a file name (excluding any file extension).
#'
#' @examples
#' # Convert a regular string to a file-safe version
#' file_safe_name("My File 2026")
#' # Returns: "My_File_2026"
#'
#' # Add extension afterwards
#' paste0(file_safe_name("My File 2026"), ".xlsx")
#' # Returns: "My_File_2026.xlsx"
file_safe_name <- function(x) {
  
  # Replace spaces, @, and other non-alphanumeric characters with underscores
  x <- gsub("[^[:alnum:]]+", "_", x)
  
  # Collapse multiple underscores
  x <- gsub("_+", "_", x)
  
  # Trim leading/trailing underscores
  x <- gsub("^_|_$", "", x)
}

#' Ensure parent directory exists for a given file path
#'
#' This helper function checks whether the parent directory of a specified file path exists.
#' If it does not exist, the function creates the parent directory (including any missing
#' intermediate directories). No warnings are shown if the directory already exists.
#'
#' @param path A string specifying the file path for which the parent directory should exist.
#'
#' @return Invisibly returns `NULL`. The main effect is that the parent directory is created
#'   if it does not already exist.
#'
#' @examples
#' # Ensure the parent directory exists before saving a plot
#' create_parent_dir("results/plots/my_plot.pdf")
#'
#' # Works for nested directories as well
#' create_parent_dir("data/output/2026/summary.xlsx")
create_parent_dir <- function(path) {

  parent_dir <- dirname(path)
  if (parent_dir != "." && !dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

#' Filter experimental data by experiment, epoch range, and optional outliers
#'
#' This helper function subsets a data frame of experimental metrics to include only the
#' specified experiments and epochs. It can also remove a specified outlier. The `versuch`
#' column is converted to a factor with levels matching the order of `experiments`.
#'
#' @param data A data frame containing experimental data. Must include at least the columns
#'   `versuch` (experiment name), `Epoche` (epoch number), and `SuperRank` (replicate rank).
#' @param experiments Character vector of experiment names (`versuch`) to include in the subset.
#'   Rows with other experiments are discarded.
#' @param epoch_range Numeric vector of length 2 specifying the start and end epochs to include.
#' @param outlier_filter Optional list specifying a single outlier to remove. Should have elements
#'   `versuch` and `SuperRank`. If `NULL` (default), no outliers are removed.
#'
#' @return A subset of `data` containing only rows from the specified `experiments` and
#'   within the specified `epoch_range`, with the specified outlier removed (if provided).
#'   The `versuch` column is returned as a factor with levels corresponding to `experiments`.
#'
#' @examples
#' # Keep only epochs 289-299 for specified experiments
#' filtered <- filter_data(my_data, experiments = c("001","002","003"), epoch_range = c(289, 299))
#'
#' # Remove a specific outlier
#' filtered <- filter_data(
#'   my_data,
#'   experiments = c("001","002","003"),
#'   epoch_range = c(289, 299),
#'   outlier_filter = list(versuch = "001", SuperRank = 6)
#' )
filter_data <- function(data, experiments, epoch_range, outlier_filter = NULL) {

  # Step 1: Filter data to include only the specified epochs
  subdata <- data[data$Epoche >= epoch_range[1] & data$Epoche <= epoch_range[2], ]

  # Step 2: Remove outliers (specific versuch and SuperRank)
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(subdata$versuch == outlier_filter$versuch &
                           subdata$SuperRank == outlier_filter$SuperRank), ]
  }

  # Step 3: Keep only valid versuch levels and factorize
  subdata <- subdata[subdata$versuch %in% experiments, ]
  subdata$versuch <- factor(subdata$versuch , levels=experiments)

  return(subdata)
}

#' Shrink the first GeomPoint layer in a ggplot object
#'
#' This function reduces the size of the first `GeomPoint` layer in a ggplot object.
#' Useful when a plot has multiple layers but you want to specifically adjust the
#' point symbols that come from a particular data layer (e.g., `symbol_df` in
#' `analyze_data()`), without affecting other points or boxplots.
#'
#' @param p A ggplot object. The plot whose first point layer you want to resize.
#' @param new_size Numeric. The desired size for the points in the first GeomPoint layer.
#'                 Default is 1.3.
#'
#' @return The same ggplot object `p` with the first GeomPoint layer's size modified.
#'
#' @details
#' - Only modifies the **first** layer that is a `GeomPoint`. Other point layers,
#'   such as outliers in `geom_boxplot()` or optional median points, are not affected.
#' - If no GeomPoint layer is found, execution **stops** with an informative error,
#'   to prevent silently producing an incorrect plot if `analyze_data()` changes.
#'
#' @examples
#' # Suppose 'p' is returned from analyze_data()
#' p_small <- shrink_first_point_layer(p, new_size = 1.5)
shrink_first_point_layer <- function(p, new_size = 1.3) {
  # Find the index of the first layer that is a GeomPoint
  point_layer_index <- which(sapply(p$layers, function(l) "GeomPoint" %in% class(l$geom)))[1]

  # Stop execution if no GeomPoint layer exists
  if (is.na(point_layer_index)) {
    stop("No GeomPoint layer found to shrink. Check if analyze_data() changed the plot structure!")
  }

  # Adjust the size of the points in the first GeomPoint layer
  p$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(p)
}

#' Add Significance Stars to a Boxplot
#'
#' This function adds statistical significance annotations (brackets and stars)
#' to a ggplot boxplot based on pairwise test results (Mann–Whitney U test for
#' two groups or Dunn’s test for three groups).
#'
#' The function supports up to three groups and automatically decides whether
#' annotations are placed above or below the data depending on available space.
#' Brackets are drawn using `ggpubr::stat_pvalue_manual()`, while labels are
#' positioned manually for full control the over layout.
#'
#' @param p A ggplot object (typically a boxplot).
#' @param subdata A data frame containing the plotted data.
#' @param metric A character string specifying the numeric variable used in the plot.
#' @param stats A list containing statistical test results. Must include either:
#'   - `mann_whitney_df` for two-group comparisons, or
#'   - `dunn_result$res` for three-group comparisons.
#' @param alpha A numeric significance threshold (default is 0.05).
#'
#' @return A ggplot object with significance annotations added.
#'
#' @details
#' The function computes significance levels and converts them into star labels
#' ("***", "**", "*", "ns"). Annotation positions are determined using a fixed
#' vertical spacing system (`step_base`) to ensure consistent spacing across plots.
#'
#' The x-axis midpoint between the compared groups is computed based on the factor
#' ordering of `subdata$versuch`.
#'
#' @examples
#' \dontrun{
#' p <- ggplot(df, aes(x = group, y = value)) + geom_boxplot()
#' p <- add_significance_stars(p, df, "value", stats)
#' }
add_significance_stars <- function(p, subdata, metric, stats, alpha = 0.05) {

  if (is.null(stats)) return(p)

  # Only add asterices to boxplots with 2 or 3 groups to avoid clutter
  group_count <- length(unique(subdata$versuch))
  if (group_count > 3) return(p)

  y_vals <- subdata[[metric]]
  y_max <- max(y_vals, na.rm = TRUE)
  y_min <- min(y_vals, na.rm = TRUE)

  # Fixed vertical spacing between significance annotations (in data units)
  # Controls distance between stacked brackets
  step_base <- 0.06

  # Vertical offset between bracket and label if annotations are placed below data
  # (used to move the label below the bracket line)
  label_offset <- 0.04

  # Extract df_pvalues for the different cases
  df_pvalues <- NULL

  # Case: Two groups
  if (group_count == 2 && !is.null(stats$mann_whitney_df)) {

    mw <- stats$mann_whitney_df

    df_pvalues <- data.frame(
      group1 = mw$Group1,
      group2 = mw$Group2,
      p = mw$p.value
    )
  }

  # Case: Three groups
  if (group_count == 3) {

    dunn <- stats$dunn_result$res
    dunn <- dunn[dunn$P.adj < alpha, ]
    
    if (nrow(dunn) == 0) return(p)

    df_pvalues <- data.frame(
      group1 = dunn$Group1,
      group2 = dunn$Group2,
      p = dunn$P.adj
    )
  }
  
  if (is.null(df_pvalues) || nrow(df_pvalues) == 0) return(p)

  # Convert p-values into significance symbols
  df_pvalues$p.signif <- ifelse(df_pvalues$p < 0.001, "***",
                                ifelse(df_pvalues$p < 0.01, "**",
                                       ifelse(df_pvalues$p < alpha, "*", "ns")))

  n <- nrow(df_pvalues)

  # Plot boundary assumptions (fixed scale system)
  # These define the decision rule for placing annotations above or below the data
  y_limit_min <- 0
  y_limit_max <- 1

  space_above <- y_limit_max - y_max
  space_below <- y_min - y_limit_min

  place_above <- space_above >= space_below

  # Geometry rule:
  # - y.position defines the bracket y-position
  # - y.label defines the text y-position
  # - The stacking uses constant step_base (not data-dependent scaling)

  if (place_above) {

    df_pvalues$y.position <- y_max + step_base * seq_len(n)

    # If above the data, the label is placed directly at the bracket height
    # (no additional offset needed for readability)
    df_pvalues$y.label <- df_pvalues$y.position

    tip <- 0.02

  } else {

    df_pvalues$y.position <- y_min - step_base * seq_len(n)

    # If below the data, the labels are shifted downward to be below the bracket
    df_pvalues$y.label <- df_pvalues$y.position - label_offset

    tip <- -0.02
  }

  # Dummy column to suppress ggpubr internal label rendering
  df_pvalues$label_dummy <- ""

  # Draw brackets only (labels suppressed)
  p <- p + ggpubr::stat_pvalue_manual(
    df_pvalues,
    label = "label_dummy",
    xmin = "group1",
    xmax = "group2",
    y.position = "y.position",
    tip.length = tip
  )

  # Calculate the midpoint between the groups in x-axis space
  # Assumes ggplot uses factor ordering of subdata$versuch
  group_levels <- levels(factor(subdata$versuch))

  df_pvalues$x_mid <- (match(df_pvalues$group1, group_levels) +
                         match(df_pvalues$group2, group_levels)) / 2

  # Draw significance labels manually (fully controlled positioning)
  p <- p + geom_text(
    data = df_pvalues,
    aes(x = x_mid, y = y.label, label = p.signif),
    inherit.aes = FALSE,
    size = 3
  )

  return(p)
}

#' Compute effect sizes from Mann–Whitney U statistic
#'
#' Calculates multiple effect size measures based on the Mann–Whitney U statistic.
#' Includes Z (normal approximation), r (standardized effect size),
#' rank-biserial correlation (RBC), and common language effect size (CLES).
#'
#' @param U Numeric. Mann–Whitney U statistic (as returned by \code{wilcox.test}).
#' @param n1 Integer. Sample size of group 1.
#' @param n2 Integer. Sample size of group 2.
#'
#' @details
#' The Z value is computed using the normal approximation:
#' \deqn{Z = (U - \mu_U) / \sigma_U}
#' where \eqn{\mu_U = n1 * n2 / 2} and
#' \eqn{\sigma_U = sqrt(n1 * n2 * (n1 + n2 + 1) / 12)}.
#'
#' Effect sizes:
#' \itemize{
#'   \item \strong{r}: \eqn{Z / sqrt(n)}, comparable across nonparametric tests
#'   \item \strong{rbc}: rank-biserial correlation, ranges [-1, 1]
#'   \item \strong{cles}: common language effect size, probability that a value
#'         from group 1 exceeds a value from group 2
#' }
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{Z}{Numeric. Standardized test statistic}
#'   \item{r}{Numeric. Effect size based on Z}
#'   \item{rbc}{Numeric. Rank-biserial correlation}
#'   \item{cles}{Numeric. Common language effect size}
#' }
#'
#' @seealso \code{\link{wilcox.test}}
#'
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(4, 5, 6)
#' test <- wilcox.test(x, y, exact = FALSE)
#' compute_effects_from_U(as.numeric(test$statistic), length(x), length(y))
compute_effects_from_U <- function(U, n1, n2) {
  n <- n1 + n2
  
  mu_U <- n1 * n2 / 2
  sigma_U <- sqrt(n1 * n2 * (n + 1) / 12)
  Z <- (U - mu_U) / sigma_U
  
  r    <- Z / sqrt(n)
  rbc  <- (2 * U) / (n1 * n2) - 1
  cles <- U / (n1 * n2)
  
  list(Z = Z, r = r, rbc = rbc, cles = cles)
}

#' Compute standardized effect size r from Z statistic
#'
#' Computes the standardized effect size r and its qualitative interpretation
#' from a Z statistic. This is commonly used for nonparametric tests such as
#' Mann–Whitney U and Dunn's test.
#'
#' @param Z Numeric. Z statistic (e.g., from Dunn's test or normal approximation).
#' @param n Integer. Total sample size used to compute Z.
#'
#' @details
#' The effect size is calculated as:
#' \deqn{r = Z / sqrt(n)}
#'
#' Interpretation follows common thresholds:
#' \itemize{
#'   \item < 0.1: negligible
#'   \item < 0.3: small
#'   \item < 0.5: medium
#'   \item >= 0.5: large
#' }
#'
#' @return A list with:
#' \describe{
#'   \item{r}{Numeric. Standardized effect size}
#'   \item{strength}{Character. Qualitative interpretation of effect size}
#' }
#'
#' @examples
#' compute_effects_from_Z(Z = 2.1, n = 30)
compute_effects_from_Z <- function(Z, n) {
  r <- Z / sqrt(n)
  
  strength <- dplyr::case_when(
    abs(r) < 0.1 ~ "negligible",
    abs(r) < 0.3 ~ "small",
    abs(r) < 0.5 ~ "medium",
    TRUE         ~ "large"
  )
  
  list(r = r, strength = strength)
}

#' Schema definition for job configurations
#'
#' Defines the expected structure and validation rules for a job object
#' used in analysis pipelines. This schema is used by \code{validate_job()}
#' to enforce consistency across job definitions.
#'
#' @format A named list where each element defines a field in a job
#' configuration. Each field is itself a list with validation rules:
#' \itemize{
#'   \item \code{type}: expected data type (currently "character")
#'   \item \code{length}: required length or \code{NA} for variable length
#'   \item \code{required}: logical, whether the field must be present
#'   \item \code{allow_empty}: logical, whether empty strings are allowed
#' }
#'
#' The following fields are defined:
#' \itemize{
#'   \item \code{name}: character(1), required, not empty
#'   \item \code{title}: character(1), required, not empty
#'   \item \code{file_prefix}: character(1), required, may be empty
#'   \item \code{lm_prefix}: character(1), required, may be empty
#'   \item \code{experiments}: character vector, required, must not be empty
#' }
#'
#' @details
#' This object acts as the single source of truth for job structure.
#' Modifications to this schema will affect all validation behavior
#' in \code{validate_job()} and \code{validate_jobs()}.
#'
#' @seealso \code{\link{validate_job}}, \code{\link{validate_jobs}}
#'
#' @export
JOB_SCHEMA <- list(
  name = list(
    type = "character",
    length = 1,
    required = TRUE,
    allow_empty = FALSE
  ),
  title = list(
    type = "character",
    length = 1,
    required = TRUE,
    allow_empty = FALSE
  ),
  file_prefix = list(
    type = "character",
    length = 1,
    required = TRUE,
    allow_empty = TRUE
  ),
  lm_prefix = list(
    type = "character",
    length = 1,
    required = TRUE,
    allow_empty = TRUE
  ),
  experiments = list(
    type = "character",
    length = NA,   # variable length
    required = TRUE,
    allow_empty = FALSE
  )
)

#' Validate a single job configuration
#'
#' Checks that a job list conforms to the expected schema defined in
#' \code{JOB_SCHEMA}. This includes verifying required fields, detecting
#' unexpected fields, and validating field types and constraints.
#'
#' @param job A named list representing a single job configuration.
#'   The expected structure is defined by \code{JOB_SCHEMA}.
#'
#' @param idx Optional integer index of the job. If provided, it is included
#'   in error messages to make debugging batches of jobs easier.
#'
#' @param schema A named list defining the expected structure of the job.
#'   Defaults to \code{JOB_SCHEMA}. Each field must specify:
#'   \itemize{
#'     \item \code{type}: expected data type (currently "character")
#'     \item \code{length}: required length or \code{NA} for variable length
#'     \item \code{allow_empty}: whether empty strings are allowed
#'   }
#'
#' @return Invisibly returns \code{TRUE} if validation succeeds.
#'
#' @details
#' Validation includes:
#' \itemize{
#'   \item Checking for missing required fields
#'   \item Checking for unexpected extra fields
#'   \item Verifying data types and lengths
#'   \item Ensuring required fields are not empty
#' }
#'
#' If validation fails, an error is thrown immediately with a descriptive
#' message indicating the problem.
#'
#' @examples
#' job <- list(
#'   name = "example",
#'   title = "Example job",
#'   file_prefix = "Fig_",
#'   lm_prefix = "",
#'   experiments = c("001", "002")
#' )
#'
#' validate_job(job)
#'
#' @seealso \code{\link{validate_jobs}}, \code{\link{JOB_SCHEMA}}
#' @export
validate_job <- function(job, idx = NULL, schema = JOB_SCHEMA) {
  prefix <- if (!is.null(idx)) paste0("Job[[", idx, "]]: ") else ""

  schema_fields <- names(schema)
  job_fields <- names(job)

  # Missing fields
  missing <- setdiff(schema_fields, job_fields)
  if (length(missing) > 0) {
    stop(prefix, "Missing field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Extra fields
  extra <- setdiff(job_fields, schema_fields)
  if (length(extra) > 0) {
    stop(prefix, "Unexpected field(s): ", paste(extra, collapse = ", "), call. = FALSE)
  }

  # Field-by-field validation
  for (field in schema_fields) {
    rules <- schema[[field]]
    value <- job[[field]]

    # Type check
    if (rules$type == "character" && !is.character(value)) {
      stop(prefix, sprintf("`%s` must be of type character.", field), call. = FALSE)
    }

    # Length check
    if (!is.na(rules$length) && length(value) != rules$length) {
      stop(prefix, sprintf("`%s` must have length %d.", field, rules$length), call. = FALSE)
    }

    if (is.na(rules$length) && length(value) == 0) {
      stop(prefix, sprintf("`%s` must not be empty.", field), call. = FALSE)
    }

    # Empty string check
    if (!rules$allow_empty && any(nchar(value) == 0)) {
      stop(prefix, sprintf("`%s` contains empty strings.", field), call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Validate a list of job configurations
#'
#' Applies \code{validate_job()} to each element of a list of jobs and
#' collects all validation errors. Stops with a combined error message
#' if any job is invalid.
#'
#' @param jobs A list of job configurations (each a named list).
#'
#' @return Returns \code{TRUE} invisibly if all jobs are valid.
#'
#' @details
#' Unlike \code{validate_job()}, this function does not stop at the first
#' error. Instead, it evaluates all jobs and reports all detected issues
#' in a single error message.
#'
#' @examples
#' jobs <- list(
#'   list(
#'     name = "job1",
#'     title = "First job",
#'     file_prefix = "A_",
#'     lm_prefix = "",
#'     experiments = c("001")
#'   ),
#'   list(
#'     name = "job2",
#'     title = "Second job",
#'     file_prefix = "B_",
#'     lm_prefix = "LM_",
#'     experiments = c("002", "003")
#'   )
#' )
#'
#' validate_jobs(jobs)
#'
#' @seealso \code{\link{validate_job}}
#'
#' @export
validate_jobs <- function(jobs) {
  errors <- character(0)

  for (i in seq_along(jobs)) {
    job <- jobs[[i]]

    result <- tryCatch(
      {
        validate_job(job, i)
        NULL  # no error
      },
      error = function(e) {
        e$message
      }
    )

    if (!is.null(result)) {
      errors <- c(errors, result)
    }
  }

  if (length(errors) > 0) {
    stop(
      paste(
        "Validation failed for the following job(s):",
        paste0("- ", errors, collapse = "\n"),
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

################
# Subfunctions #
################

#' Prepare Symbol Data for Plotting
#'
#' This function takes a mapping of symbols to experiments and a symbol configuration
#' and returns a structured list suitable for plotting. It also checks
#' that all experiments have a corresponding symbol mapping and that all symbols
#' have defined y-positions.
#'
#' @param symbol_map A named list mapping experiments (names) to symbols (vector of strings).
#'                   Example: list("exp1" = c("A", "B"), "exp2" = c("C"))
#' @param symbol_config A data.frame containing symbol properties. Must include at least:
#'                      - symbol: the symbol name (string)
#'                      - y: numeric y-position for plotting
#'                      - shape: shape code for plotting (optional)
#' @param experiments Character vector of experiments to include. Only these experiments
#'                    are retained from the symbol_map.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{symbol_df}{A data.frame with columns:
#'     - symbol: symbol name
#'     - versuch: experiment name (factor, ordered as in `experiments`)
#'     - y: y-position for plotting
#'     - shape: optional shape for plotting
#'   }
#'   \item{legend_symbols}{A character vector of symbols, ordered for use in legends.}
#' }
#'
#' @examples
#' symbol_map <- list("001" = c("A", "B"), "002" = c("C"))
#' symbol_config <- data.frame(
#'   symbol = c("A", "B", "C"),
#'   y = c(0.2, 0.5, 0.8),
#'   shape = c(21, 22, 23)
#' )
#' experiments <- c("001", "002")
#' symbols <- prepare_symbol_data(symbol_map, symbol_config, experiments)
#' symbols$symbol_df       # the symbol dataset for plotting
#' symbols$legend_symbols  # ordered symbols for legend
#'
#' @export
prepare_symbol_data <- function(symbol_map, symbol_config, experiments) {

  # Step 1: Ensure all experiments have a corresponding symbol mapping
  missing <- setdiff(experiments, names(symbol_map))

  if (length(missing) > 0) {
    stop(paste("Missing symbol_map entries for:", paste(missing, collapse=", ")))
  }

  # Step 2: Create symbol dataset (independent of main data)
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

  # Step 3: Safety checks
  missing <- setdiff(symbol_df$symbol, symbol_config$symbol)
  if (length(missing) > 0) {
    stop("Missing symbol_config entries for: ", paste(missing, collapse = ", "))
  }
  if (any(is.na(symbol_df$y))) {
    stop("Some symbols have no y-position defined")
  }

  # Step 4: Prepare for return
  symbols <- list(
    symbol_df = symbol_df,
    legend_symbols = legend_symbols
  )

  return(symbols)
}

#' Create a Dunn Test Significance Heatmap with Symbols
#'
#' Generates a `ggplot` heatmap showing pairwise Dunn test significance between experiments.
#' Significance annotations are shown in tiles ("-", "ns", "*", "**", "***"), and symbols 
#' can be mapped to the rows and columns to indicate experimental groups. 
#' The function also saves the plot to PDF and SVG files.
#'
#' @param sig_matrix_df A data frame containing significance annotations for each pairwise
#'   comparison. Rows and columns should be experiments. Allowed values: "-", "ns", "*", "**", "***".
#' @param symbol_map A named list mapping experiment names to vectors of symbol names
#'   for annotation on the heatmap edges (right side and top).
#' @param symbol_config A data frame describing each symbol, with at least columns:
#'   - `symbol`: symbol name
#'   - `shape`: integer or character code for ggplot2 shapes
#'   - `y`: numeric y-position for placement along axes
#' @param legend_symbols A character vector of symbols to show in the plot legend, in plotting order.
#' @param base_filename Character, the base file name (without extension) to save the heatmap plots as PDF and SVG.
#' @param plot_title Character, the title of the heatmap plot. Default is `"Heatmap"`.
#'
#' @return Invisibly returns a `ggplot` object representing the Dunn significance heatmap with symbols.
#'   The function also saves the heatmap to PDF and SVG files using `base_filename`.
#'
#' @examples
#' # Example: simple 3x3 Dunn matrix with symbols
#' sig_matrix <- data.frame(
#'   A = c("-", "*", "ns"),
#'   B = c("*", "-", "**"),
#'   C = c("ns", "**", "-")
#' )
#' rownames(sig_matrix) <- c("A", "B", "C")
#' symbol_map <- list(A = c("s1"), B = c("s2"), C = c("s3"))
#' symbol_config <- data.frame(
#'   symbol = c("s1", "s2", "s3"),
#'   shape = c(15, 16, 17),
#'   y = c(1, 2, 3)
#' )
#' legend_symbols <- c("s1", "s2", "s3")
#'
#' heatmap_plot <- create_dunn_heatmap_plot(
#'   sig_matrix_df = sig_matrix,
#'   symbol_map = symbol_map,
#'   symbol_config = symbol_config,
#'   legend_symbols = legend_symbols,
#'   base_filename = "example_heatmap"
#' )
#' print(heatmap_plot)
#'
#' @export
create_dunn_heatmap_plot <- function(
    sig_matrix_df,
    symbol_map,
    symbol_config,
    legend_symbols,
    base_filename,
    plot_title = "Heatmap"
) {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_filename)

    # Step 1: Convert significance to numeric
  sig_numeric <- sig_matrix_df
  sig_numeric[sig_numeric == "-"] <- NA
  sig_numeric[sig_numeric == "ns"] <- 0
  sig_numeric[sig_numeric == "*"]  <- 1
  sig_numeric[sig_numeric == "**"] <- 2
  sig_numeric[sig_numeric == "***"]<- 3
  sig_numeric <- as.data.frame(apply(sig_numeric, 2, as.numeric))

  # Add Group names explicitly from rownames
  sig_numeric$Group <- rownames(sig_matrix_df)

  # Melt for ggplot
  sig_melt <- reshape2::melt(sig_numeric, id.vars = "Group",
                             variable.name = "Comparison", value.name = "Significance")

  # Ensure ordering matches original experiment order and the excel order
  sig_melt$Group <- factor(sig_melt$Group, levels = rev(rownames(sig_matrix_df)))
  sig_melt$Comparison <- factor(sig_melt$Comparison, levels = colnames(sig_matrix_df))

  # Step 2: Prepare symbol positions for right-side
  symbol_side <- data.frame(
    Group = rep(names(symbol_map), lengths(symbol_map)),
    symbol = unlist(symbol_map),
    stringsAsFactors = FALSE
  )

  # Merge with shape info
  symbol_side <- merge(symbol_side, symbol_config, by = "symbol", all.x = TRUE)

  # Factor levels
  symbol_side$Group <- factor(symbol_side$Group, levels = levels(sig_melt$Group))

  # Spread symbols horizontally in their own column to the right
  last_tile_x <- length(levels(sig_melt$Comparison))
  x_scale <- 20   # Controls how wide the symbols spread using y-offset as scaling
  x_offset <- 0   # Horizontal offset for symbols to the right of tiles

  symbol_side <- symbol_side %>%
    group_by(Group) %>%
    mutate(
      xpos = last_tile_x + x_offset + (y - 1) * x_scale,
      ypos = as.numeric(Group)
    ) %>%
    ungroup()

  # Step 3: Prepare top annotation symbols (same spread logic, rotated above heatmap)
  top_symbols <- symbol_side %>%
    # Keep one row per symbol
    group_by(Group) %>%
    mutate(
      xpos = as.numeric(factor(Group, levels = colnames(sig_matrix_df))), # Align above each heatmap column
      ypos = last_tile_x + x_offset + (y - 1) * x_scale,                  # spread symbols vertically using y-offset
    ) %>%
    ungroup()

  # Heatmap plot with symbols in the same panel
  heatmap_plot <- ggplot() +
    # Heatmap tiles
    geom_tile(data = sig_melt, aes(x = as.numeric(Comparison), y = as.numeric(Group), fill = Significance),
              color = "black") +
    scale_fill_gradientn(
      colors = c("white", "#FFC0C0", "#FF6666", "#990000"), # light to dark red
      limits = c(0, 3),
      na.value = "grey90",
      breaks = 0:3,
      labels = c("ns", "*", "**", "***")
    ) +
    # Right-side symbols
    geom_point(data = symbol_side, aes(x = xpos, y = ypos, shape = symbol),
               size = 3, color = "black") +
    # Top symbols above heatmap
    geom_point(data = top_symbols, aes(x = xpos, y = ypos, shape = symbol),
               size = 3, color = "black") +
    # Shape legend mapping
    scale_shape_manual(
      values = setNames(symbol_config$shape, symbol_config$symbol),
      breaks = legend_symbols,
      na.translate = FALSE
    ) +
    # x-axis (heatmap columns)
    scale_x_continuous(
      breaks = 1:last_tile_x,
      labels = levels(sig_melt$Comparison),
      expand = c(0, 0)
    ) +
    # y-axis (heatmap rows)
    scale_y_continuous(
      breaks = 1:length(levels(sig_melt$Group)),
      labels = levels(sig_melt$Group),
      expand = c(0, 0)
    ) +
    # Ensure square tiles and expand y-limits to fit top symbols
    coord_fixed(
      ratio = 1,
      xlim = c(0.5, max(symbol_side$xpos) + 0.5),
      ylim = c(0.5, max(top_symbols$ypos) + 0.5)
    ) +
    theme_minimal() +
    labs(
      title = paste0(plot_title, " Significance Heatmap"),
      x = "Experiment",
      y = "Experiment"
    ) +
    theme(
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      legend.margin = margin(t = 10, r = 10, b = 10, l = 10),      # Adds padding around legend box
      legend.spacing.y = unit(0.5, "cm"),                          # Increases vertical spacing between items
      legend.spacing.x = unit(0.5, "cm"),                          # Increases horizontal spacing between items
      plot.title = element_text(hjust = 0.5),                      # Center the plot title
      plot.title.position = "plot"
    )

  # Step 5: Save plots
  ggsave(
    filename = paste0(base_filename, "_dunn_heatmap.pdf"),
    plot = heatmap_plot,
    width = 6,
    height = 5
  )
  ggsave(
    filename = paste0(base_filename, "_dunn_heatmap.svg"),
    plot = heatmap_plot,
    width = 6,
    height = 5
  )

  # Step 6: Return heatmap_plot object explicitly (invisible)
  return(invisible(heatmap_plot))
}

#' Create a Boxplot with Optional Symbol Annotations
#'
#' Generates a boxplot of a given metric across experimental groups, optionally overlaying
#' individual symbols, median-of-medians points, and raw medians for annotated visualizations.
#' The y-axis is formatted with fixed breaks (0.0, 0.1, …, 1.0) to avoid floating-point artifacts.
#'
#' @param subdata Data frame containing raw values per experimental group and SuperRank.
#'   Must contain at least:
#'   - `versuch`: factor or character identifying the experiment
#'   - `SuperRank`: numeric grouping variable for median calculation
#'   - `metric` column (as specified in `metric` argument)
#' @param experiments Character vector specifying the order of experimental groups (versuch).
#'   Used to set factor levels for correct boxplot order.
#' @param symbol_df Data frame containing symbols to plot per group, with columns:
#'   - `versuch`: experiment name
#'   - `y`: y-position of symbol
#'   - `symbol`: symbol identifier
#' @param symbol_config Data frame describing symbols, with at least columns:
#'   - `symbol`: symbol name
#'   - `shape`: integer or character code for ggplot2 shapes
#' @param legend_symbols Character vector of symbols to display in the legend, in plotting order.
#' @param metric Character, the column name in `subdata_median` containing the values to plot.
#' @param metric_name Character, label for the y-axis.
#' @param base_filename Character, base file name (without extension) to save the plot as PDF and SVG.
#' @param plot_title Character, title of the plot. Defaults to `"Plot Title"`.
#' @param mode Character, plotting mode. Options:
#'   - `"default"`: only boxplot and symbols
#'   - `"annotated"`: overlays median-of-medians points (blue) with numeric labels and red raw medians
#'   - `"red_raw_medians"`: overlays only red raw median points
#'
#' @return A `ggplot` object representing the boxplot. Also saves the plot as PDF and SVG
#'   using `base_filename`.
#'
#' @note The y-axis is scaled from 0 to 1.25 with breaks explicitly formatted as 0.0, 0.1, … 1.0
#'   to avoid machine precision artifacts in axis labels.
#'
#' @examples
#' \dontrun{
#'   versuch = c("A","B","C"),
#'   y = c(0.55,0.75,0.65),
#'   symbol = c("s1","s2","s3")
#' )
#' 
#' symbol_config <- data.frame(
#'   symbol = c("s1","s2","s3"),
#'   shape = c(15,16,17)
#' )
#' 
#' legend_symbols <- c("s1","s2","s3")
#' 
#' # --- Call the function ---
#' create_boxplot(
#'   subdata = subdata,
#'   experiments = c("A","B","C"),
#'   symbol_df = symbol_df,
#'   symbol_config = symbol_config,
#'   legend_symbols = legend_symbols,
#'   metric = "value",
#'   metric_name = "Metric Value",
#'   base_filename = "my_boxplot",
#'   plot_title = "Example Boxplot",
#'   mode = "annotated"
#' )
#' }
#'
#' @export
create_boxplot <- function(subdata,
                           experiments,
                           symbol_df,
                           symbol_config,
                           legend_symbols,
                           metric,
                           metric_name,
                           base_filename,
                           plot_title = "Plot Title",
                           stats = NULL,
                           mode = "default") {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_filename)

  # Step 1: Create the main breaks, make sure it is 0.0, 0.1 etc instead of next machine number
  breaks_main <- seq(0, 1, 0.1)
  labels_main <- sprintf("%.1f", breaks_main)

  # Step 2: Compute median per SuperRank for each versuch
  form <- as.formula(paste(metric, "~ versuch + SuperRank"))
  subdata_median <- aggregate(form, data = subdata, median)

  # Step 3: Compute median of medians per versuch
  form <- as.formula(
    paste(metric, "~ versuch")
  )
  subdata_median_median <- aggregate(form, data = subdata_median, median)

  # Step 4: Ensure median datasets have correct factor levels
  subdata_median$versuch <- factor(subdata_median$versuch, levels = experiments)
  subdata_median_median$versuch <- factor(subdata_median_median$versuch, levels = experiments)

  # Step 5: Create Plot
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
      limits = c(0, 1.25),
      expand = c(0, 0),
      breaks = breaks_main,
      labels = labels_main,
      minor_breaks = NULL
    ) +
    ggtitle(plot_title) +
    theme(
      plot.title = element_text(color = "black", size = 9, hjust = 0.5),
      plot.title.position = "plot",
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid.major.y = element_line(colour = "grey60", size = 0.2),
      panel.grid.minor.y = element_blank()
    )

  # Step 6: Add optional annotations
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

  # Step 7: Add significance asterices on boxplots with 2 or 3 groups
  p <- add_significance_stars(p, subdata, metric, stats)

  # Step 8: Save plot as PDF and SVG
  ggsave(
    filename = paste0(base_filename, ".pdf"),
    plot = p,
    height = 5,
    width = 5
  )
  ggsave(
    filename = paste0(base_filename, ".svg"),
    plot = p,
    height = 5,
    width = 5
  )

  # Step 8: Return plot object explicitly (invisible)
  return(invisible(p))
}

#' Compute Statistical Tests and Effect Sizes
#'
#' This function applies multiple statistical analyses on a dataset for a given metric:
#' 
#' 1. **Kruskal-Wallis test** across the factor `versuch`.
#' 2. **Optional Mann-Whitney U test** if exactly 2 levels are present in `versuch`.
#'    - Computes effect size `r` using normal approximation of the U statistic.
#' 3. **Dunn's post-hoc pairwise test** with Bonferroni correction.
#'    - Computes effect size `r` for each pair.
#'    - Categorizes effect size strength as negligible, small, medium, or big.
#' 4. Generates **Dunn matrices** for adjusted p-values and significance symbols.
#'
#' @param subdata A `data.frame` containing the data. Must include columns:
#'   - `versuch` (factor or character) representing experimental groups.
#'   - The column corresponding to the `metric` parameter.
#' @param metric A `string` specifying the name of the numeric column in `subdata` to analyze.
#' @param experiments A character vector of experimental group names. Determines factor levels.
#' @param alpha Numeric. Significance threshold for Dunn test effect size categorization. Default is 0.05.
#'
#' @return A `list` with the following elements:
#' \describe{
#'   \item{kruskal_df}{`data.frame` with Kruskal-Wallis test results (statistic, df, p-value, method, data.name).}
#'   \item{mann_whitney_df}{`data.frame` with Mann-Whitney U test results if 2 groups exist, otherwise `NULL`. Columns include Group1, Median1, Group2, Median2, W statistic, effect size r, p-value, significance, and method.}
#'   \item{dunn_result}{Dunn test object containing pairwise comparisons with Bonferroni-adjusted p-values and effect sizes.}
#'   \item{p_matrix_df}{`data.frame` representing pairwise Bonferroni-adjusted p-values in matrix form.}
#'   \item{sig_matrix_df}{`data.frame` representing pairwise significance symbols ("***", "**", "*", "ns") in matrix form.}
#' }
#'
#' @details
#' - Effect size `r` for Mann-Whitney U is computed as Z / sqrt(N), where Z is approximated from the U statistic.
#' - Effect sizes for Dunn pairwise tests are categorized according to common thresholds:
#'   - |r| < 0.1 → negligible
#'   - |r| < 0.3 → small
#'   - |r| < 0.5 → medium
#'   - |r| ≥ 0.5 → big
#' - The function is suitable for datasets with 2 or more experimental groups.
#'
#' @examples
#' \dontrun{
#'   stats <- compute_statistics(subdata = my_data,
#'                               metric = "accuracy",
#'                               experiments = c("ExpA", "ExpB", "ExpC"))
#'   stats$kruskal_df
#'   stats$mann_whitney_df
#'   stats$p_matrix_df
#' }
#'
#' @export
compute_statistics <- function(subdata, metric, experiments, alpha = 0.05) {

  # Step 1: Create formula
  form <- as.formula(paste(metric, "~ versuch"))

  # Step 2: Kruskal-Wallis test across versuch
  # Note: Kruskal-Wallis is two-sided by default
  kruskal_result <- kruskal.test(form, data = subdata)
  kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                           parameter = kruskal_result$parameter,
                           p.value = kruskal_result$p.value,
                           method = kruskal_result$method,
                           data.name = kruskal_result$data.name)

  # Precompute medians per group (used in MW + Dunn)
  medians_df <- subdata %>%
    dplyr::group_by(versuch) %>%
    dplyr::summarise(Median = median(.data[[metric]], na.rm = TRUE)) %>%
    dplyr::rename(Group = versuch)

  # Step 3: Optional Mann-Whitney U test for exactly 2 groups
  group_count <- length(unique(subdata$versuch))
  if(group_count == 2) {

    groups <- levels(subdata$versuch)
    x <- subdata[[metric]][subdata$versuch == groups[1]]
    y <- subdata[[metric]][subdata$versuch == groups[2]]
    abs_median_diff = abs(median(x) - median(y))

    # Mann-Whitney U test (implemented as Wilcoxon rank-sum test in R)
    # Note: This is a two-sided test by default (alternative = "two.sided")
    mw_result <- wilcox.test(x, y, exact = FALSE)

    # Extract U and sample sizes
    U  <- as.numeric(mw_result$statistic)
    n1 <- length(x)
    n2 <- length(y)

    # Effect sizes are computed via helper functions (see compute_effects_from_U / _from_Z)
    eff_U <- compute_effects_from_U(U, n1, n2)          # Effect sizes from U
    eff_Z <- compute_effects_from_Z(eff_U$Z, n1 + n2)   # Effect size from Z (shared definition with Dunn)

    # Create structured result
    mann_whitney_df <- data.frame(
      Group1 = groups[1],
      Group2 = groups[2],

      Median1 = medians_df$Median[medians_df$Group == groups[1]],
      Median2 = medians_df$Median[medians_df$Group == groups[2]],
      abs_median_diff = abs_median_diff,
      # Magnitude effect (absolute scale)
      magnitude_effect = dplyr::case_when(
        abs_median_diff < 0.01 ~ "negligible",
        abs_median_diff < 0.05 ~ "small",
        abs_median_diff < 0.10 ~ "moderate",
        TRUE                   ~ "large"
      ),

      n1 = n1,
      n2 = n2,
      n_pair = n1 + n2,

      W = U,
      Z = eff_U$Z,

      rbc = eff_U$rbc,
      cles = eff_U$cles,
      cles_effect_size = dplyr::case_when(
        abs(eff_U$cles - 0.5) >= 0.21 ~ "large",
        abs(eff_U$cles - 0.5) >= 0.14 ~ "medium",
        abs(eff_U$cles - 0.5) >= 0.06  ~ "small",
        abs(eff_U$cles - 0.5) <  0.06  ~ "negligible"
      ),
      r = eff_U$r,
      r_effect_size_strength = eff_Z$strength,
      # Rank-based interpretation
      rank_effect = dplyr::case_when(
        abs(eff_Z$r) >= 0.5 ~ "large rank separation",
        abs(eff_Z$r) >= 0.3 ~ "moderate rank separation",
        TRUE           ~ "small rank separation"
      ),

      p.value = mw_result$p.value,
      significant = ifelse(mw_result$p.value < alpha, "Yes", "No"),

      decision_rule = dplyr::case_when(
        mw_result$p.value < alpha & abs(eff_Z$r) >= 0.5 ~ "statistically and practically large effect",
        mw_result$p.value < alpha & abs(eff_Z$r) >= 0.3 ~ "statistically significant, moderate effect",
        mw_result$p.value < alpha & abs(eff_Z$r) < 0.3  ~ "statistically significant, small practical effect",
        mw_result$p.value >= alpha                 ~ "no statistically significant difference",
        TRUE                                       ~ "uncategorised"
      ),

      method = mw_result$method
    )

  } else {
    mann_whitney_df <- NULL
  }

  # Step 4: Dunn's pairwise post-hoc test with Bonferroni correction and effect sizes
  dunn_result <- dunnTest(form, data = subdata, method = "bonferroni")

  dunn_result$res <- dunn_result$res %>%
    # Split 'Comparison' into Group1 and Group2 first
    tidyr::separate(Comparison, into = c("Group1", "Group2"), sep = " - ") %>%

    # Add medians for both groups
    dplyr::left_join(medians_df, by = c("Group1" = "Group")) %>%
    dplyr::rename(Median1 = Median) %>%
    dplyr::left_join(medians_df, by = c("Group2" = "Group")) %>%
    dplyr::rename(Median2 = Median) %>%

    # Compute statistics
    rowwise() %>%

    # FIRST mutate block: compute cles + everything else EXCEPT cles_effect_size
    mutate(
      n1 = sum(subdata$versuch == Group1),
      n2 = sum(subdata$versuch == Group2),
      n_pair = n1 + n2,

      cles = mean(outer(
        subdata[[metric]][subdata$versuch == Group1],
        subdata[[metric]][subdata$versuch == Group2],
        FUN = ">"
      )),
      r = compute_effects_from_Z(Z, n_pair)$r,
      r_effect_size_strength = compute_effects_from_Z(Z, n_pair)$strength,

      abs_median_diff = abs(Median1 - Median2)
    ) %>%

    # SECOND mutate block: interpret cles + classify everything derived from earlier values
    mutate(
      cles_effect_size = dplyr::case_when(
        abs(cles - 0.5) >= 0.21 ~ "large",
        abs(cles - 0.5) >= 0.14 ~ "medium",
        abs(cles - 0.5) >= 0.06 ~ "small",
        TRUE ~ "negligible"
      ),

      # Magnitude (absolute scale), assuming abs_median_diff is between 0 and 1
      magnitude_effect = dplyr::case_when(
        abs_median_diff < 0.01 ~ "negligible",
        abs_median_diff < 0.05 ~ "small",
        abs_median_diff < 0.10 ~ "moderate",
        TRUE                   ~ "large"
      ),

      # Rank-based interpretation
      rank_effect = dplyr::case_when(
        abs(r) >= 0.5 ~ "large rank separation",
        abs(r) >= 0.3 ~ "moderate rank separation",
        TRUE          ~ "small rank separation"
      ),

      decision_rule = dplyr::case_when(
        P.adj < alpha & abs(r) >= 0.5 ~ "statistically and practically large effect",
        P.adj < alpha & abs(r) >= 0.3 ~ "statistically significant, moderate effect",
        P.adj < alpha & abs(r)  < 0.3 ~ "statistically significant, small practical effect",
        P.adj >= alpha                ~ "no statistically significant difference",
        TRUE                          ~ "uncategorised"
      ),

      significant = ifelse(P.adj < alpha, "Yes", "No")
    ) %>%
    ungroup() %>%

    # Reorder columns for clarity
    select(
      Group1, Group2,
      Median1, Median2,
      abs_median_diff,
      magnitude_effect,

      n1, n2, n_pair,
      Z, cles, cles_effect_size, r, r_effect_size_strength,
      rank_effect,

      P.unadj, P.adj,
      significant, decision_rule
    ) %>%
    as.data.frame()

  # Step 5: Create Dunn matrices (p-values and significance)
  groups <- levels(subdata$versuch)

  # Initialize matrices
  p_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups),
                     dimnames=list(groups, groups))

  sig_matrix <- matrix(NA, nrow=length(groups), ncol=length(groups),
                       dimnames=list(groups, groups))

  # Fill matrices
  for(i in 1:nrow(dunn_result$res)) {
    g1 <- dunn_result$res$Group1[i]
    g2 <- dunn_result$res$Group2[i]
    p  <- dunn_result$res$P.adj[i]

    # Fill p-value matrix
    p_matrix[g1, g2] <- p
    p_matrix[g2, g1] <- p

    # Determine significance level
    sig <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01, "**",
                         ifelse(p < 0.05, "*", "ns")))

    # Fill significance matrix
    sig_matrix[g1, g2] <- sig
    sig_matrix[g2, g1] <- sig
  }

  # Fill diagonal with "-"
  diag(sig_matrix) <- "-"

  # Step 5: Return results
  return(list(
    kruskal_df = kruskal_df,
    mann_whitney_df = mann_whitney_df,
    dunn_result = dunn_result,
    p_matrix_df = as.data.frame(p_matrix),
    sig_matrix_df = as.data.frame(sig_matrix)
  ))
}

#' Export statistical results to Excel
#'
#' This function takes the results of non-parametric statistical tests and
#' exports them to an Excel workbook. The workbook includes separate sheets
#' for the Kruskal-Wallis test, Dunn's post-hoc pairwise comparisons, Dunn
#' p-value and significance matrices, and optionally the Mann-Whitney U test
#' if two groups are compared.
#'
#' @param base_filename character. The base path and file name for the Excel
#'   workbook, without the ".xlsx" extension. The workbook will be saved as
#'   `paste0(base_filename, ".xlsx")`.
#' @param kruskal_df data.frame. Output of the Kruskal-Wallis test. Must contain
#'   columns such as statistic, parameter, p.value, method, and data.name.
#' @param dunn_result list. Output from `dunnTest()`, must include `res` data frame
#'   containing pairwise comparisons, unadjusted and adjusted p-values, and effect sizes.
#' @param p_matrix_df data.frame. Square matrix of pairwise Dunn adjusted p-values.
#'   Rows and columns correspond to factor levels in the data.
#' @param sig_matrix_df data.frame. Square matrix of significance symbols for Dunn
#'   pairwise tests (e.g., "*", "**", "***", "ns"). Same dimensions as `p_matrix_df`.
#' @param mann_whitney_df data.frame or NULL. Optional Mann-Whitney U test results
#'   when exactly two groups are compared. Includes W statistic, effect size r, p-value,
#'   median values for each group, and significance flag.
#'
#' @details
#' The Excel workbook structure:
#' \describe{
#'   \item{Kruskal-Wallis}{Summary of Kruskal-Wallis test results.}
#'   \item{Dunn Test}{Pairwise Dunn post-hoc comparisons table.}
#'   \item{Dunn Matrix (p)}{Matrix of Dunn adjusted p-values. Significant values
#'         (p < 0.05) are highlighted in blue. Diagonal cells are filled with "-".}
#'   \item{Dunn Matrix (sig)}{Matrix of significance symbols corresponding to
#'         the Dunn p-value matrix. Diagonal cells are "-".}
#'   \item{Mann-Whitney}{Optional sheet for Mann-Whitney U test if two groups
#'         are compared.}
#' }
#'
#' Numeric formatting for the Dunn p-value matrix is scientific notation with
#' 3 decimal places. Column widths are auto-adjusted, and header rows and first
#' columns are frozen for easier navigation.
#'
#' @examples
#' \dontrun{
#' # Assume you have a filtered dataset `subdata`, a metric `metric`, and experiment levels
#' experiments <- c("Exp1", "Exp2", "Exp3")
#' metric <- "value"
#' 
#' # Compute statistics
#' stats <- compute_statistics(subdata, metric, experiments)
#' 
#' # Export results to Excel
#' export_statistics_to_excel(
#'   base_filename = "results/statistics_summary",
#'   kruskal_df      = stats$kruskal_df,
#'   dunn_result     = stats$dunn_result,
#'   p_matrix_df     = stats$p_matrix_df,
#'   sig_matrix_df   = stats$sig_matrix_df,
#'   mann_whitney_df = stats$mann_whitney_df
#' )
#' }
#'
#' @export
export_statistics_to_excel <- function(base_filename,
                                       kruskal_df,
                                       dunn_result,
                                       p_matrix_df,
                                       sig_matrix_df,
                                       mann_whitney_df = NULL) {
    wb <- openxlsx::createWorkbook()

  # --- Kruskal-Wallis ---
  openxlsx::addWorksheet(wb, "Kruskal-Wallis")
  openxlsx::writeData(wb, "Kruskal-Wallis", kruskal_df)

  # --- Dunn Test (table) ---
  openxlsx::addWorksheet(wb, "Dunn Test")
  openxlsx::writeData(wb, "Dunn Test", dunn_result$res)

  # --- Dunn Matrix (p-values) ---
  openxlsx::addWorksheet(wb, "Dunn Matrix (p)")
  openxlsx::writeData(
    wb,
    "Dunn Matrix (p)",
    p_matrix_df,
    rowNames = TRUE,
    keepNA = FALSE
  )

  # Apply numeric formatting (3 decimals)
  p_style <- openxlsx::createStyle(numFmt = "0.00E+00;-0.00E+00;\"-\"")

  openxlsx::addStyle(
    wb,
    sheet = "Dunn Matrix (p)",
    style = p_style,
    rows = 2:(nrow(p_matrix_df) + 1),
    cols = 2:(ncol(p_matrix_df) + 1),
    gridExpand = TRUE
  )

  # --- Dunn Matrix (significance) ---
  openxlsx::addWorksheet(wb, "Dunn Matrix (sig)")
  openxlsx::writeData(wb, "Dunn Matrix (sig)", sig_matrix_df, rowNames = TRUE)

  # --- Optional Mann-Whitney ---
  if(!is.null(mann_whitney_df)) {
    openxlsx::addWorksheet(wb, "Mann-Whitney")
    openxlsx::writeData(wb, "Mann-Whitney", mann_whitney_df)
  }

  for(i in seq_len(nrow(p_matrix_df))) {
    openxlsx::writeData(
      wb,
      "Dunn Matrix (p)",
      "-",
      startRow = i + 1,
      startCol = i + 1
    )
  }

  # --- Highlight significant cells
  sig_highlight <- openxlsx::createStyle(bgFill = "#DCE6F1")

  openxlsx::conditionalFormatting(
    wb, "Dunn Matrix (p)",
    cols = 2:(ncol(p_matrix_df)+1),
    rows = 2:(nrow(p_matrix_df)+1),
    rule = "<0.05",
    style = sig_highlight
  )

  # --- Highlight diagonal
  diag_style <- openxlsx::createStyle(fgFill = "#EEEEEE")

  for(i in 1:nrow(p_matrix_df)) {
    openxlsx::addStyle(
      wb, "Dunn Matrix (p)", diag_style,
      rows = i+1, cols = i+1, gridExpand = FALSE
    )
  }

  # --- Auto column width ---
  for(sheet in openxlsx::sheets(wb)) {
    openxlsx::setColWidths(wb, sheet, cols = 1:20, widths = "auto")
  }

  # --- Freeze header row and column ---
  openxlsx::freezePane(wb, "Dunn Matrix (p)", firstRow = TRUE, firstCol = TRUE)
  openxlsx::freezePane(wb, "Dunn Matrix (sig)", firstRow = TRUE, firstCol = TRUE)

  # --- Save workbook ---
  openxlsx::saveWorkbook(wb, paste0(base_filename, ".xlsx"), overwrite = TRUE)
}

#' Extract and format model coefficients for plotting
#'
#' Converts a fitted linear model into a structured data.frame suitable for
#' coefficient plotting. The function extracts estimates, standard errors,
#' t-values, and p-values, and classifies terms into intercept, main effects,
#' and interaction levels.
#'
#' @param model A fitted linear model object (typically from \code{lm}).
#'
#' @return A data.frame with the following columns:
#' \itemize{
#'   \item \code{Training data set combinations}: term names from the model
#'   \item \code{Coefficients}: estimated regression coefficients
#'   \item \code{Std. Error}: standard errors of coefficients
#'   \item \code{t-value}: t-statistics
#'   \item \code{Pr(>|t|)}: p-values
#'   \item \code{TermType}: factor classifying terms as Intercept, Main Effect, or Interaction
#' }
#'
#' @details
#' Interaction terms are classified based on the number of ":" separators:
#' \itemize{
#'   \item No ":" → Main Effect
#'   \item One ":" → 2-way Interaction
#'   \item Two ":" → 3-way Interaction
#'   \item etc.
#' }
#'
#' The returned data.frame is ordered by term type (Intercept → Main Effects → Interactions)
#' and preserves coefficient order for plotting.
#'
#' This function provides a structured intermediate representation of model
#' output for downstream visualization or reporting such as in
#' \code{plot_coefficients()}.
#'
#' @examples
#' \dontrun{
#' model <- lm(y ~ A * B * C, data = df)
#' coef_df <- build_coef_df(model)
#' }
#'
#' @import dplyr
#' @import stringr
#' @export
build_coef_df <- function(model) {
  coefs <- summary(model)$coefficients
  
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

  coef_data$`Training data set combinations` <- factor(
    coef_data$`Training data set combinations`,
    levels = coef_data$`Training data set combinations`
  )

  coef_data <- coef_data[order(coef_data$TermType), ]

  return(coef_data)
}

#' Plot regression coefficients with significance annotations
#'
#' Creates a bar plot of linear or interaction model coefficients with standard
#' errors and significance stars (***, **, *, or empty not significant).
#' The function adapts the visual encoding depending on whether only main effects
#' are present or interaction terms are included. This function assumes input has
#' been generated by build_coef_df() or an equivalent structure.
#'
#' @param coef_data A data frame containing model coefficients. Must include:
#'   \itemize{
#'     \item \code{Coefficients}: Numeric coefficient estimates
#'     \item \code{Std. Error}: Standard errors of coefficients
#'     \item \code{Pr(>|t|)}: p-values for significance testing
#'     \item \code{TermType}: Factor indicating term type (e.g., Intercept, Main Effect, Interaction)
#'     \item \code{Training data set combinations}: Factor or character identifying model terms
#'   }
#'
#' @param metric_name The name of the metric that is to be used on the y-axis
#' @param title Character string specifying the plot title.
#'
#' @return A \code{ggplot} object displaying coefficient estimates with error bars
#' and significance annotations.
#'
#' @details
#' Significance levels are encoded as:
#' \itemize{
#'   \item \code{***} for p < 0.001
#'   \item \code{**}  for p < 0.01
#'   \item \code{*}   for p < 0.05
#'   \item empty string otherwise
#' }
#'
#' The function automatically switches between:
#' \itemize{
#'   \item Simple mode (Intercept + Main Effects only): No legend, single-color bars
#'   \item Full mode (interaction models): Colored bars by term type with legend
#' }
#'
#' Error bars represent ±1 standard error around coefficient estimates.
#'
#' @examples
#' \dontrun{
#' plot_coefficients(coef_data, metric_name, title = "Model Coefficients")
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @export
plot_coefficients <- function(coef_data, metric_name, title) {

  present_types <- unique(coef_data$TermType)
  simple_case <- all(present_types %in% c("Intercept", "Main Effect"))

  coef_data$Significance <- dplyr::case_when(
    coef_data$`Pr(>|t|)` < 0.001 ~ "***",
    coef_data$`Pr(>|t|)` < 0.01  ~ "**",
    coef_data$`Pr(>|t|)` < 0.05  ~ "*",
    TRUE                         ~ ""
  )

  offset <- 0.02 * max(abs(coef_data$Coefficients), na.rm = TRUE)
  coef_data$label_y <- ifelse(
    coef_data$Coefficients >= 0,
    coef_data$Coefficients + coef_data$`Std. Error` + offset,  # Above
    coef_data$Coefficients - coef_data$`Std. Error` - offset   # Below
  )

  # Base plot (shared)
  p <- ggplot(coef_data, aes(
    x = `Training data set combinations`,
    y = Coefficients
  )) +
  geom_bar(stat = "identity") +
  geom_errorbar(
    aes(
      ymin = Coefficients - `Std. Error`,
      ymax = Coefficients + `Std. Error`
    ),
    width = 0.2
  ) +
  geom_text(
    aes(
      label = Significance,
      vjust = ifelse(Coefficients >= 0, 0.6, 1.0),
      y = label_y
    ),
    size = 3
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 9)
  ) +
  ggtitle(title)

  # Add complexity only if needed
  if (simple_case) {
    p <- p +
      labs(
      x = "Training datasets",
      y = paste0("Coefficients of ", metric_name, " ranks")
    ) +
    theme(legend.position = "none")
  }
  else {
    p <- p +
      labs(
        x = "Training dataset combinations",
        y = paste0("Coefficients of ", metric_name, " ranks")
      ) +
      aes(fill = TermType) +
      scale_fill_manual(values = c(
        "Intercept" = "gray70",
        "Main Effect" = "steelblue",
        "2-way Interaction" = "#fdae61",
        "3-way Interaction" = "#f46d43",
        "4-way Interaction" = "#d73027"
      )) +
      theme(legend.position = "right")
  }

  return(p)
}

#############
# Functions #
#############

#' Analyze experimental data with non-parametric tests and annotated plots
#'
#' Executes a full workflow for experimental metrics:
#' 
#' 1. Filters data to a specified epoch range and removes specified outliers.
#' 2. Prepares symbols for plotting via `symbol_map` and `symbol_config`.
#' 3. Computes the Kruskal-Wallis test and, optionally, the Mann-Whitney test, as well as Dunn's post-hoc tests
#'    with Bonferroni correction, including effect sizes (r) and significance labels.
#' 4. Exports all statistical results to Excel sheets (`Kruskal-Wallis`, `Dunn Test`, optional `Mann-Whitney`).
#' 5. Generates boxplots with optional median-of-medians annotations and raw medians
#'    via `create_boxplot`, saving PDF and SVG files.
#' 6. Generates a Dunn post-hoc significance heatmap via `create_dunn_heatmap_plot`, saved to disk.
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
#' @param base_filename Base file name (without extension) for saved plots and Excel sheets (default `"plot"`).
#' @param mode Optional plotting mode: `"default"` creates plain boxplots, `"annotated"` adds median-of-medians points and labels + red raw medians, `"red_raw_medians"` overlays only raw median points in red.
#' @param outlier_filter Optional list specifying outliers to remove, with elements `versuch` and `SuperRank` (default `list(versuch="001", SuperRank=6)`).
#' @param epoch_range Numeric vector of length 2 specifying the start and end epochs to include (default `c(290, 299)`).
#'
#' @return Invisibly returns a list containing two ggplot objects:
#'         \describe{
#'           \item{boxplot}{The boxplot ggplot object, optionally annotated according to `mode`.}
#'           \item{heatmap}{The Dunn post-hoc significance heatmap ggplot object.}
#'         }
#'         The plots are returned invisibly to avoid automatic printing in scripts.
#'         Saves the following files to disk (prefix given by `base_filename`):
#'         \itemize{
#'           \item Excel sheets:
#'             \describe{
#'               \item{Kruskal-Wallis}{Test statistic, degrees of freedom, and p-value.}
#'               \item{Dunn Test}{Pairwise comparisons: `Group1`, `Group2`, Z-statistic, raw and Bonferroni-adjusted p-values, pairwise sample size (`n_pair`), effect size r, significance, and effect size strength.}
#'               \item{Mann-Whitney}{Optional sheet for cases with 2-group only: `Group1`, `Median1`, `Group2`, `Median2`, W-statistic, effect size r, p-value, significance, and method.}
#'             }
#'           \item Plots: PDF and SVG boxplots with optional annotations as defined by `mode`.
#'           \item Dunn post-hoc significance heatmap saved to disk.
#'         }
#'
#' @details
#' The function handles multiple experimental groups robustly:
#' - The Kruskal-Wallis test and Dunn's tests are applied across all the selected experiments.
#' - For two-group comparisons, the Mann-Whitney U test is applied explicitly in addition.
#' - Effect sizes for Dunn's test are computed as r = Z / sqrt(n_pair), where n_pair is
#'   the number of observations in the pairwise comparison.
#' - Boxplot annotations (median-of-medians, raw medians) and heatmap symbols are configured via `symbol_map` and `symbol_config`.
#'
#' @examples
#' # Example use with dummy data
#' results <- analyze_data(
#'   data = my_data,
#'   experiments = c("Small", "Medium", "Medium+Aug"),
#'   symbol_map = list("Small"="A", "Medium"="B", "Medium+Aug"="C"),
#'   symbol_config = symbol_config_df,
#'   metric = "accuracy",
#'   metric_name = "Accuracy",
#'   plot_title = "YOLO Training Performance",
#'   base_filename = "yolo_plot",
#'   mode = "annotated"
#' )
#'
#' # Access returned plots
#' boxplot_plot <- results$boxplot
#' heatmap_plot <- results$heatmap
#'
#' # Display plots
#' print(boxplot_plot)
#' print(heatmap_plot)
#'
#' @seealso
#' \code{\link[stats]{kruskal.test}}, 
#' \code{\link[FSA]{dunnTest}}, 
#' \code{\link[stats]{wilcox.test}}, 
#' \code{\link[ggplot2]{ggplot}}, 
#' \code{\link[openxlsx]{write.xlsx}}
#'
#' @import ggplot2
#' @importFrom dplyr group_by summarise mutate
#' @importFrom tidyr stack separate
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
                         base_filename = "plot", # File name without extension
                         mode = "default",
                         outlier_filter = list(versuch="001", SuperRank=6),
                         epoch_range = c(290, 299)) {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_filename)

  # Step 1: Filter data for epochs, outliers, and experiments
  subdata <- filter_data(data, experiments, epoch_range, outlier_filter)

  # Step 2: Create symbol dataset (independent of main data)
  symbols <- prepare_symbol_data(symbol_map, symbol_config, experiments)
  symbol_df <- symbols$symbol_df
  legend_symbols <- symbols$legend_symbols

  # Step 3: Compute statistics
  stats           <- compute_statistics(subdata, metric, experiments)
  kruskal_df      <- stats$kruskal_df
  mann_whitney_df <- stats$mann_whitney_df
  dunn_result     <- stats$dunn_result
  p_matrix_df     <- stats$p_matrix_df
  sig_matrix_df   <- stats$sig_matrix_df


  # Step 4: Export results to Excel with openxlsx
  export_statistics_to_excel(base_filename,
                             kruskal_df      = stats$kruskal_df,
                             dunn_result     = stats$dunn_result,
                             p_matrix_df     = stats$p_matrix_df,
                             sig_matrix_df   = stats$sig_matrix_df,
                             mann_whitney_df = stats$mann_whitney_df)

  # Step 5: Create Plot
  plot <- create_boxplot(subdata,
                         experiments,
                         symbol_df,
                         symbol_config,
                         legend_symbols,
                         metric,
                         metric_name,
                         base_filename,
                         plot_title,
                         stats,
                         mode)

  # Step 6: Dunn Post-Hoc Significance Heatmap (symbols on side and top, squares, spread with spacing)
  heatmap_plot <- create_dunn_heatmap_plot(
    sig_matrix_df  = sig_matrix_df,
    symbol_map     = symbol_map,
    symbol_config  = symbol_config,
    legend_symbols = legend_symbols,
    base_filename  = base_filename,
    plot_title     = plot_title
  )

  # Step 7: Return both plot objects (invisible)
  return(invisible(list(
    boxplot = plot,
    heatmap = heatmap_plot
  )))
}

#' Run linear and interaction models on experimental data
#'
#' This function filters, aggregates, and analyzes experimental data using
#' linear models with and without interaction terms. It produces coefficient
#' summaries, diagnostic checks, and saves results as Excel files and plots.
#'
#' @param data A data.frame containing the full dataset. Must include columns
#'   for epochs (`Epoche`), experiment identifiers (`versuch`), ranking
#'   (`SuperRank`), and the specified metric.
#' @param experiments A character vector specifying the levels of `versuch`
#'   to include and their order.
#' @param metric A character string specifying the column name of the response
#'   variable to analyze.
#' @param metric_name A human-readable name of the metric, used for plot titles.
#' @param base_filename A character string used as the base path and prefix
#'   for all generated output files.
#' @param outlier_filter A named list specifying an outlier to remove, with
#'   elements `versuch` and `SuperRank`. Set to `NULL` to disable filtering.
#'   Default is `list(versuch = "001", SuperRank = 6)`.
#' @param epoch_range A numeric vector of length 2 specifying the inclusive
#'   range of epochs to retain. Default is `c(290, 299)`.
#'
#' @details
#' The function executes the following steps:
#' \enumerate{
#'   \item Creates the output directory if it does not exist.
#'   \item Filters the dataset to the specified epoch range.
#'   \item Removes a specified outlier (if provided).
#'   \item Restricts and orders the `versuch` factor levels.
#'   \item Aggregates the data using the median across experimental factors.
#'   \item Fits two models:
#'     \itemize{
#'       \item A linear model with main effects only.
#'       \item A full interaction model with all factor interactions.
#'     }
#'   \item Extracts and classifies coefficients (main effects, interactions).
#'   \item Saves results to an Excel file, including AIC values.
#'   \item Generates and saves coefficient plots (standard and faceted).
#' }
#'
#' Several sanity checks are executed to ensure data integrity, including:
#' \itemize{
#'   \item Non-empty filtered data
#'   \item Presence of the metric column
#'   \item Absence of NA values in key variables
#'   \item Valid model matrices
#'   \item Finite AIC values
#' }
#'
#' @return An invisible list containing:
#' \describe{
#'   \item{lm}{The fitted linear model (main effects only).}
#'   \item{lmi}{The fitted linear model with interaction terms.}
#'   \item{coef}{A data.frame of model coefficients and statistics.}
#'   \item{n_rows_subdata}{Number of rows after filtering.}
#'   \item{n_rows_result}{Number of rows after aggregation.}
#' }
#'
#' @examples
#' \dontrun{
#' run_linear_model(
#'   data = df,
#'   experiments = c("001", "002", "003"),
#'   metric = "accuracy",
#'   metric_name = "Accuracy",
#'   base_filename = "results/model_"
#' )
#' }
#'
#' @export
run_linear_model <- function(data,
                             experiments,
                             metric,
                             metric_name,
                             base_filename,
                             outlier_filter = list(versuch="001", SuperRank=6),
                             epoch_range = c(290, 299)) {

  # ----------------------------
  # Step 0: Create output folder
  #         if it does not exist
  # ----------------------------
  create_parent_dir(base_filename)

  # ----------------------------
  # Step 0a: Cheack that all
  #          needed columns exist
  # ----------------------------
  required_cols <- c("Epoche", "versuch", "SuperRank",
                     "TrainTiny", "TinyAug", "Syn", "SynAug", metric)

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # ----------------------------
  # Step 1: Filter data for epochs, outliers, and experiments
  # ----------------------------
  subdata <- filter_data(data, experiments, epoch_range, outlier_filter)

  # ----------------------------
  # Step 2: Sanity checks
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
  # Step 3: Compute median per SuperRank for each Versuch
  # ----------------------------
  form <- as.formula(
    paste(metric, "~ TrainTiny + TinyAug + Syn + SynAug + version + Versuch + SuperRank")
  )
  subdata_median <- aggregate(form, data = subdata, median)

  # ----------------------------
  # Step 4: Compute ranks for the values
  # ----------------------------
  subdata_median[[metric]] <- rank(subdata_median[[metric]])

  # ----------------------------
  # Step 5: Aggregation
  # ----------------------------
  form_agg <- as.formula(
    paste(metric, "~ TrainTiny + TinyAug + Syn + SynAug + version + Versuch")
  )

  result <- aggregate(form_agg, data = subdata_median, FUN = median)

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
  coef_data_lmi <- build_coef_df(lmi_model)
  coef_data_lm  <- build_coef_df(lm_model)

  # ----------------------------
  # Step 9: File naming
  # ----------------------------
  xlsx_name_lm  <- paste0(base_filename, metric, "_lm_results.xlsx")
   pdf_name_lm  <- paste0(base_filename, metric, "_lm_coefficents.pdf")
   svg_name_lm  <- paste0(base_filename, metric, "_lm_coefficents.svg")
  xlsx_name_lmi <- paste0(base_filename, metric, "_lmi_results.xlsx")
   pdf_name_lmi <- paste0(base_filename, metric, "_lmi_coefficents.pdf")
   svg_name_lmi <- paste0(base_filename, metric, "_lmi_coefficents.svg")
  
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
       coef_data_lm,
       AIC_lm1 = AIC(lm_model),
       AIC_lmi = AIC(lmi_model)
     )),
     path = xlsx_name_lm
   )
   write_xlsx(
    list("linear_model_results" = cbind(
      coef_data_lmi,
      AIC_lm1 = AIC(lm_model),
      AIC_lmi = AIC(lmi_model)
    )),
    path = xlsx_name_lmi
  )

  # ----------------------------
  # Step 12: Plot
  # ----------------------------

  # LM plot
  p_lm <- plot_coefficients(
    coef_data_lm,
    metric_name,
    paste("Coefficients of the linear model:", metric_name)
  )

  # LMI plot
  p_lmi <- plot_coefficients(
    coef_data_lmi,
    metric_name,
    paste("Coefficients of the linear interaction model:", metric_name)
  )

  ggsave(
    filename = pdf_name_lm,
    plot = p_lm,
    height = 5,
    width = 5
  )
  ggsave(
    filename = svg_name_lm,
    plot = p_lm,
    height = 5,
    width = 5
  )
  ggsave(
    filename = pdf_name_lmi,
    plot = p_lmi,
    height = 5,
    width = 5
  )
  ggsave(
    filename = svg_name_lmi,
    plot = p_lmi,
    height = 5,
    width = 5
  )

  pdf_name_facet <- paste0(base_filename, metric, "_coefficents_FACET.pdf")

  p_facet <- ggplot(coef_data_lmi, aes(
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
    p_lm = p_lm,
    p_lmi = p_lmi,
    coef_lm = coef_data_lm,
    coef_lmi = coef_data_lmi,
    n_rows_subdata = nrow(subdata),
    n_rows_result = nrow(result)
  ))
}

#' Plot Training Time vs. Number of Images
#'
#' @description
#' This function visualizes the relationship between the number of training
#' images and the corresponding training time. It fits a linear regression model,
#' overlays the regression line, and annotates the plot with the regression
#' formula and an estimated training efficiency.
#'
#' @param training_times A numeric vector containing training times (in hours).
#' @param image_numbers A numeric vector containing the corresponding number
#'   of training images.
#' @param output_dir A character string specifying the directory where the
#'   output files will be saved.
#'
#' @return Invisibly returns the fitted linear model (`lm` object).
#'
#' @details
#' \itemize{
#'   \item Creates the output directory if it does not exist.
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
#' training_times <- c(10, 20, 35, 50)
#' image_numbers <- c(5000, 10000, 15000, 20000)
#' plot_training_times(training_times, image_numbers, tempdir())
#'
#' @import ggplot2
#' @export
plot_training_times <- function(training_times, image_numbers, output_dir)
{
  if (length(training_times) != length(image_numbers)) {
    stop("training_times and image_numbers must have the same length")
  }

  # Build output file names
  output_file_name_pdf <- paste0(output_dir, "/NumberOfImages_TraingTime.pdf")
  output_file_name_svg <- paste0(output_dir, "/NumberOfImages_TraingTime.svg")

  # Ensure the parent directory exists
  create_parent_dir(output_file_name_pdf)

  # Create a data frame for ggplot
  df <- data.frame(
    Images = image_numbers,
    Time_h = training_times
  )

  # Fit linear model for regression line
  fit <- lm(Time_h ~ Images, data = df)
  formula_text <- paste0("f(x) = ", round(coef(fit)[2], 5), " x + ", round(coef(fit)[1], 5))

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
  time_per_10000 <- coef(fit)[1] + coef(fit)[2] * 10000
  efficiency_text <- paste0("ca. ", round(time_per_10000, 1), " h / 10,000 images")

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
    ) +
   ggtitle("Dependency: Training time vs training data set size")

  # Save plot as PDF and SVG
  ggsave(filename = output_file_name_pdf, plot = p, height = 5, width = 5, units = "in") # Set the "default" units explicitly. Quite weired that inchi-binchies are the default.
  ggsave(filename = output_file_name_svg, plot = p, height = 5, width = 5, units = "in")

  # Build output file names
  output_file_name_pdf <- paste0(output_dir, "/NumberOfImages_TraingTime_NoTitle.pdf")
  output_file_name_svg <- paste0(output_dir, "/NumberOfImages_TraingTime_NoTitle.svg")

  p <- p + theme(plot.title = element_blank())
  ggsave(filename = output_file_name_pdf, plot = p, height = 5, width = 5, units = "in")
  ggsave(filename = output_file_name_svg, plot = p, height = 5, width = 5, units = "in")

  return(invisible(fit))
}

#' Plot experiment metric over epochs with optional outlier removal
#'
#' This function filters a dataset for a specific experiment, optionally removes
#' a predefined outlier, and visualizes the evolution of a selected metric over
#' epochs. The resulting plot is faceted by `SuperRank` and saved to a file.
#'
#' @param data A data.frame containing the dataset. Must include at least
#'   `versuch`, `Epoche`, `SuperRank`, and the specified metric column.
#' @param experiment_id A single experiment identifier used to subset `versuch`.
#' @param metric A character string specifying the column name of the variable
#'   to plot on the y-axis.
#' @param metric_name A human-readable name of the metric, used for axis labeling.
#' @param title A character string defining the plot title.
#' @param filename A character string specifying the output file path for the plot.
#' @param outlier_filter A named list with elements `versuch` and `SuperRank`
#'   specifying a single observation to remove. If `NULL`, no outlier removal is applied.
#'   Default is `list(versuch = "001", SuperRank = 6)`.
#'
#' @details
#' The function executes the following steps:
#' \enumerate{
#'   \item Filters the dataset for the selected experiment (`experiment_id`).
#'   \item Optionally removes a predefined outlier based on `versuch` and `SuperRank`.
#'   \item Checks that data remains after filtering.
#'   \item Creates a scatter plot of the selected metric over `Epoche`,
#'         faceted by `SuperRank`.
#'   \item Ensures that the output directory exists.
#'   \item Saves the plot to a file using `ggsave()`.
#' }
#'
#' @return Invisibly returns NULL. The function is used for its side effect
#'   of generating and saving a plot.
#'
#' @examples
#' \dontrun{
#' plot_experiment(
#'   data = df,
#'   experiment_id = "001",
#'   metric = "accuracy",
#'   metric_name = "Accuracy",
#'   title = "Experiment 001 - Accuracy over Time",
#'   filename = "results/exp001_accuracy.pdf"
#' )
#' }
#'
#' @export
plot_experiment <- function(data,
                            experiment_id, 
                            metric,
                            metric_name,
                            title,
                            filename,
                            outlier_filter = list(versuch="001", SuperRank=6)) {

  # ----------------------------
  # Step 1: Filter data for selected experiment
  # ----------------------------
  subdata <- subset(data, versuch == experiment_id)

  # Optional: remove predefined outlier (if provided)
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(
      subdata$versuch == outlier_filter$versuch & 
        subdata$SuperRank == outlier_filter$SuperRank
    ), ]
  }

  # Safety check: ensure data is not empty after filtering
  if (nrow(subdata) == 0) {
    stop("No data available after filtering for experiment_id and outlier removal")
  }

  # ----------------------------
  # Step 2: Create plot
  # ----------------------------
  p <- ggplot(subdata) +
    geom_point(aes(x = Epoche, y = .data[[metric]]), size = 0.1) +
    facet_wrap(~ SuperRank, nrow = 3) +
    ggtitle(title) +
    labs(
      x = "Epoch",
      y = metric_name
    ) +
    theme(
      plot.title = element_text(color = "black", size = 9)
    )

  # ----------------------------
  # Step 3: Ensure output directory exists
  # ----------------------------
  create_parent_dir(filename)

  # ----------------------------
  # Step 4: Save plot to file
  # ----------------------------
  ggsave(
    filename = filename,
    plot = p,
    height = 5,
    width = 5
  )
}

#' Plot histograms and test normality (Shapiro-Wilk) per experiment
#'
#' This function filters a dataset by epoch range and experiment identifiers,
#' applies a Shapiro-Wilk normality tests per experiment, and visualizes the
#' distribution of a specified metric with faceted histograms. The results are
#' saved to an Excel file and a PDF plot.
#'
#' @param data A data.frame containing the dataset. Must include columns
#'   `Epoche`, `versuch`, and the specified metric.
#' @param metric A character string specifying the column name of the variable
#'   to analyze.
#' @param metric_name A human-readable name of the metric, used for plot labels.
#' @param base_filename A character string used as the base path and prefix
#'   for output files.
#' @param versuche A character vector specifying which `versuch` values
#'   (experiments) to include.
#' @param epoch_range A numeric vector of length 2 specifying the inclusive
#'   range of epochs to retain. Default is `c(290, 299)`.
#' @param axis_text_size Numeric value controlling the size of x-axis text
#'   in the histogram plot. Default is 5.
#'
#' @details
#' The function executes the following steps:
#' \enumerate{
#'   \item Creates the output directory if it does not exist.
#'   \item Filters the dataset by epoch range and selected experiments.
#'   \item Applies a Shapiro-Wilk normality test for each experiment
#'         (`versuch`), if at least 3 observations are available.
#'   \item Classifies each experiment as normally distributed ("Yes"/"No")
#'         using a significance level of 0.05.
#'   \item Saves the test results to an Excel file.
#'   \item Generates a faceted histogram plot of the metric and saves it as PDF.
#' }
#'
#' Experiments with fewer than 3 observations are assigned `NA` for both
#' the test statistic and p-value.
#'
#' @return A data.frame containing the Shapiro-Wilk test results with columns:
#' \describe{
#'   \item{versuch}{Experiment identifier}
#'   \item{W}{Shapiro-Wilk test statistic}
#'   \item{p_value}{p-value of the test}
#'   \item{normal}{"Yes" if p > 0.05, otherwise "No"}
#' }
#'
#' @examples
#' \dontrun{
#' plot_histogram_normality(
#'   data = df,
#'   metric = "accuracy",
#'   metric_name = "Accuracy",
#'   base_filename = "results/normality",
#'   versuche = c("001", "002", "003")
#' )
#' }
#'
#' @export
plot_histogram_normality <- function(
    data,
    metric,
    metric_name,
    base_filename,
    versuche,
    epoch_range = c(290, 299),
    axis_text_size = 5
) {

  # ----------------------------
  # Step 0: Input validation
  # ----------------------------
  required_cols <- c("Epoche", "versuch", metric)
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Ensure output directory exists
  create_parent_dir(base_filename)

  # ----------------------------
  # Step 1: Filter data (epochs + selected experiments)
  # ----------------------------
  subdata_all <- subset(
    data,
    Epoche >= epoch_range[1] &
      Epoche <= epoch_range[2] &
      versuch %in% versuche
  )

  # Safety check: ensure data remains after filtering
  if (nrow(subdata_all) == 0) {
    stop("No data left after filtering")
  }

  # ----------------------------
  # Step 2: Shapiro-Wilk normality test per experiment
  # ----------------------------
  shapiro_df <- do.call(
    rbind,
    lapply(split(subdata_all[[metric]], subdata_all$versuch), function(x) {

      # Shapiro-Wilk requires at least 3 observations
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

  # Attach experiment labels back to results
  shapiro_df$versuch <- rownames(shapiro_df)
  rownames(shapiro_df) <- NULL

  # ----------------------------
  # Step 3: Classify normality (alpha = 0.05)
  # ----------------------------
  alpha <- 0.05
  shapiro_df$normal <- ifelse(shapiro_df$p_value > alpha, "Yes", "No")

  # ----------------------------
  # Step 4: Export results to Excel
  # ----------------------------
  write_xlsx(
    list("Shapiro-Wilk" = shapiro_df),
    path = paste0(base_filename, "_Shapiro_Wilk_", metric, ".xlsx")
  )

  # ----------------------------
  # Step 5: Histogram visualization
  # ----------------------------
  p <- ggplot(subdata_all, aes(x = .data[[metric]])) +
    geom_histogram(binwidth = 0.01) +
    facet_wrap(~versuch) +
    ggtitle(
      paste0(
        "Histogram of ", metric_name,
        "-values for all experiments (epoch range: ",
        epoch_range[1], "-", epoch_range[2], ")"
      )
    ) +
    labs(
      x = metric_name,
      y = "Count"
    ) +
    theme(
      plot.title = element_text(color = "black", size = 9),
      axis.text.x = element_text(size = axis_text_size)
    )

  # Save plot to file
  ggsave(
    filename = paste0(base_filename, "_histogram_", metric, ".pdf"),
    plot = p,
    height = 5,
    width = 5
  )

  # ----------------------------
  # Step 6: Return results
  # ----------------------------
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
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("001", "003", "004", "012", "006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010", "020", "011")
  ),
  list(
    name        = "augmented",
    title       = "Original and classical augmented images: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("001", "003", "004", "012")
  ),
  list(
    name        = "size",
    title       = "Training data set sizes 2: ",
    file_prefix = "FigureA_",
    lm_prefix   = "",
    experiments = c("006", "001","003")
  ),
  list(
    name        = "combinations",
    title       = "All combinations of augmentations: ",
    file_prefix = "FigureD_",
    lm_prefix   = "LinearModel_",
    experiments = c("006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")
  ),
  list(
    name        = "small_augmented",
    title       = "Augmentations of small datasets: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("006", "014", "007")
  ),
  list(
    name        = "small_syn_augmented",
    title       = "Synthetic augmentations of small datasets: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("006", "005", "008")
  ),
  list(
    name        = "small_combinations",
    title       = "Classical and synthetic augmentations of small datasets: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("006", "007", "008", "013", "011")
  ),
  list(
    name        = "size_to_lower",
    title       = "Training data set sizes: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("003", "001","006")
  ),
  list(
    name        = "non_annotated_removed",
    title       = "Removing images without glomeruli: ",
    file_prefix = "FigureB_",
    lm_prefix   = "",
    experiments = c("003", "004")
  ),
  list(
    name        = "add_augmented1",
    title       = "Conventional data augmentation 1: ",
    file_prefix = "FigureC_",
    lm_prefix   = "",
    experiments = c("004", "012")
  ),
  list(
    name        = "add_augmented2",
    title       = "Conventional data augmentation 2: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("006", "014", "005", "015")
  ),
  list(
    name        = "add_augmented3",
    title       = "Conventional data augmentation 3: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("006", "014", "005", "015", "004", "012")
  ),
  list(
    name        = "combinations_only",
    title       = "Only combinations of augmentations: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")
  ),
  list(
    name        = "Orignial_and_augmentation",
    title       = "Original data and augmentations: ",
    file_prefix = "",
    lm_prefix   = "",
    experiments = c("012", "004", "003", "001", "006")
  ),
  list(
    name        = "TrainSmall_vs_Augmented",
    title       = "TrainSmall vs Augmented: ",
    file_prefix = "FigureSupp_",
    lm_prefix   = "",
    experiments = c("001", "019")
  )
)

# Make sure that all jobs have all required fields
validate_jobs(jobs)

# Make vectors for training time and number of images, hard encoded, come from outside.
# Would be better to have it in its own file.
training_times <- c(9.544, 113.401, 21.871, 4.854, 3.099, 2.856, 4.936, 11.607, 11.489, 12.316, 100.733, 5.162, 2.808, 9.625, 5.067, 9.924, 9.77, 11.583, 9.911)
image_numbers  <- c(3410, 53908, 8855, 800, 15, 75, 815, 4000, 4015, 4075, 44275, 875, 60, 3200, 860, 3260, 3215, 4060, 3275)

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
plot_training_times(training_times, image_numbers, output_dir)

#########################################################################################################################

library(cowplot)

remove_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank()
)

no_title <- theme(
  plot.title = element_blank(),
)

no_xaxis <- theme(
  axis.title.x = element_blank(),
)

no_yaxis <- theme(
  axis.title.y = element_blank()
)

no_legend <- theme(
  legend.position = "none",
)

no_legend_title <- no_title + no_legend + no_xaxis + no_yaxis

legend_only <- theme(
  legend.position = "right",
  legend.justification = "top",
  legend.box.just = "top",
  plot.title = element_blank()
)

no_title <- theme(
  plot.title = element_blank()
)

for (metric in names(metrics)) {
  for(plot_type in names(plot_types)) {
    plots_to_assemble <- list()

    for(job in jobs) {
      metric_name     <- metrics[[metric]]
      plot_title      <- paste0(job$title, metrics[[metric]], " of the last 10 epochs")
      base_dir        <- paste0(output_dir, "/", metric, "/", plot_types[[plot_type]])
      base_filename   <- paste0(base_dir, job$file_prefix, file_safe_name(plot_title))

      if(metric == "mAP_50" && job$name == "all") {
        plots <- analyze_data(
          data1df,
          job$experiments,
          symbol_map_experiments,
          symbol_config,
          metric,
          metric_name,
          plot_title,
          base_filename,
          plot_type,
          NULL
        )
      }
      else {
        plots <- analyze_data(
          data1df,
          job$experiments,
          symbol_map_experiments,
          symbol_config,
          metric,
          metric_name,
          plot_title,
          base_filename,
          plot_type
        )
      }
      plots_to_assemble[[job$name]] <- plots
    }

    legend_grob <- get_legend(
      plots_to_assemble[["all"]]$boxplot + legend_only
    )

    p1_clean    <- plots_to_assemble[["size_to_lower"]]$boxplot + no_legend_title
    p2_clean    <- plots_to_assemble[["non_annotated_removed"]]$boxplot + remove_y + no_legend_title
    p3_clean    <- plots_to_assemble[["add_augmented1"]]$boxplot + remove_y + no_legend_title
    p4_clean    <- plots_to_assemble[["combinations"]]$boxplot + no_title + no_legend + no_yaxis

    p1_clean    <- shrink_first_point_layer(p1_clean)
    p2_clean    <- shrink_first_point_layer(p2_clean)
    p3_clean    <- shrink_first_point_layer(p3_clean)
    p4_clean    <- shrink_first_point_layer(p4_clean)

    top_row <- plot_grid(
      p1_clean, p2_clean, p3_clean, legend_grob,
      labels = c("A", "B", "C", ""),
      ncol = 4,
      align = "v",           # Align vertically
      axis = "l"             # Align left edges of the grobs
    )

    final_plot <- plot_grid(
      top_row,
      p4_clean,
      labels = c("", "D"),
      ncol = 1
    )

    final_plot <- ggdraw() +
      draw_plot(final_plot, x = 0.05, y = 0.0, width = 0.95, height = 1.0) +
      draw_label(metric_name, x = 0.03, y = 0.5, angle = 90, vjust = 0.5)

    heatmap    <- plots_to_assemble[["combinations"]]$heatmap
    heatmap    <- heatmap + no_title + no_legend

    final_plot <- plot_grid(
      final_plot,
      heatmap,
      labels = c("", "E"),
      ncol = 2
    )

    pdf_file <- paste0(base_dir, "/FigureBoxPlots_", metric, ".pdf")
    svg_file <- paste0(base_dir, "/FigureBoxPlots_", metric, ".svg")

    ggsave(
      filename = pdf_file,
      plot = final_plot,
      height = 5,
      width = 10
    )
    ggsave(
      filename = svg_file,
      plot = final_plot,
      height = 5,
      width = 10
    )
  }
}

#########################################################################################################################

for(job in jobs) {
  if(job$lm_prefix == ""){
    next
  }

  # Multiple linear regression for the models for all metrics to determine what has the most effect
  experiments <- job$experiments

  base_filename <- paste0(output_dir, "/LinearModels/", job$lm_prefix)

  plots_to_assemble <- list()
  coef_to_assemble  <- list()

  for (metric in names(metrics)) {
    results <- run_linear_model(data1df, experiments, metric, metrics[[metric]], base_filename)

    plots_to_assemble[[metric]] <- results$p_lm
    coef_to_assemble[[metric]]  <- results$coef_lm
  }

  selected_metrics <- c("mAP_50", "mAP_95")
  coef_selected    <- coef_to_assemble[selected_metrics]

  y_min <- min(sapply(coef_selected, function(df)
    min(df$Coefficients - df$`Std. Error`)
  ))

  y_max <- max(sapply(coef_selected, function(df)
    max(df$Coefficients + df$`Std. Error`)
  ))

  padding <- 0.05 * (y_max - y_min)
  common_limits <- c(y_min - padding, y_max + padding)

  plot_50 = plots_to_assemble[["mAP_50"]] + no_title + no_xaxis + coord_cartesian(ylim = common_limits)
  plot_95 = plots_to_assemble[["mAP_95"]] + no_title + no_xaxis + coord_cartesian(ylim = common_limits)

  lm_plot <- plot_grid(
    plot_50, plot_95,
    labels = c("A", "B"),
    ncol = 2,
    align = "v",           # Align vertically
    axis = "l"             # Align left edges of the plots
  )

  x_label <- "Training datasets"

  x_lab <- ggdraw() +
    draw_label(
      x_label,
      x = 0.5,
      hjust = 0.5,
      size = 10
    )

  lm_plot <- plot_grid(
    lm_plot,
    x_lab,
    ncol = 1,
    rel_heights = c(1, 0.08)
  )

  pdf_file <- paste0(base_filename, "Figure.pdf")
  svg_file <- paste0(base_filename, "Figure.svg")

  ggsave(
    filename = pdf_file,
    plot = lm_plot,
    height = 4,
    width = 8
  )
  ggsave(
    filename = svg_file,
    plot = lm_plot,
    height = 4,
    width = 8
  )
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
