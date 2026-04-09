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
#' @param base_file_name Character, the base file name (without extension) to save the heatmap plots as PDF and SVG.
#' @param plot_title Character, the title of the heatmap plot. Default is `"Heatmap"`.
#'
#' @return Invisibly returns a `ggplot` object representing the Dunn significance heatmap with symbols.
#'   The function also saves the heatmap to PDF and SVG files using `base_file_name`.
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
#'   base_file_name = "example_heatmap"
#' )
#' print(heatmap_plot)
#'
#' @export
create_dunn_heatmap_plot <- function(
    sig_matrix_df,
    symbol_map,
    symbol_config,
    legend_symbols,
    base_file_name,
    plot_title = "Heatmap"
) {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_file_name)

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
              color = "white") +
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
      axis.text.x = element_text(angle = 45, hjust = 1),
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
    filename = paste0(base_file_name, "_dunn_heatmap.pdf"),
    plot = heatmap_plot,
    width = 6,
    height = 5
  )
  ggsave(
    filename = paste0(base_file_name, "_dunn_heatmap.svg"),
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
#' @param base_file_name Character, base file name (without extension) to save the plot as PDF and SVG.
#' @param plot_title Character, title of the plot. Defaults to `"Plot Title"`.
#' @param mode Character, plotting mode. Options:
#'   - `"default"`: only boxplot and symbols
#'   - `"annotated"`: overlays median-of-medians points (blue) with numeric labels and red raw medians
#'   - `"red_raw_medians"`: overlays only red raw median points
#'
#' @return A `ggplot` object representing the boxplot. Also saves the plot as PDF and SVG
#'   using `base_file_name`.
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
#'   base_file_name = "my_boxplot",
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
                           base_file_name,
                           plot_title = "Plot Title",
                           mode = "default") {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_file_name)

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
      axis.text.x = element_text(size = 6),
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

  # Step 7: Save plot as PDF and SVG
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

  # Step 3: Optional Mann-Whitney U test for exactly 2 groups
  group_count <- length(unique(subdata$versuch))
  if(group_count == 2) {

    groups <- levels(subdata$versuch)
    x <- subdata[[metric]][subdata$versuch == groups[1]]
    y <- subdata[[metric]][subdata$versuch == groups[2]]

    # Mann-Whitney U test (implemented as Wilcoxon rank-sum test in R)
    # Note: This is a two-sided test by default (alternative = "two.sided")
    mw_result <- wilcox.test(x, y, exact = FALSE)

    # Effect size r = Z / sqrt(N)
    # We approximate Z from the U statistic using the normal approximation.
    # This corresponds to the same two-sided hypothesis test as reported by wilcox.test.
    W <- as.numeric(mw_result$statistic)
    n <- length(x) + length(y)

    # Convert W to U (same here) and compute expected value and variance under H0
    U <- W
    mu_U <- length(x)*length(y)/2
    sigma_U <- sqrt(length(x)*length(y)*(length(x)+length(y)+1)/12)

    # Z-score (signed; direction depends on group ordering)
    Z <- (U - mu_U)/sigma_U

    # Effect size (note: magnitude is typically interpreted, sign depends on group order)
    # Interpretation: |r| indicates effect size magnitude; sign depends on group order
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

  # Step 4: Dunn's pairwise post-hoc test with Bonferroni correction and effect sizes
  dunn_result <- dunnTest(form, data = subdata, method = "bonferroni")

  dunn_result$res <- dunn_result$res %>%
    # Split 'Comparison' into Group1 and Group2 first
    tidyr::separate(Comparison, into = c("Group1", "Group2"), sep = " - ") %>%
    # Compute pairwise sample size, effect size r, significance, and strength
    rowwise() %>%
    mutate(
      n_pair = sum(subdata$versuch %in% c(Group1, Group2)),
      r = Z / sqrt(n_pair),
      significant = ifelse(P.adj < alpha, "Yes", "No"),
      effect_size_strength = case_when(
        abs(r) < 0.1 ~ "negligible",
        abs(r) < 0.3 ~ "small",
        abs(r) < 0.5 ~ "medium",
        TRUE         ~ "big"
      )
    ) %>%
    ungroup() %>%
    # Reorder columns for clarity
    select(Group1, Group2, Z, P.unadj, P.adj, n_pair, r, significant, effect_size_strength)

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
#' @param base_file_name character. The base path and file name for the Excel
#'   workbook, without the ".xlsx" extension. The workbook will be saved as
#'   `paste0(base_file_name, ".xlsx")`.
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
#'   base_file_name = "results/statistics_summary",
#'   kruskal_df      = stats$kruskal_df,
#'   dunn_result     = stats$dunn_result,
#'   p_matrix_df     = stats$p_matrix_df,
#'   sig_matrix_df   = stats$sig_matrix_df,
#'   mann_whitney_df = stats$mann_whitney_df
#' )
#' }
#'
#' @export
export_statistics_to_excel <- function(base_file_name,
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
  openxlsx::saveWorkbook(wb, paste0(base_file_name, ".xlsx"), overwrite = TRUE)
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
#' @param base_file_name Base file name (without extension) for saved plots and Excel sheets (default `"plot"`).
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
#'         Saves the following files to disk (prefix given by `base_file_name`):
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
#'   base_file_name = "yolo_plot",
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
                         base_file_name = "plot", # File name without extension
                         mode = "default",
                         outlier_filter = list(versuch="001", SuperRank=6),
                         epoch_range = c(290, 299)) {

  # Step 0: Create the parent dir of the output file if it does not exsist
  create_parent_dir(base_file_name)

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
  export_statistics_to_excel(base_file_name,
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
                         base_file_name,
                         plot_title,
                         mode)

  # Step 6: Dunn Post-Hoc Significance Heatmap (symbols on side and top, squares, spread with spacing)
  heatmap_plot <- create_dunn_heatmap_plot(
    sig_matrix_df  = sig_matrix_df,
    symbol_map     = symbol_map,
    symbol_config  = symbol_config,
    legend_symbols = legend_symbols,
    base_file_name = base_file_name,
    plot_title     = plot_title
  )

  # Step 7: Return both plot objects (invisible)
  return(invisible(list(
    boxplot = plot,
    heatmap = heatmap_plot
  )))
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
#  subdata <- subdata[subdata$versuch %in% experiments, ] # This is used here, otherwise we could just use filter_data
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

library(cowplot)

remove_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y  = element_blank(),
  axis.ticks.y = element_blank()
)

no_legend_title <- theme(
  legend.position = "none",
  plot.title = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

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
      base_file_name  <- paste0(base_dir, file_safe_name(plot_title))
      
      if(metric == "mAP_50" && job$name == "all") {
        plots <- analyze_data(
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
        plots <- analyze_data(
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
      plots_to_assemble[[job$name]] <- plots
    }
    
    
    
  #  for(plot in names(figure_plots)) {
  #  }
    legend_grob <- get_legend(
      plots_to_assemble[["all"]]$boxplot + legend_only
    )

    p1_clean    <- plots_to_assemble[["size_to_lower"]]$boxplot + no_legend_title
    p2_clean    <- plots_to_assemble[["non_annotated_removed"]]$boxplot + remove_y + no_legend_title
    p3_clean    <- plots_to_assemble[["add_augmented1"]]$boxplot + remove_y + no_legend_title
    p4_clean    <- plots_to_assemble[["combinations"]]$boxplot + no_legend_title

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
      draw_plot(final_plot, x = 0.05, y = 0.05, width = 0.95, height = 0.95) +
      draw_label("Experiment", x = 0.5, y = 0.03, angle = 0, vjust = 0.5) +
      draw_label(metric_name, x = 0.03, y = 0.5, angle = 90, vjust = 0.5)

    pdf_file <- paste0(base_dir, "/FigureBoxPlots_", metric, ".pdf")
    svg_file <- paste0(base_dir, "/FigureBoxPlots_", metric, ".svg")

    ggsave(
      filename = pdf_file,
      plot = final_plot,
      height = 5,
      width = 5
    )
    ggsave(
      filename = svg_file,
      plot = final_plot,
      height = 5,
      width = 5
    )
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
