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
  symbol_df$versuch <- factor(symbol_df$versuch, levels = experiments)

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

  # Step 10: Kruskal-Wallis test across versuch
  kruskal_result <- kruskal.test(form, data = subdata)
  kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                           parameter = kruskal_result$parameter,
                           p.value = kruskal_result$p.value,
                           method = kruskal_result$method,
                           data.name = kruskal_result$data.name)

  # Step 11: Dunn's pairwise post-hoc test with Bonferroni correction
  dunn_result <- dunnTest(form, data=subdata, method="bonferroni")

  # Step 12: Calculate effect size r and add significance
  # n is the number of observation, one observation from each epoch, per experiment per repetitions
  n <- nrow(model.frame(form, data = subdata))
  dunn_result$res$r <- dunn_result$res$Z / sqrt(n)
  alpha <- 0.05
  dunn_result$res$significant <- ifelse(dunn_result$res$P.adj < alpha, "Ja", "Nein")

  # Step 13: Add strength of effect size
  dunn_result$res <- dunn_result$res %>%
    mutate(effect_size_strength = case_when(
      abs(r) < 0.1 ~ "vernachlässigbar",
      abs(r) < 0.3 ~ "klein",
      abs(r) < 0.5 ~ "mittel",
      TRUE         ~ "groß"
    ))

  # Step 14: Add sample size column and reorder columns
  dunn_result$res$n <- n
  dunn_result$res <- dunn_result$res[, c("Comparison", "Z", "P.unadj", "P.adj", "n", "r", "significant", "effect_size_strength")]

  # Step 15: Export Kruskal-Wallis and Dunn results to XLSX
  data_to_write <- list("Kruskal-Wallis" = kruskal_df, "Dunn Test" = dunn_result$res)
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
      values = setNames(symbol_config$shape, symbol_config$symbol)
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

  # Step 18: Save plot as PDF
  ggsave(
    filename = paste0(base_file_name, ".pdf"),
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
    paste(metric, "~ OG + OG_aug + SG + SG_aug + version + Versuch")
  )

  result <- aggregate(form_agg, data = subdata, FUN = median)

  # Check aggregation integrity
  expected_groups <- length(unique(subdata$versuch))
  actual_groups <- nrow(result)

  if (actual_groups == 0) stop("Aggregation produced empty result")

  # ----------------------------
  # Step 6: Model matrices (consistency check)
  # ----------------------------
  lm_formula  <- as.formula(paste(metric, "~ OG + OG_aug + SG + SG_aug"))
  lmi_formula <- as.formula(paste(metric, "~ OG * OG_aug * SG * SG_aug"))

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

plot_training_times <- function(traing_times, image_numbers)
{
  pdf("Abb_Bildanzahl_Trainingszeit.pdf",height=5, width=5)

  # Ändere die Grafikparameter
  par(cex.axis = 0.8, cex.lab =0.8, cex.main = 0.8, cex.sub = 0.5, las = 1)

  # Plotte Trainingszeit gegen Bildanzahl
  plot(image_numbers,
       traing_times,
       ylab = "Trainingszeit in h",
       xlab = "Anzahl der Trainingsbilder",
       main = "Abhängigkeit der Trainingszeit von der Trainingsdatensatzgröße",
       pch = 1,
       col = 2,
       cex = 1)

  # Füge eine Regressionslinie hinzu
  fit <- lm(traing_times ~ image_numbers)
  abline(fit)

  # Zeige die Formel der Regressionsgerade an
  formula <- paste("f(x) =", round(coef(fit)[2], digits = 3), "x +", round(coef(fit)[1], digits = 3))
  mtext(formula,
        side = 3,
        line = -12,
        cex = 0.8)

  dev.off()
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

# Load and prepare main data
exceldata <- read_excel("Design2.xlsx")
Design <- data.frame(exceldata)

data <- read.csv ("allresults_header_tab_final_v002.txt",sep="\t")
data1 <- data.frame(data)
data1$versuch <- substr(data1$filename,19,21)

data1$Versuch <- as.numeric(data1$versuch)

tmp <- merge(data1, Design, by='Versuch')

data1 <- tmp

for (i in 1:dim(data1)[1]){
  ver <- substr(data1$filename[i],22,30)
  vers <- strsplit(ver,split="/")
  a<- vers
  s <- a[[1]][1]
  if (s ==""){
    s="1"
  }
  data1$version[i] <- strtoi(s)
}
for (i in 1:dim(data1)[1]){
  ver <- data1$Epoch[i]
  vers <- strsplit(ver,split="/")
  a<- vers
  s <- a[[1]][1]
  
  data1$Epoche[i] <- strtoi(s)
}

data1_f <- data1 %>%
  group_by(versuch,version) %>%
  mutate(MaxValue = max(Epoche))

subset_ff <- subset(data1_f,MaxValue > 290)

data1_s <- subset_ff%>%
  group_by(versuch) %>%
  mutate(SuperRank = dense_rank(version))

data1df <- data1_s

#########################################################################################################################

# Mapping of versuch to symbols
symbol_map_experiments <- list(
  "001" = c("B6"),
  "003" = c("DS"),
  "004" = c("DS_b"),
  "012" = c("DS_b", "DS_b_aug"),
  "006" = c("OG"),
  "014" = c("OG_aug"),
  "005" = c("SG"),
  "015" = c("SG_aug"),
  "007" = c("OG","OG_aug"),
  "016" = c("OG_aug","SG"),
  "009" = c("SG","SG_aug"),
  "008" = c("OG","SG"),
  "017" = c("OG_aug","SG_aug"),
  "018" = c("OG","SG_aug"),
  "013" = c("OG","OG_aug","SG"),
  "019" = c("OG_aug","SG","SG_aug"),
  "010" = c("OG","SG","SG_aug"),
  "020" = c("OG","OG_aug","SG_aug"),
  "011" = c("OG","OG_aug","SG","SG_aug")
)

# Define y-positions for the symbols and their shapes
symbol_config <- data.frame(
  symbol = c("B6", "DS", "DS_b", "DS_b_aug", "OG", "OG_aug", "SG", "SG_aug"),
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
    title       = "Training data set sizes: ",
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
plot_training_times(traing_times, image_numbers)

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
