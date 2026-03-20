library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(FSA)

rm(list = ls()) 

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
##############################################################################################

#subdata <- subset(data1, ((Versuch == 3) | (Versuch == 4)) & (Epoche > 290)  )

subdata <- subset(data1,  (Epoche > 298) & (Epoche < 300) )

subdata$versuch <- factor(subdata$versuch , levels=c("012", "010", "011", "001","004",
                                                     "003", "013","005","006","007","008","009"))
ggplot(subdata, aes(x = versuch, y= mAP_95) ) +
  geom_boxplot()


data1_f <- data1 %>%
  group_by(versuch,version) %>%
  mutate(MaxValue = max(Epoche))

subset_ff <- subset(data1_f,MaxValue > 290)

data1_s <- subset_ff%>%
  group_by(versuch) %>%
  mutate(SuperRank = dense_rank(version))

data1df <- data1_s

################################################################################################
################################################################################################
###############################################################################################

analyze_data <- function(data,
                         versuch_levels,
                         symbol_map,
                         symbol_config,
                         metric,
                         plot_title = "Plot Title",
                         base_file_name = "plot", # File name without extension
                         mode = "default",
                         outlier_filter = list(versuch="001", SuperRank=6),
                         epoch_range = c(290, 299)) {

  # Step 1: Filter data to include only the last 10 epochs
  subdata <- subset(data, Epoche >= epoch_range[1] & Epoche <= epoch_range[2])

  # Step 2: Remove outliers (specific versuch and SuperRank)
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(subdata$versuch == outlier_filter$versuch & 
                         subdata$SuperRank == outlier_filter$SuperRank), ]
  }

  # Step 3: Keep only valid versuch levels and factorize
  subdata <- subdata[subdata$versuch %in% versuch_levels, ]
  subdata$versuch <- factor(subdata$versuch , levels=versuch_levels)

  # Step 4: Ensure all versuch_levels have a corresponding symbol mapping
  missing <- setdiff(versuch_levels, names(symbol_map))

  if (length(missing) > 0) {
    stop(paste("Missing symbol_map entries for:", paste(missing, collapse=", ")))
  }

  # Step 5: Create symbol dataset (independent of main data)
  symbol_df <- stack(symbol_map)
  colnames(symbol_df) <- c("symbol", "versuch")
  # Keep only relevant versuch
  symbol_df <- symbol_df[symbol_df$versuch %in% versuch_levels, ]
  # Join with symbol_config (safe: no duplication issue here)
  symbol_df <- merge(symbol_df, symbol_config, by = "symbol", all.x = TRUE)
  # Ensure factor levels match plot
  symbol_df$versuch <- factor(symbol_df$versuch, levels = versuch_levels)

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
  subdata_median$versuch <- factor(subdata_median$versuch, levels = versuch_levels)
  subdata_median_median$versuch <- factor(subdata_median_median$versuch, levels = versuch_levels)

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
    labs(shape = "") +
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
  if (mode != "default") {

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

#########################################################################################################################

# Gesamtübersicht: 19 Experimente (mAP_95), 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

versuch_levels <- c("001", "003", "004", "012", "006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")

base_file_name <- "Abb_Gesamtübersicht_19_mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Gesamtübersicht (alle 19 Experimente): mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name)

# Gesamtübersicht: 19 Experimente (mAP_50), 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

base_file_name <- "Abb_Gesamtübersicht_19_mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Gesamtübersicht (alle 19 Experimente): mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, NULL) # Outlier filter here is set to NULL

#########################################################################################################################

# 4 Experimente (großer Datensatz, mAP_95, 10 Epochen), 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

versuch_levels <- c("001", "003","004", "012")

base_file_name <- "Abb_DS_mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Datensatz A: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name)

# 4 Experimente (großer Datensatz, mAP_50, 10 Epochen), 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

base_file_name <- "Abb_DS_mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Datensatz A: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name)

#########################################################################################################################

# Effekt der Datensatzgröße: mAP_95, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

versuch_levels <- c("006", "001","003")

base_file_name <- "Abb_Datensatzgrößeneffekt__mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Effekt der Datensatzgröße: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name)

# Effekt der Datensatzgröße:mAP_50, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

base_file_name <- "Abb_Datensatzgrößeneffekt__mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Effekt der Datensatzgröße: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name)

#########################################################################################################################

versuch_levels <- c("006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")

# Datenaugmentation (15): mAP_95, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version,1

base_file_name <- "Abb_Datenaugmentation_15_mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Datenaugmentation (Übersicht): mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name)

# Datenaugmentation (15): mAP_50, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version,1

base_file_name <- "Abb_Datenaugmentation_15_mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1"
plot_title <- "Datenaugmentation (Übersicht): mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name)

#########################################################################################################################


run_linear_model <- function(data,
                             versuch_levels,
                             metric,
                             outlier_filter = list(versuch="001", SuperRank=6),
                             epoch_range = c(290, 299)) {

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
  subdata$versuch <- factor(subdata$versuch, levels = versuch_levels)

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
    Trainingsdatensatzkombinationen = rownames(coefs),
    Koeffizienten = coefs[, 1],
    `Std. Error`  = coefs[, 2],
    `t-value`     = coefs[, 3],
    "Pr(>|t|)"    = coefs[, 4],
    check.names   = FALSE
  )

  coef_data$TermType <- dplyr::case_when(
    coef_data$Trainingsdatensatzkombinationen == "(Intercept)" ~ "Intercept",
    !grepl(":", coef_data$Trainingsdatensatzkombinationen) ~ "Main Effect",
    TRUE ~ paste0(
      stringr::str_count(coef_data$Trainingsdatensatzkombinationen, ":") + 1,
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
  coef_data$Trainingsdatensatzkombinationen <- factor(
    coef_data$Trainingsdatensatzkombinationen,
    levels = coef_data$Trainingsdatensatzkombinationen
  )
  coef_data$Group <- ifelse(coef_data$TermType == "Interaction", 2,
                            ifelse(coef_data$TermType == "Main Effect", 1, 0))

  coef_data <- coef_data[order(coef_data$TermType), ]

  # ----------------------------
  # Step 9: File naming
  # ----------------------------
  xlsx_name <- paste0("linear_model_results_", metric, ".xlsx")
  pdf_name  <- paste0("Abb_LM_Koeffizienten_", metric, ".pdf")

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
    x = Trainingsdatensatzkombinationen,
    y = Koeffizienten,
    fill = TermType
  )) +
    geom_bar(stat = "identity") +
    geom_errorbar(
      aes(
        ymin = Koeffizienten - `Std. Error`,
        ymax = Koeffizienten + `Std. Error`
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
    ggtitle(paste("Koeffizienten des linearen Interaktionsmodells:", metric))

  ggsave(
    filename = pdf_name,
    plot = p,
    height = 5,
    width = 5
  )

  pdf_name_facet <- paste0("Abb_LM_Koeffizienten_FACET_", metric, ".pdf")
  
  p_facet <- ggplot(coef_data, aes(
    x = Trainingsdatensatzkombinationen,
    y = Koeffizienten,
    fill = TermType
  )) +
    geom_bar(stat = "identity") +
    geom_errorbar(
      aes(
        ymin = Koeffizienten - `Std. Error`,
        ymax = Koeffizienten + `Std. Error`
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
    ggtitle(paste("Faceted Koeffizienten des linearen Interaktionsmodells:", metric))
  
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


#########################################################################################################################

versuch_levels <- c("006", "014", "005",
                    "015", "007", "016", "009", "008",
                    "017", "018", "013", "019", "010",
                    "020", "011")

# Statistik (15 Datenaugmentations-Versuche): Vergleich linearer Modelle: mAP_50 
run_linear_model(data1df, versuch_levels, "mAP_50")
# Statistik (15 Datenaugmentations-Versuche): Vergleich linearer Modelle: mAP_95 
run_linear_model(data1df, versuch_levels, "mAP_95")

#########################################################################################################################

versuch_levels <- c("006", "014", "007")

# Effekt der konventionellen Datenaugmentation: mAP_95, 10 Epochen (schön), 5x001, Median-Version, Datensatzumbenennung
base_file_name <- "Abb_Konventionelle_Datenaugmentation_mAP_95_10E_5x001_Median_Datensatzumbenennung"
plot_title <- "Effekt der konventionellen Datenaugmentation: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name, "annotated")

# Effekt der konventionellen Datenaugmentation: mAP_95, 10 Epochen (schön), 5x001, Median-Version, Datensatzumbenennung, finale Version
base_file_name <- "Abb_Konventionelle_Datenaugmentation_mAP_95_10E_5x001_Median_Datensatzumbenennung_final"
plot_title <- "Einfluss der konventionellen Datenaugmentation: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name, "red_raw_medians")

# Effekt der konventionellen Datenaugmentation (mAP_50), 10 Epochen (schön), Median-Version, Datensatzumbenennung
base_file_name <- "Abb_Konventionelle_Datenaugmentation_mAP_50_10E_5x001_Median_Datensatzumbenennung"
plot_title <- "Effekt der konventionellen Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, "annotated")

# Effekt der konventionellen Datenaugmentation (mAP_50), 10 Epochen (schön), Median-Version, Datensatzumbenennung, finale Version
base_file_name <- "Abb_Konventionelle_Datenaugmentation_mAP_50_10E_5x001_Median_Datensatzumbenennung_final"
plot_title <- "Einfluss der konventionellen Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, "red_raw_medians")

#########################################################################################################################

versuch_levels <- c("006", "005", "008")

# Effekt der synthetischen Datenaugmentation (mAP_95), 10 Epochen (schön),Median-Version, Datensatzumbenennung
base_file_name <- "Abb_Synthetische_Datenaugmentation__mAP_95_10E_5x001_Median_Datensatzumbenennung"
plot_title <- "Einfluss der synthetischen Datenaugmentation: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name, "annotated")

# Effekt der synthetischen Datenaugmentation (mAP_95), 10 Epochen (schön),Median-Version, Datensatzumbenennung, finale Version
base_file_name <- "Abb_Synthetische_Datenaugmentation__mAP_95_10E_5x001_Median_Datensatzumbenennung_final"
plot_title <- "Einfluss der synthetischen Datenaugmentation: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name, "red_raw_medians")

# Effekt der synthetischen Datenaugmentation (mAP_50), 10 Epochen (schön),Median-Version, Datensatzumbenennung
base_file_name <- "Abb_Synthetische_Datenaugmentation__mAP_50_10E_5x001_Median_Datensatzumbenennung"
plot_title <- "Einfluss der synthetischen Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, "annotated")

# Effekt der synthetischen Datenaugmentation (mAP_50), 10 Epochen (schön),Median-Version, Datensatzumbenennung, finale Version
base_file_name <- "Abb_Synthetische_Datenaugmentation__mAP_50_10E_5x001_Median_Datensatzumbenennung_final"
plot_title <- "Einfluss der synthetischen Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, "red_raw_medians")

#########################################################################################################################

versuch_levels <- c("006", "007", "008", "013", "011")

# Maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_95, 10 Epochen (5 Plots), Median, Datensatzumbenennung
base_file_name <- "Abb_Datenaugmentation_kombiniert_mAP_95_10E_5x001_Median_Datensatzumbenennung"
plot_title <- "Kombinierte Datenaugmentation: mAP_95 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name, "annotated")

# Maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_95, 10 Epochen (5 Plots), Median, Datensatzumbenennung, finale Version
base_file_name <- "Abb_Datenaugmentation_kombiniert_mAP_95_10E_5x001_Median_Datensatzumbenennung_final"
plot_title <- "Kombinierte Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_95", plot_title, base_file_name, "red_raw_medians")

# Maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_50, 10 Epochen (5 Plots), Median, Datensatzumbenennung
base_file_name <- "Abb_Datenaugmentation_kombiniert_mAP_50_10E_5x001_Median_Datensatzumbenennung"
plot_title <- "Kombinierte Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, "annotated")

# Maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_50, 10 Epochen (5 Plots), Median, Datensatzumbenennung, finale Version
base_file_name <- "Abb_Datenaugmentation_kombiniert_mAP_50_10E_5x001_Median_Datensatzumbenennung_final"
plot_title <- "Kombinierte Datenaugmentation: mAP_50 der letzten 10 Epochen"
analyze_data(data1df, versuch_levels, symbol_map_experiments, symbol_config, "mAP_50", plot_title, base_file_name, "red_raw_medians")


#########################################################################################################################

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

# Zusammenhang zwischen Bildanzahl und Trainingszeit

# Make vectors for training time and number of images, hard encoded, come from outside.
traing_times  <- c(9.544, 113.401, 21.871, 4.854, 3.099, 2.856, 4.936, 11.607, 11.489, 12.316, 100.733, 5.162, 2.808, 9.625, 5.067, 9.924, 9.77, 11.583, 9.911)
image_numbers <- c(3410, 53908, 8855, 800, 15, 75, 815, 4000, 4015, 4075, 44275, 875, 60, 3200, 860, 3260, 3215, 4060, 3275)

plot_training_times(traing_times, image_numbers)

#########################################################################################################################

plot_versuch <- function(data,
                         versuch_id, 
                         metric,
                         title,
                         filename,
                         outlier_filter = list(versuch="001", SuperRank=6)) {

  # Filter data
  subdata <- subset(data, versuch == versuch_id)
  
  if (!is.null(outlier_filter)) {
    subdata <- subdata[!(subdata$versuch == outlier_filter$versuch & 
                           subdata$SuperRank == outlier_filter$SuperRank), ]
  }

  # Create plot
  p <- ggplot(subdata) +
    geom_point(aes(x = Epoche, y = .data[[metric]]), size = 0.1) +
    facet_wrap(~ SuperRank, nrow = 3) +
    ggtitle(title) +
    theme(plot.title = element_text(color = "black", size = 9))
  
  # Save to PDF
  pdf(filename, height = 5, width = 5)
  print(p)
  dev.off()
}

metrics <- c(
  mAP_50    = "mAP@50",
  mAP_95    = "mAP@95",
  precision = "Precision (Positiver Prädiktiver Wert)",
  recall    = "Recall (Sensitivität)"
)

experiments <- list(
  "001" = list(filter = TRUE),
  "003" = list(filter = FALSE)
)

for (versuch_id in names(experiments)) {

  for (metric in names(metrics)) {

    filter_superrank <- experiments[[versuch_id]]$filter

      plot_versuch(
      data = data1df,
      versuch_id = versuch_id,
      metric = metric,
      title = paste0("Experiment ", versuch_id, ": ", metrics[[metric]]),
      filename = paste0("Abb_Versuch_", versuch_id, "_", metric, ".pdf"),
    )
    
  }
}

# Versuch 1 (001), mAP_50

versuch_001 <-subset(data1df, versuch == "001"& SuperRank != 6)

pdf("Abb_Versuch_mAP_50_001_5x.pdf",height=5, width=5)


my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50), size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: mAP_50") + theme(plot.title = element_text(color="black", size=9))
print(my_plot)


dev.off()


# Versuch 1 (001), mAP_95

# Filtern Sie die Daten, um nur Versuch 001 zu behalten und entfernen Sie Versuch 6
versuch_001 <- subset(data1df, versuch == "001" & SuperRank != 6)

pdf("Abb_Versuch_mAP_95_001_5x.pdf", height=5, width=5)

my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95), size=0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: mAP_95") + theme(plot.title = element_text(color="black", size=9))
print(my_plot)

dev.off()


# Versuch 1 (001), precision 

versuch_001 <-subset(data1df, versuch == "001" & SuperRank != 6)

pdf("Abb_Versuch_p_001_5x.pdf",height=5, width=5)


my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: Precision (Postiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))
print(my_plot)
print(my_plot)


dev.off()

# Versuch 1 (001), recall

versuch_001 <-subset(data1df, versuch == "001" & SuperRank != 6)

pdf("Abb_Versuch_r_001_5x.pdf",height=5, width=5)


my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###################################################################################################

# Versuch 3 (003), mAP_50

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_mAP_50_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50), size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 3 (003), mAP_95

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_mAP_95_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 3 (003), precision 

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_p_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 3 (003), recall

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_r_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

#################################################################################################

# Versuch 4 (004), mAP_50

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_mAP_50_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 4 (004), mAP_95

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_mAP_95_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 4 (004), precision 

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_p_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 4 (004), recall

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_r_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###############################################################################################

# Versuch 5 (005), mAP_50

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_mAP_50_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 5 (005), mAP_95

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_mAP_95_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: mAP:95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 5 (005), precision 

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_p_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 5 (005), recall

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_r_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

#################################################################################################

dev.off()

# Versuch 6 (006), mAP_50

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_mAP_50_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 6 (006), mAP_95

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_mAP_95_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 6 (006), precision 

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_p_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 6 (006), recall

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_r_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

################################################################################################



# Versuch 7 (007), mAP_50

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_mAP_50_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 7 (007), mAP_95

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_mAP_95_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 7 (007), precision 

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_p_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 7 (007), recall

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_r_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

################################################################################################

# Versuch 8 (008), mAP_50

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_mAP_50_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 8 (008), mAP_95

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_mAP_95_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 8 (008), precision 

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_p_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 8 (008), recall

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_r_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

################################################################################################

# Versuch 9 (009), mAP_50

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_mAP_50_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 9 (009), mAP_95

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_mAP_95_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 9 (009), precision 

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_p_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 9 (009), recall

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_r_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()
#################################################################################################

# Versuch 10 (010), mAP_50

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_mAP_50_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 10 (010), mAP_95

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_mAP_95_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 10 (010), precision 

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_p_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 10 (010), recall

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_r_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

################################################################################################

# Versuch 11 (011), mAP_50

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_mAP_50_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 11 (011), mAP_95

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_mAP_95_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 11 (011), precision 

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_p_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 11 (011), recall

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_r_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

###################################################################################################

# Versuch 12 (012), mAP_50

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_mAP_50_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 12 (012), mAP_95

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_mAP_95_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 12 (012), precision 

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_p_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 12 (012), recall

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_r_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

###############################################################################################

# Versuch 13 (013), mAP_50

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_mAP_50_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 13 (013), mAP_95

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_mAP_95_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 13 (013), precision

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_p_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 13 (013), recall

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_r_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

##################################################################################################

# Versuch 14 (014), mAP_50

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_mAP_50_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 14 (014), mAP_95

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_mAP_95_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 14 (014), precision

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_p_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 14 (014), recall

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_r_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

##################################################################################################

# Versuch 15 (015), mAP_50

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_mAP_50_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 15 (015), mAP_95

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_mAP_95_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 15 (015), precision

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_p_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 15 (013), recall

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_r_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###################################################################################################

# Versuch 16 (016), mAP_50

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_mAP_50_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 16 (016), mAP_95

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_mAP_95_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 16 (016), precision

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_p_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 16 (016), recall

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_r_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

#################################################################################################

# Versuch 17 (013), mAP_50

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_mAP_50_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 17 (017), mAP_95

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_mAP_95_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 17 (017), precision

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_p_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 17 (017), recall

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_r_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###################################################################################################

# Versuch 18 (018), mAP_50

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_mAP_50_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 18 (018), mAP_95

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_mAP_95_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 18 (018), precision

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_p_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 18 (018), recall

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_r_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

################################################################################################

# Versuch 19 (019), mAP_50

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_mAP_50_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 19 (019), mAP_95

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_mAP_95_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 19 (019), precision

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_p_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 19 (019), recall

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_r_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

##################################################################################################

# Versuch 20 (020), mAP_50

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_mAP_50_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 20 (020), mAP_95

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_mAP_95_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 20 (020), precision

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_p_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 20 (020), recall

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_r_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

#################################################################################################
#################################################################################################
#################################################################################################
###################################################################################################

#Statistik Teil:


# Grafische Untersuchung auf Normalverteilung (mAP_95, 10 Epochen, 5x 001)
subdata_all <- subset(data1df, (Epoche > 289) & (Epoche < 300) & versuch %in% c("001", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020") & !(versuch == "001" & SuperRank == 6))
pdf("Histogramm_alleVersuche_mAP_95_10E_5x001.pdf", height=5, width=5)

ggplot(subdata_all, aes(x = mAP_95)) +
  geom_histogram(binwidth = 0.01) +
  facet_wrap(~versuch) +
  ggtitle("Histogramm der mAP_95-Werte für alle Versuche in den letzten 10 Epochen") +
  theme(plot.title = element_text(color = "black", size = 9),
        axis.text.x = element_text(size = 8))

dev.off()

################################################################################################
# Grafische Untersuchung auf Normalverteilung (mAP_50, 10 Epochen, 5x 001)

subdata_all <- subset(data1df, (Epoche > 289) & (Epoche < 300) & versuch %in% c("001", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020") & !(versuch == "001" & SuperRank == 6))
pdf("Histogramm_alleVersuche_mAP_50_10E_5x001.pdf", height=5, width=5)

ggplot(subdata_all, aes(x = mAP_50)) +
  geom_histogram(binwidth = 0.01) +
  facet_wrap(~versuch) +
  ggtitle("Histogramm der mAP_95-Werte für alle Versuche in den letzten 10 Epochen") +
  theme(plot.title = element_text(color = "black", size = 9),
        axis.text.x = element_text(size = 5))

dev.off()
###################################################################################################


#weitere Tabellen

library(tidyr)
####################################################################################################




























































########################################################################################################################

result <- aggregate(mAP_95 ~ OG + OG_aug + SG + SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

kruskal_result <- kruskal.test(mAP_95 ~ Versuch, data = result)
kruskal_result

lm <- lm(mAP_95 ~ OG + OG_aug + SG + SG_aug,data =result)
summary(lm)
AIC(lm)

lmi <- lm(mAP_95 ~ OG * OG_aug * SG * SG_aug,data =result)
summary(lmi)
AIC(lmi)



###################################################################################################################################

result <- aggregate(mAP_95 ~ OG + OG_aug + SG + SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

kruskal_result <- kruskal.test(mAP_95 ~ Versuch, data = result)
kruskal_result


lm <- lm(mAP_95 ~ OG + OG_aug + SG + SG_aug,data =result)
summary(lm)
AIC(lm)

lmi <- lm(mAP_95 ~ OG * OG_aug * SG * SG_aug,data =result)
summary(lmi)
AIC(lmi)

########################################################################################################################

result <- aggregate(mAP_95 ~ OG + OG_aug + SG + SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

kruskal_result <- kruskal.test(mAP_95 ~ Versuch, data = result)
kruskal_result

lm <- lm(mAP_95 ~ OG + OG_aug + SG + SG_aug,data =result)
summary(lm)
AIC(lm)

lmi <- lm(mAP_95 ~ OG * OG_aug * SG * SG_aug,data =result)
summary(lmi)
AIC(lmi)
