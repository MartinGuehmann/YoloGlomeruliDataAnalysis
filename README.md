# YOLO Glomeruli Data Analysis

R script analyzing YOLO object detection training results for glomeruli
detection in kidney tissue sections, across 19 experiments with varying training
data compositions (real, synthetic, augmented images).

It does non-parametric statistical comparisons, linear modeling, and
generates publication-ready plots and statistical summaries evaluating how
different training data compositions affect YOLO detection performance.

## Input

- `allresults_header_tab_final_v002.txt` — tab-separated YOLO training
  results; one row per epoch per run; key columns: `filename` (used to
  parse versuch/version), `Epoch`, `mAP@50`, `mAP@50-95`, `Precision`,
  `Recall`
- `Design2.xlsx` — experiment design table; joined on numeric column
  `Versuch`; provides factor columns `TrainTiny`, `TinyAug`, `Syn`,
  `SynAug` used in linear models

## Output

All output is written to `output/`, which is created next to the script:

- `output/Histograms/` — Shapiro-Wilk normality test results (`.xlsx`) and
  histograms (`.pdf`) per metric
- `output/<metric>/` — Boxplots (`.pdf`/`.svg`/`.eps`/`.tiff`) and Dunn
  heatmaps per job and metric; statistical summaries (`.xlsx`) with
  Kruskal-Wallis, Dunn, and Mann-Whitney results
- `output/<metric>/Annotated/` — same plots with median-of-medians overlay
- `output/<metric>/RedRawMedians/` — same plots with raw median points in
  red
- `output/<metric>/FigureBoxPlots_<metric>.pdf/.svg/.eps/.tiff` —
  assembled multi-panel figures (panels A-E)
- `output/LinearModels/` — coefficient plots and `.xlsx` summaries for
  linear and interaction models; assembled two-panel figure (panels A-B)
- `output/<metric>/MetricCurves/` — per-experiment epoch curves (`.pdf`),
  faceted by SuperRank
- `output/NumberOfImages_TraingTime.pdf/.svg/.eps/.tiff` — training time
  vs. dataset size regression plot

## Workflow

1. Load and parse data: extract versuch, version, and epoch from
   filenames; merge with experimental design
2. Filter runs: keep only runs with more than 290 epochs; rank versions
   within each experiment (SuperRank)
3. Normality checks: Shapiro-Wilk test and histograms for all 19
   experiments across all four metrics
4. Training time analysis: linear regression of training time vs. number
   of training images
5. Statistical analysis per job and metric: Kruskal-Wallis, optional
   Mann-Whitney U (2-group jobs), Dunn post-hoc with Bonferroni
   correction; effect sizes (r, rbc, cles)
6. Plot generation: boxplots (3 modes), Dunn heatmaps, assembled
   multi-panel figures
7. Linear models: main-effects and full interaction models on ranked
   medians; coefficient plots, faceted plots, assembled two-panel figure
   for mAP@50 and mAP@50-95
8. Epoch curves: per-experiment metric evolution plots faceted by
   SuperRank

## Notes

- Analysis window: last 10 epochs (`epoch_range = c(290, 299)`)
- Outlier removed: versuch "001", SuperRank 6
- Statistical significance threshold: alpha = 0.05
- Dunn p-values are Bonferroni-corrected
- Effect size r = Z / sqrt(n); thresholds: <0.1 negligible, <0.3 small,
  <0.5 medium, >=0.5 large
- Linear models use ranked median values as response
- Significance brackets are only added to plots with 2 or 3 groups to
  avoid clutter
- Training times and image counts are hard-coded vectors (19
  experiments); ideally these would come from a file
- All plots are saved in PDF, SVG, EPS, and TIFF (600 dpi, LZW)

## Requirements

R with the following packages:

`ggplot2`, `readr`, `dplyr`, `tidyr`, `readxl`, `writexl`, `FSA`,
`stringr`, `openxlsx`, `reshape2`, `patchwork`, `ggpubr`

## Usage

From the command line:

```sh
Rscript YOLO_data_analysis.R
```

Or open it in RStudio and select all lines and press run.

The script sets its working directory to its own location and expects
`allresults_header_tab_final_v002.txt` and `Design2.xlsx` alongside it.

## License

- **Code** (`YOLO_data_analysis.R`): [MIT License](LICENSE).
- **Data and generated figures** (`allresults_header_tab_final_v002.txt`,
  `Design2.xlsx`, and the contents of `output/`): [CC BY 4.0](LICENSE-DATA.txt).

