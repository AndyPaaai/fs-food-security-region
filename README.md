# Food Security and Global Health Region Analysis

This repository contains the datasets and R code necessary to analyze the relationship between food-security-related scientific publication activity and various food security indicators across World Health Organization (WHO) regions from 2000 to 2025. 

The analysis aims to evaluate patterns of alignment between scientific production (knowledge-generation capacity) and population-level indicators across three domains: **Food Insecurity and Hunger**, **Dietary Patterns and Intake**, and **Micronutrient and Specific Deficiencies**.

## Repository Structure

The project is divided into two main directories:

### 1. data/ (Datasets)
Contains the raw and processed Excel files for each step of the analysis:

* `data.xlsx`: The original raw database containing bibliometric variables and all global indicators (health, economy, agriculture, etc.) at the publication/country level.
* `step_1_region_global_health.xlsx`: Aggregated results for Step 1. Contains the output of region-specific simple regression models evaluating the associations between publications and indicators across different WHO regions.
* `step_2_region_global_health.xlsx`: Aggregated results for Step 2. Contains the output of Hierarchical Mixed-Effects Models, where "WHO region" is used as a random intercept to capture regional variance and temporal structures.
* `step_3_region_global_health.xlsx`: Aggregated results for Step 3. Includes interactions and mixed-effects moderator screening (e.g., conditional slopes at the 25th and 75th percentiles of moderators like health researcher density or R&D expenditure) to understand how the effect of food security publications changes based on structural contextual factors.

### 2. src/ (Source Code)
Contains the R scripts that execute data cleaning, statistical analysis, and visualization:

* `Main Analysis.R`: The core analytical engine.
  * Data Cleaning: Depurates the raw database (`data.xlsx`), handling indicators from sources like the World Bank, WHO Global Health Observatory, and Our World in Data. Converts text to numeric values, removes commas, parses percentages into proportions, and handles country-to-WHO-region mapping.
  * Aggregation: Groups data at the WHO region-year level and calculates population-weighted averages (`Population in year`), ensuring indicators reflect population exposure rather than unweighted averages.
  * Pre-analysis: Categorizes 23 indicators into their thematic domains and establishes candidate moderators (e.g., health system, socioeconomic development).
  * Statistical Modeling: Implements an automated model selection framework based on the empirical distribution (Poisson for count-type, Quasi-binomial/GLMM Beta for proportions, Gaussian/log-Gaussian for continuous). Executes the regressions for steps 1, 2, and 3.

* `R Plots.R`: The visualization and post-processing script.
  * Takes the processed output from the regressions (`step_1`, `step_2`, `step_3`) and applies a standardized mathematical scaling factor (see the Scaling Methodology section below).
  * Generates Figure 1 (Heatmap depicting the direction, magnitude, and significance of regional associations across 23 indicators and six WHO regions).
  * Generates Figure 2 (Forest Plot depicting coefficients, IRRs, and ORs with their respective confidence intervals under mixed-effects models).
  * Exports high-resolution visualizations (`.png`, `.pdf`, `.svg`).

---

## Scaling Methodology

To ensure reproducibility and facilitate the interpretation of results in the manuscript, all effect sizes in `R Plots.R` are scaled to represent the effect of an increase of **+100 publications**.

This scaling is implemented as follows:

1. Main Effects (Linear Models):
   The raw $\beta$ coefficient and its 95% Confidence Intervals (CI) are multiplied by 100.
   `estimate_scaled = estimate_raw * 100`

2. Ratio Models (IRR, OR):
   Since the raw estimates are generated on a log-link or logit scale, they are rescaled by exponentiating the per-publication estimate to the 100th power before calculating percentage changes.
   `estimate_scaled = exp(estimate_raw * 100)`

3. Interactions (Step 3):
   Simple slopes (the effect of +100 publications at low and high levels of a moderator) and contrast ratios are calculated by propagating the `X_SCALE <- 100` factor exactly to the logarithmic estimates and their confidence bounds.

---

## Usage

1. Clone this repository to your local machine.
2. Ensure you have installed the required R packages (e.g., `dplyr`, `ggplot2`, `readxl`, `lme4`, `glmmTMB`, `MASS`, `lmtest`, `sandwich`).
3. Execute `src/Main Analysis.R` to regenerate all regression models from the raw data.
4. Execute `src/R Plots.R` to apply the +100 publications scaling and generate publication-ready visualizations.
