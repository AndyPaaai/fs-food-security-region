# Food Security Regional Analysis - Plots
# =============================================================================

# --- Load libraries ---
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(grid)
library(gridExtra)

# ==========================================================================
# 1. READ DATA
# ==========================================================================

step_1 <- read_excel("../step_1_region_food_security.xlsx")
step_2 <- read_excel("../step_2_region_food_security.xlsx")

# ==========================================================================
# 2. HELPER FUNCTIONS FOR CLEANING
# ==========================================================================

clean_indicator_names <- function(data) {
  data %>%
    mutate(
      indicator_clean = case_when(
        indicator == "Malnutrition: Share of children who are stunted" ~ "Malnutrition: Share of children who are stunted",
        indicator == "Malnutrition: Share of children who are underweight, 2024" ~ "Malnutrition: Share of children who are underweight",
        indicator == "Share of people who are undernourished" ~ "Share of people who are undernourished",
        indicator == "Number of people who are undernourished" ~ "Number of people who are undernourished",
        indicator == "Death rate from malnutrition, 2021" ~ "Death rate from malnutrition",
        indicator == "Global Hunger Index, 2021" ~ "Global Hunger Index",
        indicator == "Inequality in per capita calorie intake, 2020" ~ "Inequality in per capita calorie intake",
        indicator == "Number of people who are moderately or severely food insecure, 2022" ~ "Number of people who are moderately or severely food insecure",
        indicator == "Number of people who are severely food insecure, 2022" ~ "Number of people who are severely food insecure",
        indicator == "Share of population with moderate or severe food insecurity, 2022" ~ "Share of population with moderate or severe food insecurity",
        indicator == "Share of population with severe food insecurity, 2022" ~ "Share of population with severe food insecurity",
        indicator == "Dietary composition by country, 1961 to 2022" ~ "Dietary composition",
        indicator == "Fruit consumption per capita, 1961 to 2022" ~ "Fruit consumption per capita",
        indicator == "Vegetable consumption per capita, 1961 to 2022" ~ "Vegetable consumption per capita",
        indicator == "Hidden Hunger Index in pre-school children" ~ "Hidden Hunger Index in pre-school children",
        indicator == "Share of children receiving vitamin A supplementation" ~ "Share of children receiving vitamin A supplementation",
        indicator == "Share of children who have vitamin A deficiency" ~ "Share of children who have vitamin A deficiency",
        indicator == "Share of children who have anemia" ~ "Share of children who have anemia",
        indicator == "Share of households consuming iodized salt, 2020" ~ "Share of households consuming iodized salt",
        indicator == "Share of people who have zinc deficiency, 2005" ~ "Share of people who have zinc deficiency",
        indicator == "Share of women of reproductive age who have anemia" ~ "Share of women of reproductive age who have anemia",
        indicator == "Share of pregnant women who have vitamin A deficiency" ~ "Share of pregnant women who have vitamin A deficiency",
        indicator == "Share of pregnant women who have anemia" ~ "Share of pregnant women who have anemia",
        TRUE ~ indicator
      )
    )
}

clean_region_names <- function(data) {
  if("region" %in% names(data)) {
    data <- data %>%
      mutate(
        region_clean = case_when(
          region == "Eastern Mediterranean" ~ "Eastern\nMediterranean",
          region == "South-East Asia" ~ "South-East\nAsia",
          region == "Western Pacific" ~ "Western\nPacific",
          TRUE ~ region
        )
      )
  }
  return(data)
}

clean_model_abbrev <- function(data) {
  data %>%
    mutate(
      model_abbrev = case_when(
        model_type == "Linear Gaussian" ~ "LG",
        model_type == "Negative binomial (log link)" ~ "NB",
        model_type == "Quasi-binomial (logit link)" ~ "QB",
        model_type == "Poisson (log link)" ~ "P",
        model_type == "LMM (Gaussian)" ~ "LMM",
        model_type == "GLMM (nbinom2, log link)" ~ "GLMM-NB",
        model_type == "GLMM (beta, logit link)" ~ "GLMM-Beta",
        TRUE ~ model_type
      )
    )
}

# ==========================================================================
# 3. SCALING FUNCTIONS (+100 Publications)
# ==========================================================================
X_SCALE <- 100

apply_main_effect_scaling <- function(data) {
  data %>%
    mutate(
      is_or_irr_model = effect_unit %in% c("OR", "IRR"),
      is_beta_model = effect_unit == "\u03b2",
      estimate_scaled = case_when(
        is_or_irr_model & is.finite(estimate_raw) ~ exp(X_SCALE * estimate_raw),
        is_beta_model & is.finite(estimate_raw) ~ estimate_raw * X_SCALE,
        TRUE ~ estimate_raw
      ),
      ci_low_scaled = case_when(
        is_or_irr_model & is.finite(ci_low_raw) ~ exp(X_SCALE * ci_low_raw),
        is_beta_model & is.finite(ci_low_raw) ~ ci_low_raw * X_SCALE,
        TRUE ~ ci_low_raw
      ),
      ci_high_scaled = case_when(
        is_or_irr_model & is.finite(ci_high_raw) ~ exp(X_SCALE * ci_high_raw),
        is_beta_model & is.finite(ci_high_raw) ~ ci_high_raw * X_SCALE,
        TRUE ~ ci_high_raw
      )
    )
}

# ==========================================================================
# 4. PREPROCESS DATASETS
# ==========================================================================

# -- Process step 1 --
step_1 <- step_1 %>%
  mutate(
    indicator = dependent_var,
    indicator_role = "DV"
  ) %>%
  clean_indicator_names() %>%
  mutate(indicator_label = indicator_clean) %>%
  clean_region_names() %>%
  clean_model_abbrev() %>%
  apply_main_effect_scaling()

# -- Process step 2 --
step_2 <- step_2 %>% filter(!is.na(estimate_raw)) %>%
  mutate(
    indicator = dependent_var,
    indicator_role = case_when(
      role_of_indicator == "independent" ~ "IV",
      TRUE ~ "DV"
    )
  ) %>%
  clean_indicator_names() %>%
  mutate(indicator_label = indicator_clean) %>%
  clean_region_names() %>%
  clean_model_abbrev() %>%
  mutate(
    category_label = case_when(
      category == "food_insecurity_and_hunger" ~ "Food Insecurity & Hunger",
      category == "dietary_patterns_and_intake" ~ "Dietary Patterns & Intake",
      category == "micronutrient_and_specific_deficiencies" ~ "Micronutrient Deficiencies",
      TRUE ~ category
    )
  ) %>%
  apply_main_effect_scaling()

# ==========================================================================
# FACTOR ORDERING
# ==========================================================================

indicator_order_food <- c(
  "Malnutrition: Share of children who are stunted",
  "Malnutrition: Share of children who are underweight",
  "Share of people who are undernourished",
  "Number of people who are undernourished",
  "Death rate from malnutrition",
  "Global Hunger Index",
  "Inequality in per capita calorie intake",
  "Number of people who are moderately or severely food insecure",
  "Number of people who are severely food insecure",
  "Share of population with moderate or severe food insecurity",
  "Share of population with severe food insecurity"
)

indicator_order_diet <- c(
  "Dietary composition",
  "Fruit consumption per capita",
  "Vegetable consumption per capita"
)

indicator_order_micro <- c(
  "Hidden Hunger Index in pre-school children",
  "Share of children who have vitamin A deficiency",
  "Share of children who have anemia",
  "Share of people who have zinc deficiency",
  "Share of women of reproductive age who have anemia",
  "Share of pregnant women who have vitamin A deficiency",
  "Share of pregnant women who have anemia",
  "Share of children receiving vitamin A supplementation",
  "Share of households consuming iodized salt"
)

indicator_order <- c(indicator_order_food, indicator_order_diet, indicator_order_micro)

region_order <- c("Africa", "Americas", "Eastern\nMediterranean", "Europe",
                  "South-East\nAsia", "Western\nPacific")

n_food  <- length(indicator_order_food)
n_diet  <- length(indicator_order_diet)
n_micro <- length(indicator_order_micro)
sep_y1  <- n_micro + n_diet + 0.5
sep_y2  <- n_micro + 0.5

# =============================================================================
# Figure 1: Heatmap — Regional Associations (Step 1)
# =============================================================================

# Filter to only indicators in indicator_order (excludes "Dietary composition")
step_1 <- step_1 %>% filter(indicator_label %in% indicator_order)

step_1 <- step_1 %>%
  mutate(
    effect_direction = case_when(
      effect_unit == "\u03b2" & estimate_scaled > 0 ~ "Positive",
      effect_unit == "\u03b2" & estimate_scaled < 0 ~ "Negative",
      effect_unit %in% c("IRR", "OR") & estimate_scaled > 1 ~ "Positive",
      effect_unit %in% c("IRR", "OR") & estimate_scaled < 1 ~ "Negative",
      TRUE ~ "Null"
    ),
    sig_label = case_when(
      p_value_adj_holm < 0.001 ~ "***",
      p_value_adj_holm < 0.01  ~ "**",
      p_value_adj_holm < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

step_1 <- step_1 %>%
  mutate(
    effect_magnitude_raw = case_when(
      effect_unit == "\u03b2"  ~ abs(estimate_raw),
      effect_unit %in% c("IRR", "OR") ~ abs(estimate_raw),
      TRUE ~ 0
    )
  ) %>%
  group_by(effect_unit) %>%
  mutate(
    norm_magnitude = percent_rank(effect_magnitude_raw),
    norm_magnitude = ifelse(is.na(norm_magnitude), 0, norm_magnitude)
  ) %>%
  ungroup() %>%
  mutate(
    signed_norm = case_when(
      effect_direction == "Positive" ~ norm_magnitude,
      effect_direction == "Negative" ~ -norm_magnitude,
      TRUE ~ 0
    )
  )

step_1 <- step_1 %>%
  mutate(
    indicator_label = factor(indicator_label, levels = rev(indicator_order)),
    region_clean = factor(region_clean, levels = region_order)
  )

theme_pub <- theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "sans", color = "grey10"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0,
                              margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11, color = "grey30", hjust = 0,
                                 margin = margin(b = 10)),
    plot.caption = element_text(size = 8.5, color = "grey50", hjust = 0,
                                lineheight = 1.3, margin = margin(t = 12)),
    axis.text.x = element_text(size = 11, face = "bold", color = "grey20",
                               lineheight = 0.9),
    axis.text.y = element_text(size = 10, color = "grey20"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(1.4, "cm"),
    legend.margin = margin(t = 5),
    plot.margin = margin(t = 10, r = 180, b = 10, l = 10)
  )

p1 <- ggplot(step_1, aes(x = region_clean, y = indicator_label)) +
  geom_tile(aes(fill = signed_norm), color = "white", linewidth = 0.6) +
  geom_text(aes(label = sig_label), size = 5, fontface = "bold",
            color = "grey10", vjust = 0.1) +
  geom_text(aes(label = model_abbrev), size = 2.5, color = "black",
            vjust = 1.9, fontface = "italic") +
  scale_fill_gradientn(
    colors = c("#a50f15", "#cb181d", "#ef6548", "#fc9272", "#fee0d2",
               "#f7f7f7",
               "#deebf7", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
    values = rescale(c(-1, -0.8, -0.5, -0.3, -0.1, 0, 0.1, 0.3, 0.5, 0.8, 1)),
    limits = c(-1, 1),
    na.value = "grey90",
    name = "Effect direction & relative magnitude",
    breaks = c(-0.75, -0.25, 0, 0.25, 0.75),
    labels = c("Higher\nnegative", "Lower\nnegative", "Near\nzero", "Lower\npositive", "Higher\npositive"),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5, barwidth = 14, barheight = 0.5,
      frame.colour = "grey50", ticks.colour = "grey50"
    )
  ) +
  # Separator lines between categories
  geom_hline(yintercept = sep_y1, linewidth = 1.2, color = "grey30") +
  geom_hline(yintercept = sep_y2, linewidth = 0.8, color = "grey50", linetype = "dashed") +
  # Right-side bracket: Food Insecurity & Hunger
  annotate("segment", x = 7.4, xend = 7.4,
           y = n_micro + n_diet + 1 - 0.4,
           yend = n_micro + n_diet + n_food + 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4,
           y = n_micro + n_diet + 1 - 0.4,
           yend = n_micro + n_diet + 1 - 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4,
           y = n_micro + n_diet + n_food + 0.4,
           yend = n_micro + n_diet + n_food + 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("text", x = 7.85,
           y = n_micro + n_diet + n_food / 2 + 0.5,
           label = "Food insecurity\nand hunger",
           size = 3.2, fontface = "bold.italic", color = "grey25",
           hjust = 0.5, lineheight = 0.9, angle = 270) +
  # Right-side bracket: Dietary Patterns & Intake
  annotate("segment", x = 7.4, xend = 7.4,
           y = n_micro + 1 - 0.4,
           yend = n_micro + n_diet + 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4,
           y = n_micro + 1 - 0.4,
           yend = n_micro + 1 - 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4,
           y = n_micro + n_diet + 0.4,
           yend = n_micro + n_diet + 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("text", x = 7.85,
           y = n_micro + n_diet / 2 + 0.5,
           label = "Dietary patterns\nand intake",
           size = 3.2, fontface = "bold.italic", color = "grey25",
           hjust = 0.5, lineheight = 0.9, angle = 270) +
  # Right-side bracket: Micronutrient Deficiencies
  annotate("segment", x = 7.4, xend = 7.4,
           y = 0.6,
           yend = n_micro + 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4,
           y = 0.6, yend = 0.6,
           color = "grey30", linewidth = 0.6) +
  annotate("segment", x = 7.2, xend = 7.4,
           y = n_micro + 0.4,
           yend = n_micro + 0.4,
           color = "grey30", linewidth = 0.6) +
  annotate("text", x = 7.85,
           y = n_micro / 2 + 0.5,
           label = "Micronutrient and\nspecific deficiencies",
           size = 3.2, fontface = "bold.italic", color = "grey25",
           hjust = 0.5, lineheight = 0.9, angle = 270) +
  scale_x_discrete(expand = c(0, 0), position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off", xlim = c(0.5, 6.5)) +
  labs(
    title = "Regional Associations Between Food Security Publications and Food Security Indicators",
    subtitle = "Step 1 regression analysis across WHO regions | Scaled per +100 publications",
    caption = paste0(
      "Significance: *** p < 0.001;  ** p < 0.01;  * p < 0.05 (Holm-adjusted p-values only)\n",
      "Model types: LG = Linear Gaussian;  QB = Quasi-binomial;  P = Poisson\n",
      "Color: Blue = positive association, Red = negative association  |  ",
      "Intensity: rank-normalized relative magnitude within each effect-unit type (\u03b2, IRR, OR)"
    )
  ) +
  theme_pub

ggsave("Figure_1_heatmap_food_security.pdf", p1, width = 14, height = 14, bg = "white")
ggsave("Figure_1_heatmap_food_security.svg", p1, width = 14, height = 14, bg = "white")

cat("Figure 1 (Heatmap) saved successfully.\n")

# =============================================================================
# Figure 2: Forest Plot — Mixed-Effects Model Results (Step 2)
# =============================================================================

# Filter to only indicators in indicator_order
step_2 <- step_2 %>% filter(indicator_label %in% indicator_order)

step_2 <- step_2 %>%
  mutate(
    sig_label = case_when(
      p_adj_holm < 0.001 ~ "***",
      p_adj_holm < 0.01  ~ "**",
      p_adj_holm < 0.05  ~ "*",
      TRUE ~ ""
    ),
    is_significant = p_adj_holm < 0.05,
    plot_estimate = estimate_scaled,
    plot_ci_low = ci_low_scaled,
    plot_ci_high = ci_high_scaled,
    ref_line = case_when(
      effect_unit == "\u03b2" ~ 0,
      TRUE ~ 1
    )
  )

cat_colors <- c("Food Insecurity & Hunger" = "#d95f02",
                "Dietary Patterns & Intake" = "#1b9e77",
                "Micronutrient Deficiencies" = "#7570b3")

theme_forest <- theme_minimal(base_size = 11) +
  theme(
    text = element_text(family = "sans", color = "grey10"),
    axis.text.y = element_text(size = 9, color = "grey20"),
    axis.text.x = element_text(size = 8.5, color = "grey30"),
    axis.title.x = element_text(size = 9.5, color = "grey30", margin = margin(t = 6)),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey75", fill = NA, linewidth = 0.4),
    plot.title = element_text(size = 11, face = "bold", color = "grey15", margin = margin(b = 6)),
    legend.position = "none",
    plot.margin = margin(t = 5, r = 35, b = 5, l = 5)
  )

build_panel <- function(data, panel_title, x_label, scale_type = "linear") {
  present <- intersect(indicator_order, unique(data$indicator_label))
  data <- data %>%
    mutate(indicator_label = factor(indicator_label, levels = rev(present)))

  ref <- unique(data$ref_line)

  p <- ggplot(data, aes(x = plot_estimate, y = indicator_label)) +
    geom_vline(xintercept = ref, linetype = "dashed", color = "grey55", linewidth = 0.45) +
    geom_errorbarh(aes(xmin = plot_ci_low, xmax = plot_ci_high), color = "grey40", height = 0.3, linewidth = 0.55) +
    geom_point(aes(fill = category_label, alpha = is_significant), shape = 21, size = 3.5, stroke = 0.8, color = "grey30") +
    geom_text(aes(label = model_abbrev), x = -Inf, size = 2.3, color = "grey50", hjust = -0.1, vjust = -1.0, fontface = "italic") +
    scale_fill_manual(values = cat_colors, guide = "none") +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.45), guide = "none") +
    labs(title = panel_title, x = x_label)

  if (scale_type == "pseudo_log") {
    p <- p + scale_x_continuous(trans = pseudo_log_trans(sigma = 10000), labels = scales::comma)
  } else if (scale_type == "log10") {
    p <- p + scale_x_log10(labels = scales::number_format(accuracy = 0.01))
  }

  if (scale_type == "log10") {
    data <- data %>% mutate(sig_x_pos = plot_ci_high * 1.08)
  } else {
    x_range <- max(data$plot_ci_high, na.rm = TRUE) - min(data$plot_ci_low, na.rm = TRUE)
    data <- data %>% mutate(sig_x_pos = plot_ci_high + x_range * 0.03)
  }

  p <- p +
    geom_text(data = data, aes(x = sig_x_pos, label = sig_label), size = 4.5, fontface = "bold", color = "#b2182b", hjust = 0, vjust = 0.35) +
    coord_cartesian(clip = "off") +
    theme_forest

  return(p)
}

df_beta <- step_2 %>% filter(effect_unit == "\u03b2")
df_or   <- step_2 %>% filter(effect_unit == "OR")

panels <- list()
heights_vals <- c()

if (nrow(df_beta) > 0) {
  p_beta <- build_panel(df_beta, "Coefficient (\u03b2)", "\u03b2 (95% CI, scaled +100 pubs)", scale_type = "pseudo_log")
  panels <- c(panels, list(p_beta))
  heights_vals <- c(heights_vals, nrow(df_beta))
}
if (nrow(df_or) > 0) {
  p_or <- build_panel(df_or, "Odds Ratio (OR)", "OR (95% CI, scaled +100 pubs)", scale_type = "log10")
  panels <- c(panels, list(p_or))
  heights_vals <- c(heights_vals, nrow(df_or))
}

legend_plot <- ggplot() +
  annotate("point", x = c(1, 3.8, 7.0), y = c(1, 1, 1), shape = 21, size = 4, stroke = 0.8,
           color = "grey30", fill = c("#d95f02", "#1b9e77", "#7570b3")) +
  annotate("text", x = c(1.35, 4.15, 7.35), y = c(1, 1, 1),
           label = c("Food Insecurity & Hunger", "Dietary Patterns & Intake", "Micronutrient Deficiencies"),
           size = 2.8, hjust = 0, color = "grey20") +
  annotate("point", x = c(11, 12.8), y = c(1, 1), shape = 21, size = 4, stroke = 0.8,
           color = "grey30", fill = "grey55", alpha = c(1, 0.4)) +
  annotate("text", x = c(11.35, 13.15), y = c(1, 1),
           label = c("p < 0.05 (Holm)", "Not significant"),
           size = 2.8, hjust = 0, color = "grey20") +
  annotate("text", x = c(0.6, 10.6), y = c(1.55, 1.55),
           label = c("Domain", "Holm-adjusted significance"),
           size = 3, hjust = 0, fontface = "bold", color = "grey15") +
  scale_x_continuous(limits = c(0, 16)) +
  scale_y_continuous(limits = c(0.4, 1.8)) +
  theme_void()

title_grob <- textGrob(
  "Mixed-Effects Model Results: Food Security Publications and Food Security Indicators",
  gp = gpar(fontsize = 13, fontface = "bold", col = "grey10"),
  hjust = 0, x = unit(0.02, "npc"))
subtitle_grob <- textGrob(
  "Step 2 regression analysis with WHO region as random effect | Scaled per +100 publications",
  gp = gpar(fontsize = 9.5, col = "grey35"),
  hjust = 0, x = unit(0.02, "npc"))

caption_text <- paste0(
  "Significance: *** p < 0.001;  ** p < 0.01;  * p < 0.05 (Holm-adjusted p-values only)\n",
  "Models: LMM = Linear mixed model;  GLMM-Beta = Generalized LMM (beta regression)\n",
  "Estimates and CIs are scaled to represent the effect of +100 publications.  |  Dashed line = null effect\n",
  "Non-converged models excluded  |  Opacity: lighter = not significant"
)
caption_grob <- textGrob(caption_text,
  gp = gpar(fontsize = 7.5, col = "grey50", lineheight = 1.3),
  hjust = 0, x = unit(0.02, "npc"), just = "left")

grobs_list <- c(list(title_grob, subtitle_grob), panels, list(legend_plot, caption_grob))
height_units <- c(0.7, 0.5, heights_vals, 1.8, 2.0)
height_types <- c("cm", "cm", rep("null", length(heights_vals)), "cm", "cm")

final_plot <- arrangeGrob(
  grobs = grobs_list,
  ncol = 1,
  heights = unit(height_units, height_types)
)

ggsave("Figure_2_forest_food_security.pdf", final_plot, width = 10, height = 12, bg = "white")
ggsave("Figure_2_forest_food_security.svg", final_plot, width = 10, height = 12, bg = "white")

cat("Figure 2 (Forest Plot) saved successfully.\n")
cat("\n=== SUMMARY ===\n")
cat("Datasets loaded: step_1, step_2\n")
cat("Scaling per +100 publications applied.\n")
cat("Figures saved as PDF and SVG in output folder.\n")
