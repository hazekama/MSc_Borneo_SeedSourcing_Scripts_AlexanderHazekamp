# Load libraries
library(readr)
library(dplyr)
library(broom)    # For tidy regression output
library(gt)       # For nice summary tables (optional)
library(purrr)

working_dir <- "C:/Users/Alex/Downloads/MSc Spring Semester/Research Project/Results Excel/Cleaned"
setwd(working_dir)
# Load data
rlep_frag <- read_csv("Tab1Rlep.csv")  # Table 1 format
rlep_access <- read_csv("Tab3Rlep.csv")   # Table 3 format
rparv_frag <- read_csv("Tab2Rparv.csv")  # Table 2 format
rparv_access <- read_csv("Tab4Rparv.csv")   # Table 4 format

### ---------- QUESTION 1 ----------
# Regression: Mean Patch Size ~ Fragmentation Loss

# R. leprosula
lm_rlep_q1_Om <- lm(Mean_Ha_Om ~ Frag_pct_Om, data = rlep_frag)
lm_rlep_q1_EqSS <- lm(Mean_Ha_EqSS ~ Frag_pct_EqSS, data = rlep_frag)
lm_rlep_q1_D <- lm(Mean_Ha_Om ~ Frag_pct_D, data = rlep_frag)
lm_rlep_q1_High <- lm(Mean_Ha_High ~ Frag_pct_High, data = rlep_frag)

# R. parvifolia
lm_rparv_q1_Om <- lm(Mean_Ha_Om ~ Frag_pct_Om, data = rparv_frag)
lm_rparv_q1_EqSS <- lm(Mean_Ha_EqSS ~ Frag_pct_EqSS, data = rparv_frag)
lm_rparv_q1_D <- lm(Mean_Ha_D ~ Frag_pct_D, data = rparv_frag)
lm_rparv_q1_high <- lm(Mean_Ha_High ~ Frag_pct_High, data = rparv_frag)

# Cleaned up for results:
models_q1 <- list(
  rlep_Om = lm_rlep_q1_Om,
  rlep_EqSS = lm_rlep_q1_EqSS,
  rlep_D = lm_rlep_q1_D,
  rlep_High = lm_rlep_q1_High,
  rparv_Om = lm_rparv_q1_Om,
  rparv_EqSS = lm_rparv_q1_EqSS,
  rparv_D = lm_rparv_q1_D,
  rparv_High = lm_rparv_q1_high
)

results_q1 <- purrr::map_df(models_q1, tidy, .id = "Model")

print(results_q1)

### ---------- QUESTION 2 ----------
# Regression: Fragmentation Loss ~ Accessibility (1 km + 5 km)

# R. leprosula
lm_rlep_q2_Om <- lm(Frag_pct_Om ~ `1km_pct_Om` + `5km_pct_Om`, data = rlep_access)
lm_rlep_q2_EqSS <- lm(Frag_pct_EqSS ~ `1km_pct_EqSS` + `5km_pct_EqSS`, data = rlep_access)
lm_rlep_q2_D <- lm(Frag_pct_D ~ `1km_pct_D` + `5km_pct_D`, data = rlep_access)
lm_rlep_q2_High <- lm(Frag_pct_High ~ `1km_pct_High` + `5km_pct_High`, data = rlep_access)


# R. parvifolia
lm_rparv_q2_Om <- lm(Frag_pct_Om ~ `1km_pct_Om` + `5km_pct_Om`, data = rparv_access)
lm_rparv_q2_EqSS <- lm(Frag_pct_EqSS ~ `1km_pct_EqSS` + `5km_pct_EqSS`, data = rparv_access)
lm_rparv_q2_D <- lm(Frag_pct_D ~ `1km_pct_D` + `5km_pct_D`, data = rparv_access)
lm_rparv_q2_High <- lm(Frag_pct_High ~ `1km_pct_High` + `5km_pct_High`, data = rparv_access)

models_q2 <- list(
  rlep_Om = lm_rlep_q2_Om,
  rlep_EqSS = lm_rlep_q2_EqSS,
  rlep_D = lm_rlep_q2_D,
  rlep_High = lm_rlep_q2_High,
  rparv_Om = lm_rparv_q2_Om,
  rparv_EqSS = lm_rparv_q2_EqSS,
  rparv_D = lm_rparv_q2_D,
  rparv_High = lm_rparv_q2_High
)

results_q2 <- map_df(models_q2, tidy, .id = "Model")

print(results_q2)
# Optionally write to CSV
# write.csv(results_q2, "regression_results_q2.csv", row.names = FALSE)

### ---------- SUMMARISE RESULTS ----------

# Function to extract stats from a model with possibly multiple predictors
extract_model_stats <- function(model, question, species, response, model_name) {
  tidy_model <- tidy(model)
  glance_model <- glance(model)
  
  # Filter out intercept and get one row per predictor
  preds <- tidy_model %>% filter(term != "(Intercept)")
  
  preds %>%   # Take the data frame 'preds' (model coefficients excluding intercept)
    transmute(  # Create a new data frame with selected and transformed columns:
      Question = question,      # Add a column named 'Question' with the value passed in (e.g. "Q1" or "Q2")
      Species = species,        # Add a column named 'Species' with the model/species name string
      Response = response,      # Add a column named 'Response' with the dependent variable's name (e.g. "Mean Patch Size")
      Predictor = term,         # Add 'Predictor' column from the model term names (independent variables)
      Estimate = round(estimate, 3),   # Add coefficient estimates rounded to 3 decimal places (effect size of predictor)
      `Std. Error` = round(std.error, 3),  # Add standard errors of estimates rounded to 3 decimals (measure of estimate uncertainty)
      `p-value` = round(p.value, 4),  # Add p-values rounded to 4 decimals (statistical significance of predictor)
      `R-squared` = round(glance_model$r.squared, 3),  # Add overall model R-squared (variance explained), rounded to 3 decimals
      Model = model_name        # Add a column with the model name for reference (e.g. "rlep_Om")
    )
}

# Extract Q1 results (Mean Patch Size ~ Fragmentation Loss)
summary_q1 <- purrr::imap_dfr(models_q1, function(mod, nm) {
  extract_model_stats(mod, "Q1", species = nm, response = "Mean Patch Size", model_name = nm)
})


# Extract Q2 results (Fragmentation Loss ~ Accessibility)
summary_q2 <- purrr::imap_dfr(models_q2, function(mod, nm) {
  extract_model_stats(mod, "Q2", species = nm, response = "Fragmentation Loss", model_name = nm)
})

# Combine all results and arrange
summary_results <- bind_rows(summary_q1, summary_q2) %>%
  select(Question, Species, Response, Predictor, Estimate, `Std. Error`, `p-value`, `R-squared`, Model) %>%
  arrange(Question, Species, Predictor)

# Display as nice table
summary_results %>% 
  gt::gt() %>%
  gt::tab_header(
    title = "Summary of Regression Models",
    subtitle = "Q1: Mean Patch Size vs Fragmentation Loss; Q2: Fragmentation Loss vs Accessibility"
  )

# After creating summary_results (a dataframe), write to CSV:
write.csv(summary_results, "regression_summary_results.csv", row.names = FALSE)

# Or for nicer look:
library(readr)
write_csv(summary_results, "regression_summary_results_nice.csv")

### ---------- PLOTS FOR APENDIX SUMMARY RESULTS ----------

working_dir <- "C:/Users/Alex/Downloads/MSc Spring Semester/Research Project/Results Excel"
setwd(working_dir)
# Load data
rlep_seedzone_sum <- read_csv("Summary_SeedZone_ByThreshold_Rlep.csv")  
rlep_frag_sum <- read_csv("Rlep_Fragmentation_Loss_Wide_BySeedZone.csv")
rlep_access_sum <- read_csv("R.lep Accessibility Final.csv")
rparv_seedzone_sum <- read_csv("Summary_SeedZone_ByThreshold_Rparv.csv")  
rparv_frag_sum <- read_csv("Rparv_Fragmentation_Loss_Wide_BySeedZone.csv")
rparv_access_sum <- read_csv("R.parv Accessibility Final.csv")


# ---------------------------------------------
# SETUP: Colour Palette & Packages
# ---------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

# Custom color palette for Threshold × Species combinations
plot_colors <- c(
  "Omission_R. leprosula"   = "#D7191C",
  "Omission_R. parvifolia"  = "#A50F15",
  "EqSS_R. leprosula"       = "#2B83BA",
  "EqSS_R. parvifolia"      = "#084594",
  "DanumPlot_R. leprosula"  = "#31A354",
  "DanumPlot_R. parvifolia" = "#006D2C",
  "High08_R. leprosula"     = "#FDAE61",
  "High08_R. parvifolia"    = "#D95F0E"
)

# ---------------------------------------------
# PLOT 1: Viable Hectares by Seed Zone
# ---------------------------------------------
rlep_long <- rlep_seedzone_sum %>%
  pivot_longer(cols = ends_with("_Total_Ha"),
               names_to = "Threshold", values_to = "Viable_Ha") %>%
  mutate(Threshold = gsub("_Total_Ha", "", Threshold),
         Species = "R. leprosula")

rparv_long <- rparv_seedzone_sum %>%
  pivot_longer(cols = ends_with("_Total_Ha"),
               names_to = "Threshold", values_to = "Viable_Ha") %>%
  mutate(Threshold = gsub("_Total_Ha", "", Threshold),
         Species = "R. parvifolia")

viable_combined <- bind_rows(rlep_long, rparv_long)

ggplot(viable_combined, aes(x = as.factor(ZoneID), y = Viable_Ha,
                            color = interaction(Threshold, Species, sep = "_"),
                            group = interaction(Threshold, Species, sep = "_"))) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  scale_color_manual(values = plot_colors) +
  labs(title = "Viable Habitat Area by Seed Zone",
       x = "Seed Zone", y = "Viable Area (ha)", color = "Threshold × Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave("viable_habitat_area_by_seed_zone.jpg", width = 10, height = 8, units = "in", dpi = 300)

# For Bar Chart:
ggplot(viable_combined, aes(x = as.factor(ZoneID), y = Viable_Ha,
                            fill = interaction(Threshold, Species, sep = "_"))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = plot_colors) +
  labs(title = "Viable Habitat Area by Seed Zone",
       x = "Seed Zone", y = "Viable Area (ha)", fill = "Threshold × Species") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave("viable_habitat_area_by_seed_zone_bar.jpg", width = 10, height = 8, units = "in", dpi = 300)
# ---------------------------------------------
# PLOT 2: Fragmentation Loss % by Seed Zone
# ---------------------------------------------
# Define the color scheme
plot_colors <- c(
  "Omission_R. leprosula"   = "#D7191C",
  "Omission_P. parvifolia"  = "#A50F15",
  "EqSS_R. leprosula"       = "#2B83BA",
  "EqSS_P. parvifolia"      = "#084594",
  "DanumPlot_R. leprosula"  = "#31A354",
  "DanumPlot_P. parvifolia" = "#006D2C",
  "High08_R. leprosula"     = "#FDAE61",
  "High08_P. parvifolia"    = "#D95F0E"
)

# Process R. leprosula
rlep_frag_long <- rlep_frag_sum %>%
  pivot_longer(cols = starts_with("Frag_Loss_Pct_"),
               names_to = "Threshold", values_to = "FragLoss") %>%
  mutate(
    Threshold = gsub("Frag_Loss_Pct_", "", Threshold),
    Threshold = recode(Threshold, "Om" = "Omission", "D" = "DanumPlot"),
    Species = "R. leprosula"
  )

# Process P. parvifolia
rparv_frag_long <- rparv_frag_sum %>%
  pivot_longer(cols = starts_with("Frag_Loss_Pct_"),
               names_to = "Threshold", values_to = "FragLoss") %>%
  mutate(
    Threshold = gsub("Frag_Loss_Pct_", "", Threshold),
    Threshold = recode(Threshold, "Om" = "Omission", "D" = "DanumPlot"),
    Species = "P. parvifolia"
  )

# Combine datasets
frag_combined <- bind_rows(rlep_frag_long, rparv_frag_long) %>%
  mutate(ThresholdSpecies = paste(Threshold, Species, sep = "_"))

# Plot
ggplot(frag_combined, aes(
  x = as.factor(ZoneID),
  y = FragLoss,
  fill = ThresholdSpecies
)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = plot_colors, drop = FALSE) +
  labs(
    title = "Fragmentation Loss % by Seed Zone",
    x = "Seed Zone",
    y = "Fragmentation Loss (%)",
    fill = "Threshold × Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save output
ggsave("FragmentationLoss_percent_by_seed_zone_barchart.jpg",
       width = 10, height = 8, units = "in", dpi = 300)

# ---------------------------------------------
# PLOT 3: Accessibility – R. leprosula
# ---------------------------------------------
access_colors_rlep <- c(
  "Om. 1km" = "#D7191C",   # lighter red
  "Om. 5km" = "#A50F15",   # darker red
  "Eq. 1km" = "#2B83BA",   # lighter blue
  "Eq. 5km" = "#084594",   # darker blue
  "D. 1km"  = "#31A354",   # lighter green
  "D. 5km"  = "#006D2C",   # darker green
  "H. 1km"  = "#FDAE61",   # lighter orange
  "H. 5km"  = "#D95F0E"    # darker orange
)

rlep_access_long <- rlep_access_sum %>%
  pivot_longer(cols = -ZoneID,  # keep ZoneID column intact
               names_to = "Metric", values_to = "AccessPct") %>%
  mutate(Buffer = ifelse(grepl("1km", Metric), "1 km", "5 km"),
         Threshold = case_when(
           grepl("Om", Metric) ~ "Omission",
           grepl("Eq", Metric) ~ "EqSS",
           grepl("D", Metric)  ~ "DanumPlot",
           grepl("H", Metric)  ~ "High08",
           TRUE ~ "Unknown"
         ))

rlep_filtered <- rlep_access_long %>%
  filter(Metric %in% names(access_colors_rlep))

ggplot(rlep_filtered, aes(x = factor(ZoneID), y = AccessPct, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Accessibility of Viable Habitat – R. leprosula",
       x = "Seed Zone",
       y = "Accessible Area (ha)",
       fill = "Threshold × Buffer") +
  scale_fill_manual(values = access_colors_rlep, na.value = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave("Accessible_Rlep_by_seed_zone_barchart.jpg",
       width = 10, height = 8, units = "in", dpi = 300)
# ---------------------------------------------
# PLOT 4: Accessibility – R. parvifolia
# ---------------------------------------------
access_colors_rparv <- c(
  "Om. 1km" = "#D7191C",   # lighter red
  "Om. 5km" = "#A50F15",   # darker red
  "Eq. 1km" = "#2B83BA",   # lighter blue
  "Eq. 5km" = "#084594",   # darker blue
  "D. 1km"  = "#31A354",   # lighter green
  "D. 5km"  = "#006D2C",   # darker green
  "H. 1km"  = "#FDAE61",   # lighter orange
  "H. 5km"  = "#D95F0E"    # darker orange
)

rparv_access_long <- rparv_access_sum %>%
  pivot_longer(cols = -ZoneID,  # keep ZoneID column intact
               names_to = "Metric", values_to = "AccessPct") %>%
  mutate(Buffer = ifelse(grepl("1km", Metric), "1 km", "5 km"),
         Threshold = case_when(
           grepl("Om", Metric) ~ "Omission",
           grepl("Eq", Metric) ~ "EqSS",
           grepl("D", Metric)  ~ "DanumPlot",
           grepl("H", Metric)  ~ "High08",
           TRUE ~ "Unknown"
         ))

rparv_filtered <- rparv_access_long %>%
  filter(Metric %in% names(access_colors_rparv))

ggplot(rparv_filtered, aes(x = factor(ZoneID), y = AccessPct, fill = Metric)) +
  geom_col(position = "dodge") +
  labs(title = "Accessibility of Viable Habitat – R. parvifolia",
       x = "Seed Zone",
       y = "Accessible Area (ha)",
       fill = "Threshold × Buffer") +
  scale_fill_manual(values = access_colors_rparv, na.value = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggsave("Accessible_Rparv_by_seed_zone_barchart.jpg",
       width = 10, height = 8, units = "in", dpi = 300)




