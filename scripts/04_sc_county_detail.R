# ============================================================
# 04_sc_county_detail.R
# Zoom-in figure: South Carolina county-level MMR coverage,
# highlighting Spartanburg County (the confirmed outbreak epicenter).
# ============================================================
#
# WHY THIS SCRIPT EXISTS:
#   The state-level analysis (03_join_and_visualize.R) found South
#   Carolina's statewide average MMR coverage (0.951) sits just ABOVE
#   the 95% herd-immunity threshold, despite having the single largest
#   2026 outbreak (686 cases) in the dataset -- a surprising result
#   given the overall weak state-level relationship. This script
#   zooms into SC's 46 counties to show what the state average hides.
#
# EXTERNAL VALIDATION (see README/chat history for sources):
#   Multiple independent sources (NPR, CDC scenario assessments, SC
#   Dept. of Public Health, ABC News) confirm the outbreak was
#   centered in Spartanburg County, whose own school-level MMR rate
#   (reported externally as 88.9%) is well below the state average.
#   This script checks whether Spartanburg shows up as a low-coverage
#   outlier in OUR dataset too -- it does (0.900, 3rd-lowest of 46).
#
# IMPORTANT CAVEAT -- DON'T OVERCLAIM:
#   Jasper (0.825) and Fairfield (0.894) counties have LOWER coverage
#   than Spartanburg (0.900) in this dataset, yet the outbreak did not
#   occur there. Low coverage is evidently NECESSARY but not
#   SUFFICIENT -- an actual introduction/index case is also required
#   to ignite an outbreak. This script labels those counties too, so
#   the figure doesn't imply "lowest coverage = where outbreaks
#   happen," only "outbreaks happen below the threshold."
# ============================================================

library(tidyverse)

sy_cols <- c("SY2017_18","SY2018_19","SY2019_20","SY2020_21",
             "SY2021_22","SY2022_23","SY2023_24","SY2024_25")

mmr_raw <- read_csv("data/raw/mmr_data_us_counties_v2.csv", show_col_types = FALSE,
                     locale = locale(encoding = "UTF-8"),
                     col_types = cols(
                       FIPS = col_character(),
                       SY2017_18 = col_character(),
                       SY2018_19 = col_character(),
                       SY2019_20 = col_character(),
                       SY2020_21 = col_character(),
                       SY2021_22 = col_character(),
                       SY2022_23 = col_character(),
                       SY2023_24 = col_character(),
                       SY2024_25 = col_character(),
                       .default = col_guess()
                     ))

# --- Filter to South Carolina, most recent year (2024-25) ---
# (SC is not one of the LA/MO/UT methodology-mismatch states, but we
# still defensively strip any trailing "*" in case that changes with a
# future data update.)
sc_counties <- mmr_raw %>%
  filter(State == "SC") %>%
  mutate(mmr_rate = as.numeric(str_remove(SY2024_25, "\\*$"))) %>%
  filter(!is.na(mmr_rate)) %>%
  select(County, mmr_rate)

cat("SC counties with 2024-25 data:", nrow(sc_counties), "\n")

sc_state_avg <- mean(sc_counties$mmr_rate)
cat("SC statewide average (unweighted):", round(sc_state_avg, 3), "\n")

# --- Flag counties of interest for annotation ---
outbreak_county <- "Spartanburg"
lower_than_outbreak <- sc_counties %>%
  filter(mmr_rate < sc_counties$mmr_rate[sc_counties$County == outbreak_county]) %>%
  pull(County)

cat("Counties with LOWER coverage than Spartanburg but no major outbreak reported:\n")
print(lower_than_outbreak)

sc_counties <- sc_counties %>%
  mutate(
    highlight = case_when(
      County == outbreak_county ~ "Outbreak epicenter (Spartanburg)",
      County %in% lower_than_outbreak ~ "Lower coverage, no major outbreak",
      TRUE ~ "Other SC counties"
    ),
    County = fct_reorder(County, mmr_rate)
  )

# --- Horizontal bar chart, sorted by coverage ---
# NOTE ON THE TWO REFERENCE LINES: the 95% threshold and SC's actual
# state average (0.951) are extremely close together (~0.1 percentage
# points apart), so their labels are placed in a right-side margin
# (rather than directly on the lines, rotated) to avoid overlapping
# each other or the bars. x-axis limits are extended slightly past
# 100% to make room for that margin.
n_counties <- nrow(sc_counties)

p <- ggplot(sc_counties, aes(x = mmr_rate, y = County, fill = highlight)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = sc_state_avg, linetype = "dotted", color = "gray30", linewidth = 0.5) +
  annotate("label", x = 1.01, y = n_counties - 1, label = "95% threshold",
           hjust = 0, size = 3, color = "black", label.size = 0, fill = "white") +
  annotate("label", x = 1.01, y = n_counties - 4,
           label = paste0("SC state avg: ", scales::percent(sc_state_avg, accuracy = 0.1)),
           hjust = 0, size = 3, color = "gray30", label.size = 0, fill = "white") +
  scale_fill_manual(values = c(
    "Outbreak epicenter (Spartanburg)" = "firebrick",
    "Lower coverage, no major outbreak" = "goldenrod2",
    "Other SC counties" = "steelblue4"
  )) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.16),
                      breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(
    title = "South Carolina County-Level MMR Coverage (2024-25)",
    subtitle = "The state average (dotted line) sits just above the 95% threshold,\nbut masks Spartanburg County -- the confirmed epicenter of the 2025-2026 outbreak",
    x = "2-dose MMR coverage",
    y = NULL,
    fill = NULL,
    caption = paste(
      "Source: JHU CSSE county-level MMR dataset (school year 2024-25).",
      "Outbreak location confirmed via CDC, SC DPH, and NPR reporting (see project README).",
      "Note: low coverage is necessary but not sufficient -- Jasper and Fairfield have lower",
      "coverage than Spartanburg but were not the outbreak site.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 7)
  )

print(p)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/sc_county_mmr_detail_20260704.png", plot = p,
       width = 8, height = 10, dpi = 300)

write_csv(sc_counties, "data/clean/sc_county_mmr_20260704.csv")
