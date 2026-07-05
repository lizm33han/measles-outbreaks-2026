# ============================================================
# 05_pa_county_detail.R
# Zoom-in figure: Pennsylvania county-level MMR coverage,
# highlighting the Lancaster-Lebanon outbreak cluster.
# ============================================================
#
# WHY THIS SCRIPT EXISTS:
#   Same idea as 04_sc_county_detail.R -- checking whether PA's 2026
#   measles outbreak (concentrated in Lancaster and Lebanon counties)
#   shows up as a low-coverage outlier at the county level, the way
#   Spartanburg did in South Carolina.
#
# EXTERNAL VALIDATION (see README for sources):
#   Multiple sources (PA Dept. of Health, WHYY, Philadelphia Inquirer,
#   Global Biodefense) confirm PA's 2026 outbreak began in Lancaster
#   County in late April and spread to neighboring Lebanon County. Of
#   84 statewide cases as of late June: 41 in Lancaster, and 72 total
#   across the Lancaster-Lebanon cluster (implying ~31 in Lebanon and
#   nearby counties). UNLIKE South Carolina's single-county epicenter,
#   this is a TWO-COUNTY cluster with meaningfully DIFFERENT coverage
#   rates -- worth showing as two distinct categories, not one.
#
# WHAT THIS SCRIPT FINDS (checked against the data before writing this):
#   - Lancaster: 0.885 -- 8th-lowest of PA's 67 counties, clearly below
#     both the 95% threshold AND the state average (0.932).
#   - Lebanon: 0.932 -- statistically unremarkable, right at the state
#     average. Despite being heavily affected by the SAME outbreak
#     cluster, Lebanon's OWN coverage rate does not stand out the way
#     Lancaster's does. This is a genuinely different pattern from SC
#     (where Spartanburg alone was both the epicenter AND a clear
#     low-coverage outlier) -- worth calling out explicitly rather than
#     assuming the SC story repeats exactly.
#
# CAVEAT -- DON'T OVERCLAIM (same as SC script):
#   7 PA counties have LOWER coverage than Lancaster (Cameron, Dauphin,
#   Fulton, Monroe, Pike, Snyder, Wayne) yet none were the outbreak
#   site. Low coverage remains necessary but not sufficient.
# ============================================================

library(tidyverse)

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

# --- Filter to Pennsylvania, most recent year (2024-25) ---
pa_counties <- mmr_raw %>%
  filter(State == "PA") %>%
  mutate(mmr_rate = as.numeric(str_remove(SY2024_25, "\\*$"))) %>%
  filter(!is.na(mmr_rate)) %>%
  select(County, mmr_rate)

cat("PA counties with 2024-25 data:", nrow(pa_counties), "\n")

pa_state_avg <- mean(pa_counties$mmr_rate)
cat("PA statewide average (unweighted):", round(pa_state_avg, 3), "\n")

# --- Flag the outbreak cluster and lower-coverage-but-unaffected counties ---
primary_epicenter <- "Lancaster"
secondary_hotspot <- "Lebanon"

lancaster_rate <- pa_counties$mmr_rate[pa_counties$County == primary_epicenter]

lower_than_epicenter <- pa_counties %>%
  filter(mmr_rate < lancaster_rate, !County %in% c(primary_epicenter, secondary_hotspot)) %>%
  pull(County)

cat("Counties with LOWER coverage than Lancaster but no major outbreak reported:\n")
print(lower_than_epicenter)

pa_counties <- pa_counties %>%
  mutate(
    highlight = case_when(
      County == primary_epicenter ~ "Primary epicenter (Lancaster)",
      County == secondary_hotspot ~ "Secondary hotspot (Lebanon)",
      County %in% lower_than_epicenter ~ "Lower coverage, no major outbreak",
      TRUE ~ "Other PA counties"
    ),
    County = fct_reorder(County, mmr_rate)
  )

# --- Horizontal bar chart, sorted by coverage ---
n_counties <- nrow(pa_counties)

p <- ggplot(pa_counties, aes(x = mmr_rate, y = County, fill = highlight)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = pa_state_avg, linetype = "dotted", color = "gray30", linewidth = 0.5) +
  annotate("label", x = 1.01, y = n_counties - 1, label = "95% threshold",
           hjust = 0, size = 3, color = "black", label.size = 0, fill = "white") +
  annotate("label", x = 1.01, y = n_counties - 4,
           label = paste0("PA state avg: ", scales::percent(pa_state_avg, accuracy = 0.1)),
           hjust = 0, size = 3, color = "gray30", label.size = 0, fill = "white") +
  scale_fill_manual(values = c(
    "Primary epicenter (Lancaster)" = "firebrick",
    "Secondary hotspot (Lebanon)" = "darkorange2",
    "Lower coverage, no major outbreak" = "goldenrod2",
    "Other PA counties" = "steelblue4"
  )) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.16),
                      breaks = c(0, 0.25, 0.5, 0.75, 1.0)) +
  labs(
    title = "Pennsylvania County-Level MMR Coverage (2024-25)",
    subtitle = paste0(
      "Lancaster sits clearly below the state average, but Lebanon --\n",
      "also heavily affected by the same outbreak cluster -- does not stand out"
    ),
    x = "2-dose MMR coverage",
    y = NULL,
    fill = NULL,
    caption = paste(
      "Source: JHU CSSE county-level MMR dataset (school year 2024-25).",
      "Outbreak location confirmed via PA Dept. of Health, WHYY, and Philadelphia Inquirer reporting (see project README).",
      "Note: low coverage is necessary but not sufficient -- 7 PA counties have lower",
      "coverage than Lancaster but were not part of the outbreak.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.y = element_text(size = 6)
  )

print(p)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/pa_county_mmr_detail_20260704.png", plot = p,
       width = 8, height = 11, dpi = 300)

write_csv(pa_counties, "data/clean/pa_county_mmr_20260704.csv")
