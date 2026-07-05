# ============================================================
# 02_clean_mmr_coverage.R
# Clean county-level MMR (2-dose) vaccination coverage data
# and aggregate to STATE level using a two-track approach.
# ============================================================
#
# DATA PROVENANCE NOTE (verify against README.md before use):
#   The two source files are MISLABELED relative to the repo's own
#   README.md. In practice:
#     - `mmr_data_us_counties_v2.csv`  = the actual rates data
#        (FIPS, County, State, SY2017_18 ... SY2024_25)
#     - `mmr_data_sources_v2.csv`      = the data dictionary / per-state
#        source notes (NOT rate data)
#   This script uses the files by their ACTUAL content, not their names.
#
# KNOWN LIMITATIONS:
#   - Arkansas has no county-level data at all (per data dictionary notes).
#   - Spatial unit is NOT uniform across states -- most report by county,
#     but at least one state (Alaska) reports by health region instead.
#     Treat cross-state comparisons of county-level detail with that
#     caveat in mind; state-level aggregates below are still valid.
#   - No population weights are available in this dataset, so state
#     averages below are SIMPLE (unweighted) means across reporting
#     counties, not population-weighted. This means a state with many
#     small rural counties and one huge urban county will have its
#     average pulled toward the rural counties. Document this as a
#     limitation in the report, don't silently present it as
#     population-weighted coverage.
#
# TWO-TRACK APPROACH (decided after examining missingness):
#   Track A (SNAPSHOT / cross-sectional): most recent available year of
#     data per county -> one current-ish coverage estimate per state.
#     Maximizes state coverage (all 47 states + DC represented).
#   Track B (TREND / longitudinal): restrict to counties with data in
#     at least 5 of the 8 school years, so year-over-year comparisons
#     of the SAME counties aren't confounded by which counties happen
#     to report each year. This drops many states -- print exactly
#     which ones survive so the trend claims are honestly scoped.
#
# METHODOLOGY-MISMATCH FLAG (trailing "*" in the SY columns):
#   The source data dictionary (mmr_data_sources_v2.csv) uses a trailing
#   "*" to mark state/year combinations where the value is NOT
#   comparable to a true 2-dose MMR rate, due to a documented change in
#   how that state collected/aggregated the data. Confirmed via the
#   per-state notes fields, THREE state/year combinations are flagged
#   this way (not just one -- checked every SY column, not just the
#   first one found):
#     - Louisiana, SY2020-21 & SY2021-22: reflects a COMBINED
#       vaccine-series rate (DTaP/Polio/HepB/MMR/VAR/Tdap/MenACWY
#       bundled together), not a true 2-dose MMR rate.
#     - Missouri, SY2019-20: an old aggregation method later corrected;
#       "not comparable to later 2-dose MMR data due to the differing
#       methodology" (state's own wording).
#     - Utah, SY2017-18 (i.e. data "prior to 2018-19"): mixed
#       county-level and school-district-level rates, later replaced
#       with a consistent county-level methodology.
#   If left alone, read_csv() would silently coerce these to NA anyway
#   (guesses the SY columns are numeric, "0.932*" fails to parse) -- but
#   relying on that side effect is fragile and undocumented, and doesn't
#   generalize (it would only catch cells that literally have a "*";
#   it wouldn't flag a state/year with a real methodology break but no
#   asterisk). We read the SY columns as CHARACTER explicitly and then
#   EXPLICITLY null out all three flagged combinations below.
# ============================================================

library(tidyverse)

sy_cols <- c("SY2017_18","SY2018_19","SY2019_20","SY2020_21",
             "SY2021_22","SY2022_23","SY2023_24","SY2024_25")

# Read SY columns as character so asterisked values ("0.932*") come in
# as literal text instead of triggering silent parsing-failure NAs.
# (Specifying col_character() for these columns UP FRONT avoids the
# warning entirely, rather than letting readr guess "double", fail on
# the asterisked cells, and THEN converting -- that would still trigger
# the parsing-failure warning during read_csv itself.)
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

# Lookup table of state/school-year combinations documented in the data
# dictionary as methodology-mismatched -- confirmed by checking every
# starred cell in the raw file, not assumed from a single example.
methodology_mismatch <- tribble(
  ~State, ~school_year,
  "LA",   "2020-21",
  "LA",   "2021-22",
  "MO",   "2019-20",
  "UT",   "2017-18"
)

# --- Reshape to long format: one row per county x school year ---
mmr_long <- mmr_raw %>%
  pivot_longer(cols = all_of(sy_cols), names_to = "school_year", values_to = "mmr_rate_raw") %>%
  mutate(
    school_year = str_remove(school_year, "^SY"),
    school_year = str_replace(school_year, "_", "-"),   # e.g. "2017-18"
    is_methodology_mismatch = paste(State, school_year) %in%
      paste(methodology_mismatch$State, methodology_mismatch$school_year),
    mmr_rate = if_else(
      is_methodology_mismatch,
      NA_character_,
      str_remove(mmr_rate_raw, "\\*$")   # strip any other stray asterisks defensively
    ),
    mmr_rate = as.numeric(mmr_rate)
  ) %>%
  filter(!is.na(State))

n_excluded <- sum(mmr_long$is_methodology_mismatch, na.rm = TRUE)
cat("Excluded", n_excluded, "county-years flagged as methodology-mismatched",
    "(LA 2020-21/2021-22, MO 2019-20, UT 2017-18)\n")

# ============================================================
# TRACK A: Snapshot state coverage (most recent year available
#          per county, then simple mean across counties per state)
# ============================================================
mmr_snapshot_county <- mmr_long %>%
  filter(!is.na(mmr_rate)) %>%
  group_by(FIPS, County, State) %>%
  slice_max(order_by = school_year, n = 1, with_ties = FALSE) %>%
  ungroup()

mmr_snapshot_state <- mmr_snapshot_county %>%
  group_by(State) %>%
  summarise(
    n_counties_reporting = n(),
    most_recent_year_used = paste(sort(unique(school_year)), collapse = "; "),
    mean_mmr_rate = mean(mmr_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(State)

cat("=== TRACK A: Snapshot state coverage ===\n")
cat("States represented:", nrow(mmr_snapshot_state), "out of 51 (50 states + DC)\n")
print(mmr_snapshot_state, n = 51)

# ============================================================
# TRACK B: Trend subset (counties with data in >= 5 of 8 years)
# ============================================================
county_year_counts <- mmr_long %>%
  filter(!is.na(mmr_rate)) %>%
  group_by(FIPS, County, State) %>%
  summarise(n_years = n(), .groups = "drop")

trend_counties <- county_year_counts %>% filter(n_years >= 5)

mmr_trend_long <- mmr_long %>%
  semi_join(trend_counties, by = c("FIPS", "County", "State")) %>%
  filter(!is.na(mmr_rate))

mmr_trend_state_year <- mmr_trend_long %>%
  group_by(State, school_year) %>%
  summarise(
    n_counties = n(),
    mean_mmr_rate = mean(mmr_rate, na.rm = TRUE),
    .groups = "drop"
  )

states_in_trend <- sort(unique(mmr_trend_state_year$State))
cat("\n=== TRACK B: Trend subset (>=5 of 8 years, same counties tracked) ===\n")
cat("States with enough longitudinal data:", length(states_in_trend), "out of 51\n")
print(states_in_trend)

write_csv(mmr_snapshot_state, "data/clean/mmr_state_snapshot_20260704.csv")
write_csv(mmr_trend_state_year, "data/clean/mmr_state_trend_20260704.csv")
