# ============================================================
# 03_join_and_visualize.R
# Join state-level MMR coverage (Track A snapshot) with NNDSS
# measles case counts, and build the coverage-vs-cases scatterplot.
# ============================================================
#
# GEOGRAPHY / KEY MISMATCH NOTE:
#   - nndss_measles_clean_20260704.csv uses FULL STATE NAMES
#     (e.g. "Michigan"), produced by 01_clean_nndss_measles.R.
#   - mmr_state_snapshot_20260704.csv uses STATE ABBREVIATIONS
#     (e.g. "MI"), produced by 02_clean_mmr_coverage.R.
#   A crosswalk is built below using base R's built-in state.name /
#   state.abb vectors (plus DC, which isn't in either base R vector).
#
# WHICH CASE COUNT TO USE:
#   Track A's MMR snapshot is each state's MOST RECENT available
#   school year (mostly 2024-25, some earlier -- see
#   most_recent_year_used column). To keep the timing roughly aligned,
#   this script uses each state's 2026 cumulative YTD case count
#   (most recent reported MMWR week) rather than a 5-year sum, since
#   that better reflects "current coverage vs. the current outbreak."
#   A 5-year cumulative (2022-2026) total is also included as an
#   alternate column in case a different comparison is wanted later.
# ============================================================

library(tidyverse)

nndss <- read_csv("data/clean/nndss_measles_clean_20260704.csv", show_col_types = FALSE)
mmr_snapshot <- read_csv("data/clean/mmr_state_snapshot_20260704.csv", show_col_types = FALSE)

# --- Build state name <-> abbreviation crosswalk ---
state_crosswalk <- tibble(
  state_name = c(state.name, "District of Columbia"),
  state_abb  = c(state.abb, "DC")
)

# --- Most recent 2026 cumulative case count per state ---
cases_2026_latest <- nndss %>%
  filter(mmwr_year == 2026) %>%
  group_by(state) %>%
  filter(mmwr_week == max(mmwr_week)) %>%
  ungroup() %>%
  select(state, cases_2026_ytd = measles_cum_ytd_total)

# --- 5-year cumulative total (2022-2026), as an alternate measure ---
# BUG FIX: must group by state AND mmwr_year before taking the max week,
# otherwise max(mmwr_week) is computed across ALL years mixed together
# (week numbers reset each year, 1-52), which grabs whichever single
# week-52 rows exist from complete prior years and effectively DROPS
# the current partial year (2026, which has only reached ~week 25) from
# the sum almost entirely. Grouping by state + year first ensures we get
# exactly one (the last reported) row per state per year, then sum those.
cases_5yr_total <- nndss %>%
  group_by(state, mmwr_year) %>%
  filter(mmwr_week == max(mmwr_week)) %>%   # last reported week of THIS year
  ungroup() %>%
  group_by(state) %>%
  summarise(cases_5yr_cumulative = sum(measles_cum_ytd_total), .groups = "drop")

# --- Join everything together on state abbreviation ---
coverage_vs_cases <- mmr_snapshot %>%
  left_join(state_crosswalk, by = c("State" = "state_abb")) %>%
  left_join(cases_2026_latest, by = c("state_name" = "state")) %>%
  left_join(cases_5yr_total, by = c("state_name" = "state")) %>%
  filter(!is.na(cases_2026_ytd))  # drop states with no NNDSS match (shouldn't be many -- check!)

cat("States in final joined dataset:", nrow(coverage_vs_cases), "\n")
cat("States dropped from MMR snapshot (no NNDSS match):",
    nrow(mmr_snapshot) - nrow(coverage_vs_cases), "\n")

# --- Quick correlation check before plotting ---
cor_test <- cor.test(coverage_vs_cases$mean_mmr_rate, coverage_vs_cases$cases_2026_ytd,
                      method = "spearman")
print(cor_test)
# Spearman (rank-based) is used rather than Pearson because case counts
# are heavily right-skewed (a few states with large outbreaks, most with
# very few cases) -- a rank correlation is more robust to that than a
# linear one.
#
# NOTE ON THE RESULT: a near-zero, non-significant Spearman rho here
# does NOT mean coverage is irrelevant. The scatterplot shows a
# TRIANGLE/THRESHOLD pattern rather than a smooth linear one: every
# large outbreak sits below the 95% herd-immunity line, but most
# states below 95% still have near-zero cases. A rank correlation
# assumes a roughly monotonic relationship and doesn't capture a
# "necessary but not sufficient" / gatekeeper pattern like this. The
# grouped comparison below tests that threshold idea more directly.

# --- Grouped (below vs. at/above 95%) comparison ---
# Tests the "threshold effect" idea directly: does the DISTRIBUTION of
# case counts differ between low- and high-coverage states, rather
# than assuming a smooth relationship across the whole coverage range.
coverage_vs_cases <- coverage_vs_cases %>%
  mutate(coverage_group = if_else(mean_mmr_rate < 0.95, "Below 95%", "At/above 95%"))

group_summary <- coverage_vs_cases %>%
  group_by(coverage_group) %>%
  summarise(
    n_states = n(),
    median_cases = median(cases_2026_ytd),
    max_cases = max(cases_2026_ytd),
    mean_cases = mean(cases_2026_ytd),
    .groups = "drop"
  )

cat("\n=== Case counts by coverage group ===\n")
print(group_summary)

wilcox_test <- wilcox.test(cases_2026_ytd ~ coverage_group, data = coverage_vs_cases)
cat("\n")
print(wilcox_test)
# Wilcoxon rank-sum (Mann-Whitney) compares whether the case-count
# DISTRIBUTION differs between the two groups -- a more direct test of
# "does crossing the 95% threshold matter" than a continuous correlation.

# --- Identify states to label: top 8 by case count ---
states_to_label <- coverage_vs_cases %>%
  slice_max(order_by = cases_2026_ytd, n = 8)

# --- Scatterplot ---
# Requires ggrepel for non-overlapping labels (Virginia/Florida sit right
# on top of each other with plain geom_text): install.packages("ggrepel")
# if not already installed.
library(ggrepel)

p <- ggplot(coverage_vs_cases, aes(x = mean_mmr_rate, y = cases_2026_ytd)) +
  geom_vline(xintercept = 0.95, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
  annotate("text", x = 0.95, y = max(coverage_vs_cases$cases_2026_ytd) * 0.95,
           label = "95% herd immunity threshold", hjust = 1.05, size = 3.2, color = "firebrick") +
  geom_point(aes(color = coverage_group), size = 2.5, alpha = 0.8) +
  geom_text_repel(data = states_to_label, aes(label = state_name),
                   size = 3, color = "black", max.overlaps = 20,
                   box.padding = 0.5, seed = 42) +
  scale_color_manual(values = c("Below 95%" = "steelblue4", "At/above 95%" = "gray60")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "State MMR Coverage vs. 2026 Measles Case Counts",
    subtitle = "Each point is one state; coverage is most recent available county-average (Track A snapshot)",
    x = "Mean 2-dose MMR coverage (most recent available county-average)",
    y = "Cumulative measles cases, 2026 (NNDSS, most recent reported week)",
    color = "Coverage group",
    caption = "Sources: CDC NNDSS Weekly Data; JHU CSSE county-level MMR dataset.\nMMR coverage is an unweighted mean across reporting counties, not population-weighted."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

print(p)

# Assumes an output/figures/ folder exists in the project (not
# specified as an existing convention, so this is a reasonable default
# -- rename the path below if you'd rather store figures elsewhere).
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/coverage_vs_cases_scatterplot_20260704.png", plot = p,
       width = 9, height = 6.5, dpi = 300)

write_csv(coverage_vs_cases, "data/clean/coverage_vs_cases_joined_20260704.csv")
