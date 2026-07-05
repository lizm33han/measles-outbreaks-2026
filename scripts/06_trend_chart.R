# ============================================================
# 06_trend_chart.R
# Track B trend line: MMR coverage over time (school years
# 2017-18 through 2024-25), for the 35-state longitudinal subset.
# ============================================================
#
# WHY THIS SCRIPT EXISTS:
#   Scripts 03-05 all address the CROSS-SECTIONAL half of the research
#   question ("how does coverage relate to case counts"). This script
#   addresses the other half -- "how has coverage changed over time" --
#   which up to now has only been supported by externally-cited figures
#   in the report's Introduction, not by this project's own data.
#
# WHICH STATES ARE SHOWN:
#   - A bold "Track B average" line: the mean of all 35 states' own
#     year-by-year averages (an average of state averages, consistent
#     with the unweighted-by-population approach used throughout this
#     project -- NOT population-weighted).
#   - Four individual state lines, chosen because they're the outbreak
#     states examined in Findings 1-3 of the report: South Carolina,
#     Pennsylvania, Texas, and Utah. All four happen to qualify for
#     Track B (>=5 of 8 years), which was confirmed before writing this
#     script rather than assumed.
#   - Gaps in a state's own line (e.g. SC/TX have no data before
#     2019-20; Utah's earliest usable year is 2018-19 since 2017-18 is
#     excluded as a methodology-mismatch year -- see 02_clean_mmr_coverage.R)
#     are left as actual gaps rather than interpolated, so the chart
#     doesn't imply data that doesn't exist.
# ============================================================

library(tidyverse)

mmr_trend <- read_csv("data/clean/mmr_state_trend_20260704.csv", show_col_types = FALSE)

cat("States in Track B trend subset:", n_distinct(mmr_trend$State), "\n")

highlight_states <- c("SC", "PA", "TX", "UT")
missing_check <- setdiff(highlight_states, unique(mmr_trend$State))
if (length(missing_check) > 0) {
  warning("These highlight states are NOT in the Track B subset: ", paste(missing_check, collapse = ", "))
}

# --- "Track B average" line: mean of all 35 states' own year averages ---
track_b_avg <- mmr_trend %>%
  group_by(school_year) %>%
  summarise(mean_mmr_rate = mean(mean_mmr_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(State = "Track B average (35 states)")

# --- Combine highlight states + average line into one plotting frame ---
plot_data <- mmr_trend %>%
  filter(State %in% highlight_states) %>%
  select(State, school_year, mean_mmr_rate) %>%
  bind_rows(track_b_avg) %>%
  mutate(
    school_year = factor(school_year, levels = c(
      "2017-18","2018-19","2019-20","2020-21",
      "2021-22","2022-23","2023-24","2024-25"
    )),
    line_group = if_else(State == "Track B average (35 states)", "Average", "Highlighted state")
  )

# --- Line chart ---
p <- ggplot(plot_data, aes(x = school_year, y = mean_mmr_rate, group = State,
                            color = State, linewidth = line_group)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  # annotate() is placed AFTER the geom_line/geom_point layers on purpose:
  # ggplot2 infers the x-axis scale type (discrete vs. continuous) from the
  # FIRST layer with a mapped x value, in the order layers are added. Since
  # annotate()'s x = 1 is a bare numeric, putting it before the factor-x
  # layers made ggplot lock in a continuous scale, which then broke when it
  # hit the "2017-18" factor labels. Explicit scale_x_discrete() below is a
  # second, more robust safeguard against this regardless of layer order.
  annotate("text", x = 1, y = 0.955, label = "95% herd immunity threshold",
           hjust = 0, size = 3.2, color = "gray40") +
  scale_x_discrete() +
  scale_linewidth_manual(values = c("Average" = 1.4, "Highlighted state" = 0.8), guide = "none") +
  scale_color_manual(values = c(
    "Track B average (35 states)" = "black",
    "SC" = "firebrick",
    "PA" = "darkorange2",
    "TX" = "steelblue4",
    "UT" = "seagreen4"
  ), labels = c(
    "Track B average (35 states)" = "Track B average (35 states)",
    "SC" = "South Carolina",
    "PA" = "Pennsylvania",
    "TX" = "Texas",
    "UT" = "Utah"
  )) +
  scale_y_continuous(labels = scales::percent, limits = c(0.8, 1)) +
  labs(
    title = "MMR Coverage Over Time, 2017-18 to 2024-25",
    subtitle = "Track B subset (states with data in >=5 of 8 school years); gaps reflect years without usable data",
    x = "School year", y = "2-dose MMR coverage (state average)", color = NULL,
    caption = paste(
      "Source: JHU CSSE county-level MMR dataset.",
      "Track B average is an unweighted mean of 35 states' own averages, not population-weighted.",
      "Gaps (e.g. SC/TX pre-2019-20, UT 2017-18) reflect missing or excluded data, not zero coverage.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
ggsave("output/figures/mmr_trend_chart_20260704.png", plot = p, width = 9, height = 6.5, dpi = 300)

write_csv(plot_data, "data/clean/mmr_trend_plot_data_20260704.csv")
