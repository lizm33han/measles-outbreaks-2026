# ============================================================
# 01_clean_nndss_measles.R
# Clean and reshape CDC NNDSS weekly measles data (2022-2026)
# ============================================================
#
# GOAL: produce one tidy data frame with one row per
#       state x MMWR year x week, with clean numeric case counts
#       for Measles Imported + Indigenous combined.
#
# KNOWN NNDSS PITFALLS (same family of issues as the
# cyclosporiasis project, see that project's README):
#   1. Inconsistent capitalization of `Reporting Area` across years
#      (e.g. "MICHIGAN" in 2022-2025 vs "Michigan" in 2026).
#      FIX: match on toupper(), display with str_to_title() after filtering.
#   2. Thousands-separator commas stored as TEXT in numeric columns
#      (e.g. "1,789"). FIX: str_remove_all(",") before as.numeric().
#   3. Flag columns (`*_flag`) don't uniformly mean zero -- a populated
#      numeric value always takes precedence over its flag.
#   4. `Reporting Area` mixes actual states with:
#        - regional aggregates (NEW ENGLAND, PACIFIC, MOUNTAIN, ...)
#        - national aggregates (TOTAL, US RESIDENTS, US TERRITORIES)
#        - territories (PUERTO RICO, GUAM, AMERICAN SAMOA, ...)
#        - "NEW YORK CITY" reported SEPARATELY from "NEW YORK"
#      All of these must be excluded/handled explicitly or they will
#      contaminate a state-level analysis.
#
# OPEN QUESTION TO VERIFY (documented, not silently assumed):
#   Does NNDSS's "New York" state row already INCLUDE New York City,
#   or does it EXCLUDE it (with "New York City" reported as a fully
#   separate supplementary figure)? This has flipped historically for
#   different notifiable diseases. Default here: treat "New York" and
#   "New York City" as SEPARATE non-overlapping figures and SUM them
#   into one NY state total. Verify against CDC NNDSS documentation
#   before relying on NY-specific findings.
# ============================================================

library(tidyverse)

raw <- read_csv("data/raw/NNDSS_Weekly_Data_20260704.csv", show_col_types = FALSE)

# --- 1. Standardize column names for easier reference ---
nndss <- raw %>%
  rename(
    reporting_area = `Reporting Area`,
    mmwr_year       = `Current MMWR Year`,
    mmwr_week       = `MMWR WEEK`,
    label           = Label,
    current_week    = `Current week`,
    current_flag    = `Current week, flag`,
    cum_ytd_current = `Cumulative YTD Current MMWR Year`,
    cum_ytd_flag    = `Cumulative YTD Current MMWR Year, flag`
  )

# --- 2. Define the 50 states + DC (matched via UPPERCASE to dodge the
#        capitalization inconsistency entirely) ---
us_states_upper <- toupper(c(state.name, "District of Columbia"))

# Reporting areas that are NOT states and must be excluded from state analysis
non_state_areas_upper <- toupper(c(
  "New England", "Middle Atlantic", "East North Central", "East South Central",
  "West North Central", "West South Central", "South Atlantic", "Mountain", "Pacific",
  "Total", "US Residents", "U.S. Residents", "Non-US Residents", "Non-U.S. Residents",
  "US Territories", "U.S. Territories",
  "Puerto Rico", "Guam", "American Samoa", "U.S. Virgin Islands",
  "Commonwealth of Northern Mariana Islands", "Northern Mariana Islands"
))

# --- 3. Filter to measles rows, exclude non-state aggregates ---
nndss_states <- nndss %>%
  filter(label %in% c("Measles, Imported", "Measles, Indigenous")) %>%
  mutate(area_upper = toupper(reporting_area)) %>%
  filter(area_upper %in% us_states_upper | area_upper == "NEW YORK CITY") %>%
  mutate(
    # collapse "NEW YORK CITY" into "NEW YORK" -- see open question above
    state_upper = if_else(area_upper == "NEW YORK CITY", "NEW YORK", area_upper),
    state = str_to_title(state_upper),
    state = case_when(
      state == "District Of Columbia" ~ "District of Columbia",
      TRUE ~ state
    )
  )

# --- 4. Clean numeric columns: strip commas, coerce to numeric,
#        apply flag-precedence rule (populated value wins over flag) ---
clean_numeric <- function(x) as.numeric(str_remove_all(x, ","))

nndss_clean <- nndss_states %>%
  mutate(
    current_week_n    = clean_numeric(current_week),
    cum_ytd_current_n = clean_numeric(cum_ytd_current),
    # if no numeric value AND flag is "-", treat as true zero;
    # if no numeric value and NO flag either, treat as NA (not reported)
    current_week_n = case_when(
      !is.na(current_week_n)                  ~ current_week_n,
      is.na(current_week_n) & current_flag == "-" ~ 0,
      TRUE                                     ~ NA_real_
    )
  ) %>%
  select(state, mmwr_year, mmwr_week, label, current_week_n, cum_ytd_current_n)

# --- 5. Combine Imported + Indigenous into one weekly total per state,
#        keep the two labels available separately too ---
nndss_wide <- nndss_clean %>%
  group_by(state, mmwr_year, mmwr_week) %>%
  summarise(
    measles_current_week_total = sum(current_week_n, na.rm = TRUE),
    measles_cum_ytd_total      = sum(cum_ytd_current_n, na.rm = TRUE),
    .groups = "drop"
  )

# --- 6. Sanity check: national weekly total vs published CDC figure ---
# CDC's dedicated measles dashboard reported 2,170 cases by 2026-07-02.
# NNDSS "Total" aggregate row (excluded above) should be compared against
# summing nndss_wide across all states for MMWR year 2026 -- expect these
# to be CLOSE but not necessarily identical (different case classification
# / reporting timing between the two CDC data systems). Document any gap,
# don't assume error.
national_check <- nndss_wide %>%
  filter(mmwr_year == 2026) %>%
  group_by(mmwr_week) %>%
  summarise(national_cum = sum(measles_cum_ytd_total), .groups = "drop") %>%
  arrange(desc(mmwr_week))

print(head(national_check, 5))

write_csv(nndss_wide, "data/clean/nndss_measles_clean_20260704.csv")
