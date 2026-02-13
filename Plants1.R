#first file
#
# Japanese Knotweed: Where is it most reported? (GBIF + R)
# Simple EDA script
#

rm(list = ls())

# Packages
library(rgbif)
library(tidyverse)
library(lubridate)
library(scales)

# 1) Get the GBIF taxon key for Japanese knotweed
bb <- name_backbone(name = "Reynoutria japonica")
taxon_key <- bb$usageKey
bb$scientificName

# 2) Pulling a manageable subset of records
res <- occ_search(
  taxonKey = taxon_key,
  year = "2000,2024",
  hasCoordinate = TRUE,
  limit = 3000,
  start = 0
)

occ_raw <- res$data
cat("Pulled rows:", nrow(occ_raw), "\n")

# 3) Keep only what we need + clean
occ <- occ_raw %>%
  transmute(
    gbif_id = key,
    country = country,
    year = as.integer(year),
    event_date = eventDate,
    lat = decimalLatitude,
    lon = decimalLongitude
  ) %>%
  filter(!is.na(country), !is.na(year))

# 4) Validation checks (good for your rubric write-up)
cat("Year range:", min(occ$year), "to", max(occ$year), "\n")
cat("Missing country:", sum(is.na(occ$country)), "\n")
cat("Unique GBIF IDs:", n_distinct(occ$gbif_id), "\n")

# 5) Table: Top 10 countries by occurrence records
top_countries <- occ %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10)

print(top_countries)

# Save table for your Medium post
write_csv(top_countries, "knotweed_top_countries.csv")

# 6) Figure: Bar chart of top 10 countries
p1 <- ggplot(top_countries, aes(x = reorder(country, n), y = n)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Japanese knotweed: Top 10 countries by GBIF records (sample)",
    x = "Country",
    y = "Number of occurrence records"
  ) +
  theme_minimal()

print(p1)
ggsave("fig_top10_countries.png", p1, width = 8, height = 4.5, dpi = 150)

# 7) Figure: Trend over time (global counts per year)
by_year <- occ %>%
  count(year) %>%
  arrange(year)

p2 <- ggplot(by_year, aes(x = year, y = n)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Japanese knotweed: GBIF records per year (sample)",
    x = "Year",
    y = "Records"
  ) +
  theme_minimal()

print(p2)
ggsave("fig_year_trend.png", p2, width = 8, height = 4.5, dpi = 150)


# 8) Optional: Month-of-year seasonality (if eventDate exists)
occ_month <- occ %>%
  mutate(event_date = ymd_hms(event_date, quiet = TRUE)) %>%
  mutate(event_date = if_else(is.na(event_date), ymd(event_date, quiet = TRUE), event_date)) %>%
  filter(!is.na(event_date)) %>%
  mutate(month = month(event_date, label = TRUE, abbr = TRUE)) %>%
  count(month)

if (nrow(occ_month) > 0) {
  p3 <- ggplot(occ_month, aes(x = month, y = n)) +
    geom_col() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Japanese knotweed: GBIF records by month (sample)",
      x = "Month",
      y = "Records"
    ) +
    theme_minimal()

  print(p3)
  ggsave("fig_month_seasonality.png", p3, width = 8, height = 4.5, dpi = 150)
}

