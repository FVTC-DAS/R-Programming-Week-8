# Lab 8 – Tidy Data & Factors
# Student Name: __________________________
#
#
# INSTRUCTIONS
# - Do ALL your work in this single R script (lab8.R).
# - Replace each TODO with working code and short answers in comments where requested.
# - Some of the code has been filled in for you.  Fill in the blanks (_____) as needed.
# - Run often; commit early and push when each exercise is complete.
# - Do not delete the section headers.
#
# PACKAGES YOU'LL USE

## Check for needed packages and install the ones that are missing
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(forcats)) install.packages("forcats")
if(!require(scales)) install.packages("scales")
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")

library(tidyverse)  # loads dplyr, tidyr, ggplot2, readr, etc.
library(forcats)
library(scales)
library(dplyr)
library(readr)

# -------------------------------------------------------------------
# EXERCISE 1 – Democracy Scores (tidy data)
# -------------------------------------------------------------------
# Task: Read the dataset from https://moderndive.com/data/dem_score.csv
# into a data frame named dem_score. Convert from wide to long ("tidy")
# into dem_score_tidy with columns: country, year (integer), democracy_score.
# Finally, show the first 5 rows.
# Hints: readr::read_csv(), tidyr::pivot_longer(), mutate(year = as.integer(...))

# TODO 1.1: READ -> dem_score
dem_score <- _____("https://moderndive.com/data/dem_score.csv")

# TODO 1.2: TIDY -> dem_score_tidy
dem_score_tidy <- dem_score |> 
_____(cols = -country, names_to = "year", values_to = "democracy_score") |> 
_____(year = as.integer(year))

# TODO 1.3: PREVIEW first 5 rows
head(_____, 5)

# -------------------------------------------------------------------
# EXERCISE 2 – Life Expectancy (tidy data)
# -------------------------------------------------------------------
# Task: Read life expectancy from https://moderndive.com/data/le_mess.csv
# Tidy into le_tidy with columns: country, year (integer), life_expectancy.
# Show first 5 rows.

# TODO 2.1: READ -> le_mess
le_mess <- _____("https://moderndive.com/data/le_mess.csv")

# TODO 2.2: TIDY -> le_tidy
le_tidy <- le_mess |> 
_____(cols = -country, names_to = "year", values_to = "life_expectancy") |> 
_____(year = as.integer(year))

# TODO 2.3: PREVIEW
head(_____, 5)

# -------------------------------------------------------------------
# EXERCISE 3 – Join the two tidy datasets
# -------------------------------------------------------------------
# Task: Combine dem_score_tidy and le_tidy into a single data frame with
# columns country, year, life_expectancy, democracy_score.
# Hint: dplyr::inner_join() (or left_join) by c("country", "year"), then select columns.

# TODO 3.1: JOIN -> df_combined
df_combined <- _____ |> 
_____(_____, by = c("country", "year")) |> 
_____(country, year, life_expectancy, democracy_score)

# TODO 3.2: PREVIEW first 10 rows
head(_____, 10)

# -------------------------------------------------------------------
# EXERCISE 4 – Star Wars eye colors (factors & lumping)
# -------------------------------------------------------------------
# Data: dplyr::starwars
# Task A: Count and plot eye colors (bar chart). Handle missing values explicitly.
# Task B: Recreate the plot using only the TOP 5 eye colors; group the rest as
#         "Other" using forcats::fct_lump_n(..., n = 5, other_level = "Other").
# Hints: filter(!is.na(eye_color)), count(eye_color, sort = TRUE), ggplot2::geom_col(), coord_flip()

# TODO 4.1: COUNT eye colors -> starwars_eye_counts
starwars_eye_counts <- starwars |> 
 _____(!is.na(eye_color)) |> 
 _____(eye_color, sort = TRUE)

# TODO 4.2: PLOT all eye colors (bar chart)
starwars_eye_counts |> 
ggplot(aes(x = _____(eye_color, n), y = n)) +
geom_col() +
coord_flip() +
labs(title = "Star Wars Characters by Eye Color", x = "Eye Color", y = "Count")

# TODO 4.3: LUMP to TOP 5 + "Other" and plot
starwars_top5 <- starwars |>
_____(!is.na(eye_color)) |>
_____(eye_color_top = fct_lump_n(eye_color, n = 5, other_level = "Other")) |>
_____(eye_color_top, sort = TRUE)

starwars_top5 |>
  ggplot(aes(x = _____(eye_color_top, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Star Wars Eye Colors (Top 5 + Other)", x = "Eye Color", y = "Count")

# -------------------------------------------------------------------
# EXERCISE 5 – Hotels ADR by month (ordering factors)
# -------------------------------------------------------------------
# Data URL:
# https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv
# Tasks:
# (1) Compute mean ADR by month and hotel.
# (2) Make a grouped bar chart by month (x) and mean ADR (y), filled by hotel.
# (3) Observe the default month order and write a brief comment in code.
# (4) Reorder months to calendar order and format y-axis as dollars.
# Hints: group_by(...), summarise(mean_adr = mean(adr, na.rm = TRUE)),
#        scales::dollar, forcats::parse_factor with levels = month.name or month.abb

# TODO 5.1: READ -> hotels
hotels <- _____("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

# TODO 5.2: SUMMARIZE mean ADR by month and hotel -> adr_by_month
adr_by_month <- hotels |>
  _____(hotel, arrival_date_month) |>
  _____(mean_adr = mean(adr, na.rm = TRUE), .groups = "drop")

# TODO 5.3: PLOT unordered months
adr_by_month |>
  ggplot(aes(x = arrival_date_month, y = mean_adr, fill = hotel)) +
  geom_col(position = "dodge") +
  labs(title = "Average Daily Rate by Month & Hotel (Unordered)", x = "Month", y = "Average Daily Rate")

# TODO 5.3a: SHORT REFLECTION (write in comments)
# Q: How are the months ordered in the initial plot? What would be a better order?
# A: ________________________________________________________________

# TODO 5.4: REORDER months to calendar order & format y as dollars
adr_by_month |>
  _____(arrival_date_month = parse_factor(arrival_date_month, levels = month.name, ordered = TRUE)) |>
  ggplot(aes(x = arrival_date_month, y = mean_adr, fill = hotel)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Average Daily Rate by Month & Hotel", x = "Month", y = "Average Daily Rate ($)") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
