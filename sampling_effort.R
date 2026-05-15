# This code is to determine if there is a way to decrease the sampling effort
# of the Bloody Hill bladderpod sampling SOP.

# Libraries ----
library(HTLNbladderpod)
library(DBI)
library(odbc)
library(tidyverse)

# getting bloodyhill data ----
importData(instance = 'local',
           path = "C:/Users/kbailey/Documents/HTLN Database/",
           name = "MoBlad_BloodyHill_v3.66.accdb",
           new_env = TRUE)

# merging data

bh_counts <- VIEWS_HTLN_BLDP$tbl_5m_MoBladCount
bh_period <- VIEWS_HTLN_BLDP$tlu_PeriodID
bh_core <- VIEWS_HTLN_BLDP$tlu_5m_Grid_Core
density_class <- VIEWS_HTLN_BLDP$tlu_DensityClasses

bh_data <- bh_counts |>
  dplyr::left_join(bh_period) |>
  dplyr::left_join(bh_core) |>
  dplyr::left_join(density_class)

# adding core/non-core column
bh_data <- bh_data |>
  mutate(type = if_else(is.na(Core_ID), "non-core", "core")) |>
  # cleaning up the columns
  select(ParkCode,
         Location_ID,
         Period_ID,
         StartDate,
         EndDate,
         CalYr,
         FirstBloom,
         DensityClass,
         LowerBound,
         UpperBound,
         Midpoint,
         AreaSampled,
         type)


# Can we drop non-core sampling years ----
bh_all <- bh_data |>
  # filter(type == "core") |>
  # filter(AreaSampled == "entire") |>
  mutate(CalYr = as.numeric(CalYr),
         type = factor(type),
         DensityClass = factor(DensityClass,
                               levels = c("0",
                                          "1",
                                          "2",
                                          "3",
                                          "4",
                                          "5",
                                          "6",
                                          "7")))


# Data exploration ----

ggplot() +
  geom_bar(data = bh_all,
               aes(DensityClass))


# data is right skewed

# With how the data are collected and placed into density classes, we can really
# only determine the estimated probability of each class in a given year,
# estimated cumulative probabilities, expected/typical class,
# and trends over time of the distribution classes.


# fitting cumulative link mixed model ----
# A cumulative link mixed model (CLMM) will estimate:
  # Whether plot group (core vs. 5‑year) shifts the probability of being in higher density classes
  # Whether year affects density class
  # Whether trends differ between plot groups
  # How much plot‑to‑plot variation exists

library(ordinal)

mod1 <- clmm(DensityClass ~ CalYr + (1|Location_ID),
             data = bh_all)

mod2 <- clmm(DensityClass ~ CalYr + type + (CalYr|Location_ID),
             data = bh_all)

mod3 <- clmm(DensityClass ~ CalYr * type + (1|Location_ID),
             data = bh_all)

# If you want to look at if years are different (dont forget to set year as
# factor), friedman test. This will tell us if the distribution of classes
# has shifted across years (or treatments)
