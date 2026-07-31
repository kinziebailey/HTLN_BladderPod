# model building

# libraries ----
library(HTLNbladderpod)
library(DBI)
library(odbc)
library(tidyverse)


# loading data ----
# getting databases from Renviron
PATH_DATA <- Sys.getenv("PATH_DATA")
bh_database <- Sys.getenv("DATA_BASE_BH")
glad_database <- Sys.getenv("DATA_BASE_OTHER")

importData(instance = 'local',
           path = PATH_DATA,
           name = bh_database,
           new_env = TRUE)

bh_counts <- VIEWS_HTLN_BLDP$tbl_5m_MoBladCount
bh_period <- VIEWS_HTLN_BLDP$tlu_PeriodID
bh_core <- VIEWS_HTLN_BLDP$tlu_5m_Grid_Core
bh_accuracy <- VIEWS_HTLN_BLDP$tbl_5m_Accuracy
bh_cedar1 <- VIEWS_HTLN_BLDP$tbl_5m_Cedar
bh_location <- VIEWS_HTLN_BLDP$tlu_GridLocations

## other glades data ----
importData(instance = 'local',
           path = PATH_DATA,
           name = glad_database,
           new_env = TRUE)

glad_counts <- VIEWS_HTLN_BLDP$tbl_MoBladGridCount
glad_cells <- VIEWS_HTLN_BLDP$tlu_CellID
# glad_names <- VIEWS_HTLN_BLDP$tlu_GladeNames
# grid_id <- VIEWS_HTLN_BLDP$tlu_GridID
locations <- VIEWS_HTLN_BLDP$tlu_GridIdLocation
sampling_dates <- VIEWS_HTLN_BLDP$tlu_SamplingEvents
# sampling_period <- VIEWS_HTLN_BLDP$tlu_SamplingPeriods
density_class <- VIEWS_HTLN_BLDP$tlu_DensityClasses

# merging data
glad_data1 <- glad_cells |>
  left_join(locations,
            by = join_by(CellID)) |>
  right_join(glad_counts,
             by = join_by(Grid,
                          Cell)) |>
  left_join(sampling_dates,
            by = join_by(EventID)) |>
  dplyr::mutate(Count = NA,
                DensityClass_Actual = NA) |>
  select(Grid,
         CellID,
         `OBJECTID *`,
         `Shape *`,
         X_Coord,
         Y_Coord,
         Lon_DD,
         Lat_DD,
         GridSizeMeters,
         PeriodID,
         StartDate,
         EndDate,
         DensityClass,
         Count,
         DensityClass_Actual)

# adding bloody hill data
bh_data <-  bh_counts |>
  dplyr::left_join(bh_period,
                   by = "Period_ID") |>
  dplyr::left_join(bh_core,
                   by = join_by(Location_ID)) |>
  dplyr::left_join(bh_location,
                   by = join_by(Location_ID)) |>
  dplyr::mutate(Grid = "Bloody Hill",
                PeriodID = Period_ID,
                CellID = Location_ID) |>
  dplyr::left_join(bh_accuracy,
                   by = join_by(Location_ID, Period_ID)) |>
  dplyr::select(Grid,
                CellID,
                `OBJECTID *`,
                `Shape *`,
                X_Coord,
                Y_Coord,
                Lon_DD,
                Lat_DD,
                GridSizeMeters,
                PeriodID,
                StartDate,
                EndDate,
                DensityClass,
                Count,
                DensityClass_Actual)

# combining data
bp_data1 <- bind_rows(glad_data1,
                      bh_data)

bp_data <- bp_data1 |>
  dplyr::left_join(density_class,
                   by = join_by(DensityClass)) |>
  dplyr::mutate(year = as.numeric(year(StartDate)),
                DensityClass = case_when(DensityClass == -9999 ~ NA_real_,
                                         TRUE ~ DensityClass),
                DensityClassf = factor(DensityClass,
                                       levels = c("0",
                                                  "1",
                                                  "2",
                                                  "3",
                                                  "4",
                                                  "5",
                                                  "6"),
                                       ordered = TRUE),
                DensityClass_Actualf = factor(DensityClass_Actual,
                                              levels = c("0",
                                                         "1",
                                                         "2",
                                                         "3",
                                                         "4",
                                                         "5",
                                                         "6"),
                                              ordered = TRUE))

# Setup ----
## bp_data: all data for all years
## count_data: subset of actual counted plots
## grid: spatial data

count_data <- bp_data |>
  dplyr::filter(!is.na(Count))

grid <- bp_data |>
  select(Grid,
         CellID,
         `OBJECTID *`,
         `Shape *`,
         X_Coord,
         Y_Coord,
         Lon_DD,
         Lat_DD,
         GridSizeMeters) |>
  distinct()

# Defining class boundaries ----

## distribution of true count per density class
ggplot(data = count_data,
       aes(x = Count)) +
  geom_density() +
  facet_wrap(~DensityClassf,
             scales = "free")

# 0:
# 1:
# 2:
# 3:
# 4:
# 5:
# 6:


class_lower <- sort(unique(bp_data$LowerBound))
class_upper <- sort(unique(bp_data$UpperBound))

# Observational Model ----

## True counts
# count ~ negbinomial(DensityClassf, dispersion) ?

## Density Class
# DensityClassf ~ brms::brm(family = cumulative, class boundaries as thresholds) model already created?

# Latent Model ----
#log_N[i,t] = mu + a_site[site] + s_spatial[plot] + r_time[t] + beta * log_N[i,t-1] + epsilon[i,t]
