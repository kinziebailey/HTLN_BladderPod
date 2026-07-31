# This code is to look at the time since disturbance model to understand how
# the removal of cedar impacts bladderpods at all glades.


# libraries ----
library(HTLNbladderpod) # loading bladder pod data
library(DBI) # loading bladder pod data
library(odbc) # loading bladder pod data
library(tidyverse) # data wrangling
# library(car) # regression diagnostics
library(brms) # bayesian modeling
library(bayesplot) # plotting bayesian

# getting data ----
## bloodyhill data ----

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

# reading in disturbance data
treatments <- read.csv("data/MoBladFire.csv")

treatments <- treatments |>
  mutate(Date = as.POSIXct(Date,
                           format = "%m/%d/%Y"),
         year = year(Date))

# merging data
glad_data1 <- glad_cells |>
  left_join(locations,
            by = join_by(CellID)) |>
  right_join(glad_counts,
             by = join_by(Grid,
                          Cell)) |>
  left_join(sampling_dates,
            by = join_by(EventID)) |>
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
         DensityClass)

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
                DensityClass)

# combining data
bp_data1 <- bind_rows(glad_data1,
                      bh_data)

bp_data <- bp_data1 |>
  dplyr::left_join(density_class) |>
  dplyr::mutate(year = as.numeric(year(StartDate)),
                DensityClass = case_when(DensityClass == -9999 ~ NA_real_,
                                         TRUE ~ DensityClass))


# time since treatment
bp_data_treatment <- bp_data |>
  # adding treatments by glade and year
  left_join(treatments,
            by = c("Grid" = "Glade",
                   "year" = "year")) |>
  arrange(Grid,
          StartDate) |>
  group_by(Grid) |>
  fill(Date,
       Type,
       GladeImpacted) |>
  mutate(Date_corrected = if_else(StartDate < Date,
                        NA_Date_,
                        Date)) |>
  fill(Date_corrected,
       Type,
       GladeImpacted) |>
  ungroup() |>
  mutate(time_since_treatment = as.numeric(year(StartDate) - year(Date_corrected)),
         DensityClass_f = factor(DensityClass,
                                 levels = c("0",
                                            "1",
                                            "2",
                                            "3",
                                            "4",
                                            "5",
                                            "6"),
                                 ordered = TRUE),
         treatment = if_else(if_any(c("Date_corrected",
                                      "Type"), is.na),
                             NA_character_,
                             paste0(year(Date_corrected),
                                    Type)))

# plotting data -----

ggplot() +
  geom_bar(data = bp_data_treatment,
           aes(DensityClass)) +
  facet_wrap(~Grid,
             scale = "free")

# Average density class per yst

avg_dens <- bp_data_treatment |>
  summarise(avg_denclass = mean(DensityClass,
                                na.rm = TRUE),
            .by = c(Grid,
                    time_since_treatment,
                    treatment))

ggplot(data = avg_dens,
       aes(time_since_treatment,
           avg_denclass,
           color = treatment)) +
  geom_line() +
  facet_wrap(~Grid)

# average density class per year
avg_dens_year <- bp_data_treatment |>
  summarise(avg_denclass = mean(DensityClass,
                                na.rm = TRUE),
            .by = c(Grid,
                    year))

ggplot(data = avg_dens_year,
       aes(year,
           avg_denclass)) +
  geom_line() +
  facet_wrap(~Grid,
             scales = "free")

# Average density class per yst and treatment type

avg_dens <- bp_data_treatment |>
  summarise(avg_denclass = mean(DensityClass,
                                na.rm = TRUE),
            .by = c(Grid,
                    time_since_treatment,
                    Type))

ggplot(data = avg_dens,
       aes(time_since_treatment,
           avg_denclass,
           color = Grid)) +
  geom_line() +
  scale_x_continuous(limits = c(1, 11)) +
  facet_wrap(~Type)
# The large increase from time 0 to time 1 is because time 0 only occurs when
# the treatment was applied at the incorrect time of year.
# Also, how to account for


# number of plots in density classes per year
# bp_year <- bp_data_treatment |>
#   count(time_since_treatment,
#         DensityClass_f,
#         name = "count")
#
# ggplot(data = bp_year,
#        aes(x = time_since_treatment,
#            y = count,
#            color = DensityClass_f)) +
#   # geom_point() +
#   geom_smooth(se = FALSE)
#
# ggplot() +
#   geom_boxplot(data = bp_year,
#                aes(x = DensityClass_f,
#                    y = count))
#
# ggplot() +
#   geom_line(data = bp_year,
#             aes(x = time_since_treatment,
#                 y = count,
#                 color = DensityClass_f))
#
# # count per density class
# bp_counts <- bp_data_treatment |>
#   count(time_treatment,
#         DensityClass_f,
#         Grid)
#
# ggplot() +
#   geom_line(data = bp_counts,
#             aes(x = time_treatment,
#                 y = n,
#                 color = DensityClass_f)) +
#   facet_wrap(~Grid,
#              scale = "free")


# Creating models ----
## BRMS

timesince_model <- brm(formula = DensityClass_f ~ time_treatment + (1 | GridSizeMeters) + (1 | CellID),
                       data = bp_data_treatment,
                       family = cumulative(link = "logit"),
                       chains = 4,
                       cores = 4,
                       iter = 500)

summary(timesince_model)

plot(timesince_model)

conditional_effects(timesince_model,
                    categorical = TRUE)
