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
importData(instance = 'local',
           path = "C:/Users/kbailey/Documents/HTLN/HTLN Database/",
           name = "MoBlad_BloodyHill_v3.66.accdb",  # "MoBlad_Glades_v1.8.accdb"
           new_env = TRUE)

bh_counts <- VIEWS_HTLN_BLDP$tbl_5m_MoBladCount
bh_period <- VIEWS_HTLN_BLDP$tlu_PeriodID
bh_core <- VIEWS_HTLN_BLDP$tlu_5m_Grid_Core
# density_class <- VIEWS_HTLN_BLDP$tlu_DensityClasses
bh_accuracy <- VIEWS_HTLN_BLDP$tbl_5m_Accuracy
bh_cedar1 <- VIEWS_HTLN_BLDP$tbl_5m_Cedar
bh_location <- VIEWS_HTLN_BLDP$tlu_GridLocations

## other glades data ----
importData(instance = 'local',
           path = "C:/Users/kbailey/Documents/HTLN/HTLN Database/",
           name = "MoBlad_Glades_v1.8.8.accdb",
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
  dplyr::mutate(year = year(StartDate))




# Treatment data frame
treatment <- data.frame(glade = c("Bloody Hill", "Bloody Hill", "Bloody Hill", "Bloody Hill",
                                  "Bloody Hill Hillside", "Bloody Hill Hillside",
                                  "Bloody Hill Road", "Bloody Hill Road",
                                  "Manley", "Manley", "Manley",
                                  "North Bloody Hill", "North Bloody Hill", "North Bloody Hill",
                                  "North Bloody Hill South", "North Bloody Hill South",
                                  "Northwest Bloody Hill", "Northwest Bloody Hill", "Northwest Bloody Hill",
                                  "Walnut", "Walnut", "Walnut" ,"Walnut", "Walnut",
                                  "Wire Road", "Wire Road", "Wire Road", "Wire Road" ,"Wire Road"),
                        treatment_date = c(2010, 2018, 2019, 2021,
                                           2018, 2021,
                                           2014, 2021,
                                           2018, 2020, 2021,
                                           2002, 2020, 2021,
                                           2014, 2021,
                                           2008, 2010, 2021,
                                           1999, 2002, 2006, 2019, 2021,
                                           2005, 2009, 2011, 2020, 2021),
                        type = c("fire", "cedar", "cedar", "fire",
                                 "cedar", "fire",
                                 "fire", "fire",
                                 "cedar", "cedar", "fire",
                                 "fire", "cedar", "fire",
                                 "fire", "fire",
                                 "fire", "fire", "fire",
                                 "fire", "fire", "cedar", "cedar", "fire",
                                 "fire", "cedar", "fire", "cedar", "fire"))

# time since treatment

bp_data_treatment <- bp_data |>
  full_join(treatment,
            by = c("Grid" = "glade"),
            relationship = "many-to-many")

# probably not the most elegent way to do this......

bp_data_treatment <- bp_data |>
  mutate(time_treatment = case_when(Grid == "Bloody Hill" &
                                      year >= 2010 & year < 2018 ~ year - 2010,
                                    Grid == "Bloody Hill" &
                                      year >= 2018 & year < 2019 ~ year - 2018,
                                    Grid == "Bloody Hill" &
                                      year >= 2019 & year < 2021 ~ year - 2019,
                                    Grid == "Bloody Hill" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "Bloody Hill Hillside" &
                                      year >= 2018 & year < 2022 ~ year - 2018,
                                    Grid == "Bloody Hill Hillside" &
                                      year >= 2022 ~ year - 2022,
                                    Grid == "Bloody Hill Road" &
                                      year >= 2014 & year < 2021 ~ year - 2014,
                                    Grid == "Bloody Hill Road" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "Manley" &
                                      year >= 2018 & year < 2020 ~ year - 2018,
                                    Grid == "Manley" &
                                      year >= 2020 & year < 2021 ~ year - 2020,
                                    Grid == "Manley" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "North Bloody Hill" &
                                      year >= 2002 & year < 2020 ~ year - 2002,
                                    Grid == "North Bloody Hill" &
                                      year >= 2020 & year < 2021 ~ year - 2020,
                                    Grid == "North Bloody Hill" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "North Bloody Hill South" &
                                      year >= 2014 & year < 2021 ~ year - 2014,
                                    Grid == "North Bloody Hill South" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "Northwest Bloody Hill" &
                                      year >= 2008 & year < 2010 ~ year - 2008,
                                    Grid == "Northwest Bloody Hill" &
                                      year >= 2010 & year < 2021 ~ year - 2010,
                                    Grid == "Northwest Bloody Hill" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "Walnut" &
                                      year >= 1999 & year < 2002 ~ year - 1999,
                                    Grid == "Walnut" &
                                      year >= 2002 & year < 2006 ~ year - 2002,
                                    Grid == "Walnut" &
                                      year >= 2006 & year < 2019 ~ year - 2006,
                                    Grid == "Walnut" &
                                      year >= 2019 & year < 2021 ~ year - 2019,
                                    Grid == "Walnut" &
                                      year >= 2021 ~ year - 2021,
                                    Grid == "Wire Road" &
                                      year >= 2006 & year < 2009 ~ year - 2006,
                                    Grid == "Wire Road" &
                                      year >= 2009 & year < 2011 ~ year - 2009,
                                    Grid == "Wire Road" &
                                      year >= 2011 & year < 2020 ~ year - 2011,
                                    Grid == "Wire Road" &
                                      year >= 2020 & year < 2021 ~ year - 2020,
                                    Grid == "Wire Road" &
                                      year >= 2021 ~ year - 2021,
                                    TRUE ~ NA),
         DensityClass = case_when(DensityClass == -9999 ~ NA,
                                  TRUE ~ DensityClass),
         DensityClass_f = factor(DensityClass,
                                 levels = c("0",
                                            "1",
                                            "2",
                                            "3",
                                            "4",
                                            "5",
                                            "6",
                                            "7"),
                                 ordered = TRUE)) |>
  arrange(Grid,
          CellID,
          StartDate)

# plottin data -----

ggplot() +
  geom_bar(data = bp_data_treatment,
           aes(DensityClass)) +
  facet_wrap(~Grid,
             scale = "free")

# number of plots in density classes per year
bp_year <- bp_data_treatment |>
  count(time_treatment,
        DensityClass_f,
        name = "count")

ggplot() +
  geom_boxplot(data = bp_year,
               aes(x = DensityClass_f,
                   y = count,
                   fill = DensityClass_f))

ggplot() +
  geom_line(data = bp_year,
            aes(x = time_treatment,
                y = count,
                color = DensityClass_f))

# count per density class
bp_counts <- bp_data_treatment |>
  count(time_treatment,
        DensityClass_f,
        Grid)

ggplot() +
  geom_line(data = bp_counts,
            aes(x = time_treatment,
                y = n,
                color = DensityClass_f)) +
  facet_wrap(~Grid,
             scale = "free")


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
