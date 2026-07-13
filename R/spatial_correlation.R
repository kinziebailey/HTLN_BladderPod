# loading spatial data from shp files

library(sf)
library(archive)
library(dplyr)
library(readr)
library(ggplot2)
library(spdep)
library(RColorBrewer)
library(HTLNbladderpod)

# getting databases from Renviron
## database path
PATH_DATA <- Sys.getenv("PATH_DATA")

## shp path
shape_path <- Sys.getenv("PATH_SPATIAL")

## bloody hill database
bh_database <- Sys.getenv("DATA_BASE_BH")

# list for file names
files <- list.files(shape_path,
                    pattern = "\\.7z$",
                    full.names = TRUE)

# extracting data from zip
for (i in files) {
  archive::archive_extract(i,
                           dir = "unzipped")
}

# loading shape files
shape_files <- list.files("unzipped",
                          pattern = "\\.shp$",
                          full.names = TRUE)

# reading shpae file
shapes <- lapply(shape_files, st_read)
names(shapes) <- tools::file_path_sans_ext(basename(shape_files))

# adding to global environment
list2env(shapes, envir = .GlobalEnv)


# Plotting Bloody Hill spatial data

# grids
library(terra)

terrain <- rast("terrain.tif")

plot(st_geometry(fcl_Grid_BH5mp))


# Loading bp data (this will need to change when I get access to Microsoft stuff)
importData(instance = 'local',
           path = PATH_DATA,
           name = bh_database,
           new_env = TRUE)

# merging data
bh_counts <- VIEWS_HTLN_BLDP$tbl_5m_MoBladCount
bh_period <- VIEWS_HTLN_BLDP$tlu_PeriodID
bh_core <- VIEWS_HTLN_BLDP$tlu_5m_Grid_Core
density_class <- VIEWS_HTLN_BLDP$tlu_DensityClasses
bh_accuracy <- VIEWS_HTLN_BLDP$tbl_5m_Accuracy


bh_data1 <- bh_counts |>
  dplyr::left_join(bh_period,
                   by = "Period_ID") |>
  # dplyr::left_join(bh_core) |>
  dplyr::left_join(bh_accuracy,
                   by = c("Period_ID",
                          "Location_ID")) |>
  dplyr::left_join(density_class,
                   by = "DensityClass")

# Data Wrangling
bh_data <- bh_data1 |>
  # converting classes
  mutate(Date = as.Date(StartDate, format = "%m/%d/%Y"),
         DensityClass_f = as.factor(DensityClass#,
                                  # levels = C("0",
                                  #            "1",
                                  #            "2",
                                  #            "3",
                                  #            "4",
                                  #            "5",
                                  #            "6")
                                  )) |>
  select(Location_ID,
         DensityClass,
         DensityClass_f,
         ParkCode,
         Date,
         CalYr,
         AreaSampled,
         PeriodDescriptor,
         AnnualBrome,
         HopCover,
         Sericea,
         EasternRedCedar,
         Thistle,
         BushHoneysuckle,
         Count,
         DensityClass_Est,
         DensityClass_Actual,
         LowerBound,
         UpperBound,
         Midpoint,
         LowerBound50,
         UpperBound50)

# Merging spatial and count data
bh_data_shp <- fcl_Grid_BH5mp |>
  dplyr::left_join(bh_data,
                   by = c("Location_I" = "Location_ID"))

# plotting data ----
ggplot() +
  geom_sf(data = bh_data_shp,
          aes(fill = DensityClass)) +
  facet_wrap(~CalYr)

# Determining spatial autocorrelation

# Global Moran's I ----
# Is there any spatial auto correlation at all?
# splitting up years
years <- split(bh_data_shp,
               bh_data_shp$CalYr)

# spatial weights
near_list <- lapply(years, function(y) poly2nb(y, queen = TRUE)) # queen true given plant data

# converting to weights
weights_list <- lapply(near_list, nb2listw)

# calculating morans
mor_results <- mapply(function(y, weights_list) {
  moran.test(y$DensityClass, weights_list)
}, years, weights_list)

# yes. Each year has spatial auto correlation

# Local Moran's I ----
# Where exactly are the clusters or hotspots?
neighbours <- poly2nb(bh_data_shp, queen = TRUE)
spatial_weights <- nb2listw(neighbours)

local_autocor <- localmoran(bh_data_shp$DensityClass,
                            spatial_weights)

# adding back to dataset
bh_data_shp$Ii <- local_autocor[, "Ii"]
bh_data_shp$E_Ii <- local_autocor[, "E.Ii"]
bh_data_shp$var_Ii <- local_autocor[, "Var.Ii"]
bh_data_shp$Z_Ii <- local_autocor[, "Z.Ii"]
bh_data_shp$p_Ii <- local_autocor[, "Pr(z != E(Ii))"]

bh_data_shp <- bh_data_shp |>
  mutate(clusters = case_when(Z_Ii > 0 &
                                DensityClass > mean(DensityClass) &
                                p_Ii < 0.05 ~ "High-High",
                              Z_Ii > 0 &
                                DensityClass < mean(DensityClass) &
                                p_Ii < 0.05 ~ "Low-Low",
                              Z_Ii < 0 &
                                DensityClass > mean(DensityClass) &
                                p_Ii < 0.05 ~ "High-Low",
                              Z_Ii < 0 &
                                DensityClass < mean(DensityClass) &
                                p_Ii < 0.05 ~ "Low-High",
                              TRUE ~ "Not Significant"))

# High-High: presence of spatial clustering of neighors with high values
#   surrounded by those with similar values
# Low-Low: spatial clustering of neighbors with low values surrounded by those
#   with similar values
# High/Low: spatial outlies or neighbors with calues that are statistically insig

# Plotting cluster map

# subsetting core plots
core_outline <- bh_data_shp |>
  filter(AreaSampled== "core") |>
  sf::st_union()


ggplot() +
  geom_sf(data = bh_data_shp,
          aes(fill = clusters)) +
  geom_sf(data = core_outline,
          fill = NA,
          color = "black",
          size = 2) +
  scale_fill_manual(values = c("High-High" = "red",
                               "Low-Low" = "blue",
                               "High-Low" = "mistyrose",
                               "Low-High" = "lightblue",
                               "Not Significant" = "white")) +
  theme_bw()

