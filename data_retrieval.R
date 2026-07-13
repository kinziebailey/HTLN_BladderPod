# Getting data from the Bladderpod databases

# Determine if packages need to be installed
packages <-c(
  "sf",
  "archive",
  "DBI",
  "dplyr",
  "odbc"
)

# Install packages if needed
install_fun <- function(p) {
  need_install <- p[!p %in% installed.packages()[, "Package"]]
  if(length(need_install)) install.packages(need_install,
                                            dependencies = TRUE)
}

install_fun(packages)

# Libraries
library(DBI) # accessing databases
library(dplyr) # data manipulation
library(odbc) # used to connect with drive
library(sf) # spatial data
library(archive) # unzip files



library(readr)
library(magrittr)

library(dbplyr)


# Connect to Access ----
# The following code allows you to directly see the data tables in access

bh_db_path <- "C:/Users/kbailey/Documents/HTLN Database/MoBlad_BloodyHill_v3.66.accdb"

conn_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", bh_db_path)

conn <- dbConnect(odbc(),
                  .connection_string = conn_string)

view_list_db <- DBI::dbListTables(conn)

# removing tables you need access to
view_list_db <- view_list_db[!grepl("^MSys", view_list_db)]

is_valid <- vapply(view_list_db, function(t) {
  tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT * FROM [", t, "] WHERE 0=1"))
    TRUE
  }, error = function(e) FALSE)
}, logical(1))

valid_tables <- view_list_db[is_valid]


view_import <- lapply(seq_along(valid_tables), function(x){
  # get all tables from the database
  view <- valid_tables[[x]]

  tab <- tbl(conn,
             view) |>
    collect() |>
    as.data.frame()

  return(tab)
})

DBI::dbDisconnect(conn)

# rename
view_import <- setNames(view_import,
                        valid_tables)


# Getting data
tbl_5m_Accuracy <- tbl(conn, "tbl_5m_Accuracy") |>
  collect()

tbl_5m_Cedar <- tbl(conn, "tbl_5m_Cedar") |>
  collect()

tbl_5m_MoBladCount <- tbl(conn, "tbl_5m_MoBladCount") |>
  collect()

tbl_5m_PAR <- tbl(conn, "tbl_5m_PAR") |>
  collect()

tlu_5m_Grid <- tbl(conn, "tlu_5m_Grid") |>
  collect()

tlu_5m_Grid_Core <- tbl(conn, "tlu_5m_Grid_Core") |>
  collect()

tlu_DensityClasses <- tbl(conn, "tlu_DensityClasses") |>
  collect()

tlu_GridCentroids_Display <- tbl(conn, "tlu_GridCentroids_Display") |>
  collect()

tlu_GridLocations <- tbl(conn, "tlu_GridLocations") |>
  collect()

tlu_ParkName <- tbl(conn, "tlu_ParkName") |>
  collect()

tlu_PeriodID <- tbl(conn, "tlu_PeriodID") |>
  collect()

# disconnect from access
dbDisconnect(conn)


# ImportExport package
access_import()








# Other database

glades_db_path <- "C:/Users/kbailey/Documents/HTLN Database/MoBlad_Glades_v1.8.8.accdb"

conn_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", glades_db_path)

conn <- dbConnect(odbc(),
                  .connection_string = conn_string)

view_list_db <- DBI::dbListTables(conn)

# removing tables you need access to
view_list_db <- view_list_db[grepl("tbl|tlu", view_list_db)]

is_valid <- vapply(view_list_db, function(t) {
  tryCatch({
    DBI::dbGetQuery(conn, paste0("SELECT * FROM [", t, "] WHERE 0=1"))
    TRUE
  }, error = function(e) FALSE)
}, logical(1))

valid_tables <- view_list_db[is_valid]


view_import <- lapply(seq_along(valid_tables), function(x){
  # get all tables from the database
  view <- valid_tables[[x]]

  tab <- tbl(conn,
             view) |>
    collect() |>
    as.data.frame()

  return(tab)
})



# Getting data
tbl_MoBladGridCount <- tbl(conn, "tbl_MoBladGridCount") |>
  collect()

tbl_ProtocolNarrativeReference <- tbl(conn, "tbl_ProtocolNarrativeReference") |>
  collect()

tbl_ProtSOPQAPCollection <- tbl(conn, "tbl_ProtSOPQAPCollection") |>
  collect()

tbl_QAPReference <- tbl(conn, "tbl_QAPReference") |>
  collect()

tbl_SOPReference <- tbl(conn, "tbl_SOPReference") |>
  collect()

tlu_CellID <- tbl(conn, "tlu_CellID") |>
  collect()

tlu_DensityClasses <- tbl(conn, "tlu_DensityClasses") |>
  collect()

tlu_GladeNames <- tbl(conn, "tlu_GladeNames") |>
  collect()

tlu_GridIdLocation <- tbl(conn, "tlu_GridIdLocation") |>
  collect()

tlu_GridID <- tbl(conn, "tlu_GridID") |>
  collect()

tlu_SamplingEvents <- tbl(conn, "tlu_SamplingEvents") |>
  collect()

tlu_SamplingPeriods <- tbl(conn, "tlu_SamplingPeriods") |>
  collect()

tlu_WR_All_EventIDs_by_Cells <- tbl(conn, "tlu_WR_All_EventIDs_by_Cells") |>
  collect()

xtab_SOPCollection <- tbl(conn, "xtab_SOPCollection") |>
  collect()

qry_CellDensityByEvent <- tbl(conn, "qry_CellDensityByEvent") |>
  collect()


qry_DataPackageOutput <- tbl(conn, "qry_DataPackageOutput") |>
  collect()


qry_PopulationSize_HiLo <- tbl(conn, "qry_PopulationSize_HiLo") |>
  collect()

Query1 <- tbl(conn, "Query1") |>
  collect()

Query3 <- tbl(conn, "Query3") |>
  collect()


DBI::dbGetQuery(conn)


DBI::dbDisconnect(conn)

# rename
view_import <- setNames(view_import,
                        valid_tables)
