#' @title importData: Import views directly from HTLN bladderpod database
#'
#' @description This function imports all views in the HTLN MoBlad_BloodyHill
#' Microsoft Access DB. Each view is added to a VIEWS_HTLN_BLDP environment
#' in your workspace, or to your global environment based on whether
#' new_env = TRUE or FALSE. You must have the latest ODBC SQL driver installed for this function to
#' work. It can be downloaded from: https://go.microsoft.com/fwlink/?linkid=2168524
#'
#' @importFrom dplyr collect rename tbl
#' @importFrom magrittr %>%
#'
#' @param instance Specify whether you are connecting to the local instance or server.
#' \describe{
#' \item{"local"}{Default. Connects to local install of backend database}
#' \item{"server"}{Connects to main backend on server. Note that you must have permission to access the server, and
#' connection speeds are likely to be much slower than the local instance. You must also be connected to VPN or NPS network.}}
#'
#' @param server Quoted name of the server to connect to, if instance = "server". Valid input is the server
#' address to connect to the main database. If connecting to the local instance, leave blank.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_MIDN_NCBN environment. If \code{FALSE}, stores views in global environment
#'
#' @param name Character. Specifies the name of the database. Default is "MIDN_Forest"
#'
#' @examples
#' \dontrun{
#' # Import using default settings of local instance, server = 'localhost' and add VIEWS_MIDN_NCBN environment
#' importData()
#'
#' # Import using computer name (# should be real numbers)
#' importData(server = "INPNETN-######", new_env = TRUE)
#'
#' # Import from main database on server
#' importData(server = "INP###########\\########", instance = "server", new_env = TRUE)
#' }
#'
#' @return MIDN database views in specified environment
#'
#' @export
