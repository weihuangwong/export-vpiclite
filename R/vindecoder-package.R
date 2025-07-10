#' vindecoder: VIN Decoding Using VPIC Lite Database
#'
#' This package provides functions to decode Vehicle Identification Numbers (VINs) 
#' using a lightweight implementation of the VPIC (Vehicle Product Information 
#' Catalog) database. The package includes functions to initialize an in-memory 
#' database from CSV files and decode VINs to extract vehicle information.
#'
#' @section Main Functions:
#' The two main functions you'll use are:
#' \itemize{
#'   \item \code{\link{init_vpic_db}}: Initialize the VPIC database connection
#'   \item \code{\link{sp_vin_decode}}: Decode a VIN to extract vehicle information
#' }
#'
#' @section Workflow:
#' The typical workflow is:
#' \enumerate{
#'   \item Initialize database: \code{con <- init_vpic_db()}
#'   \item Decode VIN: \code{result <- sp_vin_decode("1HGCM82633A004352", con)}
#'   \item Clean up: \code{DBI::dbDisconnect(con)}
#' }
#'
#' @docType package
#' @name vindecoder
#' @aliases vindecoder-package
NULL
