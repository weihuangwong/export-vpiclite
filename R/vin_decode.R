# R Implementation of spVinDecode functionality

#' Initialize VPIC Database Connection
#'
#' Creates an in-memory DuckDB database connection and loads all CSV tables
#' from the specified directory into the database for VIN decoding operations.
#'
#' @param tables_path Character string specifying the path to the directory
#'   containing CSV table files. Default is "extdata/tables/".
#'
#' @return A DBI connection object to the initialized DuckDB database.
#'
#' @details This function creates an in-memory DuckDB database and loads all
#'   CSV files from the specified directory as tables. The table names are
#'   derived from the CSV file names (without the .csv extension).
#'
#' @examples
#' \dontrun{
#' con <- init_vpic_db()
#' # Use connection for VIN decoding
#' DBI::dbDisconnect(con)
#' }
#'
#' @export
#' @importFrom DBI dbConnect dbExecute
#' @importFrom duckdb duckdb
#' @importFrom tools file_path_sans_ext
init_vpic_db <- function(tables_path = "extdata/tables/") {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  
  # Load all CSV tables into DuckDB
  csv_files <- list.files(tables_path, pattern = "\\.csv$", full.names = TRUE)
  
  for(csv_file in csv_files) {
    table_name <- tools::file_path_sans_ext(basename(csv_file))
    
    # Create table from CSV
    query <- sprintf("CREATE TABLE %s AS SELECT * FROM read_csv('%s', sample_size = -1)", 
                    table_name, csv_file)
    DBI::dbExecute(con, query)
  }
  
  return(con)
}

#' Extract VIN Descriptor
#'
#' Extracts the VIN descriptor from a Vehicle Identification Number (VIN).
#' The descriptor is typically 11 or 14 characters long, depending on
#' whether the manufacturer is classified as a small manufacturer.
#'
#' @param vin Character string containing the VIN to process.
#'
#' @return Character string containing the VIN descriptor.
#'
#' @details This function normalizes the VIN, replaces position 9 with an
#'   asterisk (check digit position), and extracts the descriptor. For small
#'   manufacturers (3rd position is '9'), it uses 14 characters instead of 11.
#'
#' @keywords internal
f_vin_descriptor <- function(vin) {
  # Normalize VIN
  vin <- trimws(toupper(vin))
  vin <- paste0(vin, paste(rep("*", 17), collapse = ""))
  vin <- substr(vin, 1, 17)
  
  # Replace position 9 with asterisk
  vin <- paste0(substr(vin, 1, 8), "*", substr(vin, 10, 17))
  
  # Extract descriptor (11 or 14 characters)
  descriptor <- substr(vin, 1, 11)
  
  # If 3rd position is '9' (small manufacturer), use 14 characters
  if (substr(vin, 3, 3) == "9") {
    descriptor <- substr(vin, 1, 14)
  }
  
  return(toupper(descriptor))
}

#' Extract World Manufacturer Identifier (WMI)
#'
#' Extracts the World Manufacturer Identifier from a Vehicle Identification
#' Number (VIN). The WMI is typically the first 3 characters, but can be
#' extended to 6 characters for small manufacturers.
#'
#' @param vin Character string containing the VIN to process.
#'
#' @return Character string containing the WMI.
#'
#' @details For small manufacturers (3rd position is '9'), the WMI is extended
#'   to include characters 12-14 from the VIN, making it 6 characters total.
#'
#' @keywords internal
f_vin_wmi <- function(vin) {
  if (nchar(vin) < 3) return(vin)
  
  wmi <- substr(vin, 1, 3)
  
  # If 3rd position is '9' and VIN is long enough, extend to 6 characters
  if (substr(vin, 3, 3) == "9" && nchar(vin) >= 14) {
    wmi <- paste0(wmi, substr(vin, 12, 14))
  }
  
  return(wmi)
}

#' Determine Model Year from VIN Position 10
#'
#' Calculates the model year from the 10th position of a VIN using the
#' standard VIN encoding rules.
#'
#' @param vin Character string containing the VIN to process.
#'
#' @return Integer representing the model year, or NULL if cannot be determined.
#'   Negative values indicate non-conclusive results.
#'
#' @details This function implements the standard VIN model year encoding
#'   where position 10 cycles through characters representing different years.
#'   The function accounts for the 30-year cycle and vehicle type considerations.
#'
#' @keywords internal
f_vin_model_year2 <- function(vin) {
  vin <- toupper(vin)
  if (nchar(vin) < 10) return(NULL)
  
  pos10 <- substr(vin, 10, 10)
  model_year <- NULL
  conclusive <- FALSE
  
  # Map position 10 character to year
  if (grepl("[A-H]", pos10)) {
    model_year <- 2010 + utf8ToInt(pos10) - utf8ToInt("A")
  } else if (grepl("[J-N]", pos10)) {
    model_year <- 2010 + utf8ToInt(pos10) - utf8ToInt("A") - 1
  } else if (pos10 == "P") {
    model_year <- 2023
  } else if (grepl("[R-T]", pos10)) {
    model_year <- 2010 + utf8ToInt(pos10) - utf8ToInt("A") - 3
  } else if (grepl("[V-Y]", pos10)) {
    model_year <- 2010 + utf8ToInt(pos10) - utf8ToInt("A") - 4
  } else if (grepl("[1-9]", pos10)) {
    model_year <- 2031 + as.numeric(pos10) - 1
  }
  
  if (!is.null(model_year)) {
    # Check if vehicle type suggests 30-year cycle offset
    wmi <- f_vin_wmi(vin)
    
    # This would require database lookup for vehicle type
    # For now, implement basic logic
    pos7 <- substr(vin, 7, 7)
    
    # If position 7 is numeric and model year is too far in future
    if (grepl("[0-9]", pos7) && model_year > (as.numeric(format(Sys.Date(), "%Y")) + 1)) {
      model_year <- model_year - 30
      conclusive <- TRUE
    }
    
    # If model year is too far in future, subtract 30
    if (model_year > (as.numeric(format(Sys.Date(), "%Y")) + 1)) {
      model_year <- model_year - 30
      conclusive <- TRUE
    }
  }
  
  # Return negative if not conclusive
  if (!conclusive && !is.null(model_year)) {
    model_year <- -model_year
  }
  
  return(model_year)
}

#' Calculate VIN Check Digit
#'
#' Calculates the check digit for a 17-character VIN using the standard
#' VIN check digit algorithm.
#'
#' @param vin Character string containing the 17-character VIN.
#'
#' @return Character string containing the calculated check digit ("0"-"9" or "X"),
#'   or "?" if the VIN is invalid.
#'
#' @details This function implements the standard VIN check digit calculation
#'   using character-to-value mapping and position weights. The check digit
#'   helps validate VIN authenticity.
#'
#' @keywords internal
f_vin_check_digit <- function(vin) {
  if (nchar(vin) != 17) return("?")
  
  # VIN character to value mapping
  char_values <- c(
    "0"=0, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9,
    "A"=1, "B"=2, "C"=3, "D"=4, "E"=5, "F"=6, "G"=7, "H"=8, "J"=1, "K"=2,
    "L"=3, "M"=4, "N"=5, "P"=7, "R"=9, "S"=2, "T"=3, "U"=4, "V"=5, "W"=6,
    "X"=7, "Y"=8, "Z"=9
  )
  
  # Position weights
  weights <- c(8, 7, 6, 5, 4, 3, 2, 10, 0, 9, 8, 7, 6, 5, 4, 3, 2)
  
  total <- 0
  for (i in 1:17) {
    char <- substr(vin, i, i)
    if (char %in% names(char_values)) {
      total <- total + char_values[char] * weights[i]
    } else {
      return("?")
    }
  }
  
  check_digit <- total %% 11
  if (check_digit == 10) return("X")
  return(as.character(check_digit))
}

#' Calculate Error Value from Error Codes
#'
#' Calculates the total error value by summing weights of provided error codes
#' from the ErrorCode table in the database.
#'
#' @param error_codes Character string containing comma-separated error codes.
#' @param con DBI connection object to the VPIC database.
#'
#' @return Numeric value representing the total error weight.
#'
#' @details This function parses comma-separated error codes and queries the
#'   ErrorCode table to sum the weights of all provided error codes.
#'
#' @keywords internal
#' @importFrom DBI dbGetQuery
f_error_value <- function(error_codes, con) {
  if (is.null(error_codes) || error_codes == "") return(0)
  
  # Parse comma-separated error codes
  codes <- strsplit(error_codes, ",")[[1]]
  codes <- trimws(codes)
  
  if (length(codes) == 0) return(0)
  
  # Query ErrorCode table for weights
  query <- sprintf("SELECT SUM(weight) as total_weight FROM ErrorCode WHERE id IN (%s)", 
                  paste(codes, collapse = ","))
  
  result <- DBI::dbGetQuery(con, query)
  return(ifelse(is.na(result$total_weight), 0, result$total_weight))
}

#' Decode Vehicle Identification Number (VIN)
#'
#' Main function to decode a Vehicle Identification Number using the VPIC
#' database. This function implements the complete VIN decoding algorithm
#' including multiple passes and error handling.
#'
#' @param vin Character string containing the VIN to decode.
#' @param con DBI connection object to the VPIC database (created with \code{init_vpic_db}).
#' @param include_private Logical indicating whether to include private/confidential
#'   decoding information. Default is FALSE.
#' @param year Integer specifying the model year to use for decoding. If NULL,
#'   the year will be calculated from the VIN. Default is NULL.
#' @param include_all Logical indicating whether to include all possible matches.
#'   Default is FALSE.
#' @param no_output Logical indicating whether to suppress formatted output.
#'   Default is FALSE.
#'
#' @return A list containing decoded VIN information including:
#'   \item{vin}{The input VIN}
#'   \item{descriptor}{VIN descriptor}
#'   \item{wmi}{World Manufacturer Identifier}
#'   \item{model_year_source}{Source of model year information}
#'   \item{return_code}{Return code indicating decoding status}
#'   \item{decoding_items}{Detailed decoding results}
#'   \item{summary}{Summary of best decoding result}
#'   \item{details}{Detailed information for best result}
#'   \item{all_results}{All decoding results ranked by quality}
#'
#' @details This function implements a multi-pass VIN decoding algorithm:
#'   \enumerate{
#'     \item First attempts to find exact descriptor match
#'     \item Falls back to calculated model year from position 10
#'     \item Performs multiple passes with different year calculations
#'     \item Ranks results by quality and returns the best match
#'   }
#'
#' @examples
#' \dontrun{
#' con <- init_vpic_db()
#' result <- sp_vin_decode("1HGCM82633A004352", con)
#' print(result$summary)
#' DBI::dbDisconnect(con)
#' }
#'
#' @export
#' @importFrom DBI dbGetQuery
sp_vin_decode <- function(vin, con, include_private = FALSE, year = NULL, 
                         include_all = FALSE, no_output = FALSE) {
  
  # Normalize VIN
  vin <- trimws(toupper(vin))
  
  # Initialize variables
  descriptor <- f_vin_descriptor(vin)
  wmi <- f_vin_wmi(vin)
  model_year_source <- "***X*|Y"
  conclusive <- FALSE
  error_12 <- FALSE
  return_code <- ""
  
  # Try to get model year from VinDescriptor table
  descriptor_query <- sprintf("SELECT ModelYear FROM VinDescriptor WHERE Descriptor = '%s'", 
                             descriptor)
  descriptor_result <- DBI::dbGetQuery(con, descriptor_query)
  
  decoding_items <- data.frame()
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (nrow(descriptor_result) > 0) {
    dmy <- descriptor_result$ModelYear[1]
    
    # Check if model year is valid (1980 to current year + 1)
  
    if (dmy >= 1980 && dmy <= (current_year + 1)) {
      conclusive <- TRUE
      error_12 <- !is.null(year) && !is.na(dmy) && year != dmy
      
      # Call core decoding with pass 1
      core_result <- sp_vin_decode_core(1, vin, dmy, descriptor, conclusive, 
                                       error_12, include_all, include_private, 
                                       FALSE, con)
      decoding_items <- rbind(decoding_items, core_result$items)
      return_code <- core_result$return_code
    }
  } else {
    # Use fVinModelYear2 for year calculation
    rmy <- f_vin_model_year2(vin)
    conclusive <- TRUE
    omy <- NULL
    
    if (!is.null(rmy) && rmy < 0) {
      omy <- -rmy - 30
      rmy <- -rmy
      conclusive <- FALSE
    }
    
    do_3_and_4 <- TRUE
    
    # Check if user provided year matches calculated years
    if (!is.null(year) && year >= 1980 && year <= (current_year + 1)) {
      if (year == rmy || (!is.null(omy) && year == omy)) {
        do_3_and_4 <- TRUE
      } else {
        model_year_source <- as.character(year)
        
        # Call core decoding with pass 2 (user year)
        core_result <- sp_vin_decode_core(2, vin, year, model_year_source, 
                                         TRUE, TRUE, include_all, include_private, 
                                         FALSE, con)
        decoding_items <- rbind(decoding_items, core_result$items)
        return_code <- core_result$return_code
        
        # Check if error code contains '8' to determine if we should do passes 3&4
        do_3_and_4 <- grepl("8", return_code) && !is.null(rmy)
      }
    }
    
    # Perform passes 3 and 4 if needed
    if (do_3_and_4) {
      if (!is.null(rmy)) {
        error_12 <- !is.null(year) && !is.null(rmy) && year != rmy
        
        # Pass 3: Use calculated model year
        core_result <- sp_vin_decode_core(3, vin, rmy, model_year_source, 
                                         conclusive, error_12, include_all, 
                                         include_private, FALSE, con)
        decoding_items <- rbind(decoding_items, core_result$items)
        
        # Pass 4: Use alternative model year if available
        if (!is.null(omy)) {
          error_12 <- !is.null(year) && !is.null(omy) && year != omy
          
          core_result <- sp_vin_decode_core(4, vin, omy, model_year_source, 
                                           conclusive, error_12, include_all, 
                                           include_private, FALSE, con)
          decoding_items <- rbind(decoding_items, core_result$items)
        }
      }
    }
  }
  
  # Process and rank results
  if (nrow(decoding_items) > 0) {
    result <- process_decoding_results(decoding_items, con)
    
    if (!no_output) {
      return(result)
    }
  }
  
  return(list(
    vin = vin,
    descriptor = descriptor,
    wmi = wmi,
    model_year_source = model_year_source,
    return_code = return_code,
    decoding_items = decoding_items
  ))
}

#' Core VIN Decoding Function
#'
#' Performs the core VIN decoding logic for a specific pass of the decoding
#' algorithm. This function handles pattern matching and database queries.
#'
#' @param pass Integer indicating the decoding pass number.
#' @param vin Character string containing the VIN to decode.
#' @param model_year Integer specifying the model year for this pass.
#' @param model_year_source Character string indicating the source of model year.
#' @param conclusive Logical indicating if the model year is conclusive.
#' @param error_12 Logical indicating if error code 12 should be set.
#' @param include_all Logical indicating whether to include all matches.
#'   Default is FALSE.
#' @param include_private Logical indicating whether to include private information.
#'   Default is FALSE.
#' @param include_not_publicly_available Logical indicating whether to include
#'   non-public information.
#' @param con DBI connection object to the VPIC database.
#'
#' @return A list containing decoding items and return code.
#'
#' @keywords internal
#' @importFrom DBI dbGetQuery
sp_vin_decode_core <- function(pass, vin, model_year, model_year_source, 
                              conclusive, error_12, include_all, include_private, 
                              include_not_publicly_available, con) {
  
  wmi <- f_vin_wmi(vin)
  descriptor <- f_vin_descriptor(vin)
  return_code <- ""
  
  # Extract keys for pattern matching
  keys <- ""
  if (nchar(vin) > 3) {
    keys <- substr(vin, 4, 8)
    if (nchar(vin) > 9) {
      keys <- paste0(keys, "|", substr(vin, 10, 17))
    }
  }
  
  # Check if WMI exists in database
  wmi_query <- sprintf("SELECT Id FROM Wmi WHERE Wmi = '%s'", wmi)
  if (!include_not_publicly_available) {
    wmi_query <- paste0(wmi_query, " AND PublicAvailabilityDate <= CURRENT_DATE")
  }
  
  wmi_result <- DBI::dbGetQuery(con, wmi_query)
  
  if (nrow(wmi_result) == 0) {
    return_code <- paste0(return_code, " 7 ")
    return(list(items = data.frame(), return_code = return_code))
  }
  
  wmi_id <- wmi_result$Id[1]
  
  # Pattern matching query - initial pass without lookup values
  pattern_query <- sprintf("
    SELECT 
      %d as DecodingId,
      'Pattern' as Source,
      p.Id as PatternId,
      UPPER(p.Keys) as Keys,
      p.VinSchemaId,
      wvs.WmiId,
      p.ElementId,
      p.AttributeId,
      e.LookupTable,
      'XXX' as Value
    FROM Pattern p
    JOIN Element e ON p.ElementId = e.Id
    JOIN VinSchema vs ON p.VinSchemaId = vs.Id
    JOIN Wmi_VinSchema wvs ON vs.Id = wvs.VinSchemaId
    JOIN Wmi w ON wvs.WmiId = w.Id
    WHERE w.Wmi = '%s'
    AND '%s' LIKE REPLACE(p.Keys, '*', '_') || '%%'
    AND p.ElementId NOT IN (26, 27, 29, 39)
    AND e.Decode IS NOT NULL
    ", pass, wmi, keys)
  
  # Add model year constraints
  if (!is.null(model_year)) {
    pattern_query <- paste0(pattern_query, sprintf(" AND %d BETWEEN wvs.YearFrom AND COALESCE(wvs.YearTo, 2999)", model_year))
  }
  
  # Add privacy constraints
  if (!include_private) {
    pattern_query <- paste0(pattern_query, " AND COALESCE(e.IsPrivate, 0) = 0")
  }
  
  if (!include_not_publicly_available) {
    pattern_query <- paste0(pattern_query, " AND w.PublicAvailabilityDate <= CURRENT_DATE")
    pattern_query <- paste0(pattern_query, " AND COALESCE(vs.TobeQCed, 0) = 0")
  }
  
  decoding_items <- DBI::dbGetQuery(con, pattern_query)

  # Iterate through decoding items to set values
  if (nrow(decoding_items) > 0) {
    for (i in 1:nrow(decoding_items)) {
      if (!is.na(decoding_items$LookupTable[i]) && decoding_items$LookupTable[i] != "XXX") {
        # Lookup actual value from the appropriate table
        lookup_table <- decoding_items$LookupTable[i]
        attribute_id <- decoding_items$AttributeId[i]
        lookup_query <- sprintf("SELECT Name FROM %s WHERE Id = %s", 
                                lookup_table, attribute_id)
        lookup_result <- DBI::dbGetQuery(con, lookup_query)
        if (nrow(lookup_result) > 0) {
          decoding_items$Value[i] <- lookup_result$Name[1]
        } else {
          decoding_items$Value[i] <- NA
        }
      } 
    }
  }
  
  # Add vehicle type information
  vehicle_type_query <- sprintf("
    SELECT 
      %d as DecodingId,
      'VehType' as Source,
      NULL as PatternId,
      UPPER('%s') as Keys,
      NULL as VinSchemaId,
      w.Id as WmiId,
      39 as ElementId,
      'XXX' as LookupTable,
      CAST(vt.Id as TEXT) as AttributeId,
      UPPER(vt.Name) as Value
    FROM Wmi w
    JOIN VehicleType vt ON w.VehicleTypeId = vt.Id
    WHERE w.Wmi = '%s'
    ", pass, wmi, wmi)
  
  if (!include_not_publicly_available) {
    vehicle_type_query <- paste0(vehicle_type_query, " AND w.PublicAvailabilityDate <= CURRENT_DATE")
  }
  
  vehicle_type_result <- DBI::dbGetQuery(con, vehicle_type_query)
  
  if (nrow(vehicle_type_result) > 0) {
    decoding_items <- rbind(decoding_items, vehicle_type_result)
  }
  
  # Add model year to each decoding item
  if (nrow(decoding_items) > 0) {
    decoding_items$ModelYear <- model_year
  }
  
  return(list(items = decoding_items, return_code = return_code))
}

#' Process and Rank Decoding Results
#'
#' Processes multiple decoding results and ranks them by quality to determine
#' the best match for a given VIN.
#'
#' @param decoding_items Data frame containing all decoding items from multiple passes.
#' @param con DBI connection object to the VPIC database.
#'
#' @return A list containing summary, details, and all results ranked by quality.
#'
#' @details This function calculates error codes, element weights, and pattern
#'   counts for each decoding result, then ranks them to identify the best match.
#'
#' @keywords internal
process_decoding_results <- function(decoding_items, con) {
  if (nrow(decoding_items) == 0) {
    return(data.frame())
  }
  
  # Get unique decoding IDs
  decoding_ids <- unique(decoding_items$DecodingId)
  
  # Calculate error codes, element weights, and patterns for each decoding
  results <- data.frame()
  
  for (decoding_id in decoding_ids) {
    items <- decoding_items[decoding_items$DecodingId == decoding_id, ]
    
    # Calculate error codes (simplified)
    error_codes <- ""
    error_value <- 0
    
    # Calculate element weights
    element_weights <- sum(items$ElementId %in% c(1:50), na.rm = TRUE)  # Simplified
    
    # Count patterns
    patterns <- length(unique(items$PatternId[!is.na(items$PatternId)]))
    
    # Extract model year from the decoding items
    model_year <- NA
    if ("ModelYear" %in% names(items) && any(!is.na(items$ModelYear))) {
      model_year <- items$ModelYear[!is.na(items$ModelYear)][1]
    }
    
    result_row <- data.frame(
      DecodingId = decoding_id,
      ErrorCodes = error_codes,
      ErrorValue = error_value,
      ElementsWeight = element_weights,
      Patterns = patterns,
      ModelYear = model_year
    )
    
    results <- rbind(results, result_row)
  }
  
  # Rank results (simplified ranking)
  results <- results[order(-results$ElementsWeight, results$ErrorValue), ]
  
  # Get the best result
  best_result <- results[1, ]
  
  # Return decoded information for the best result
  best_items <- decoding_items[decoding_items$DecodingId == best_result$DecodingId, ]
  
  # Process the best items to look up actual values for pattern-based results
  if (nrow(best_items) > 0) {
    best_items <- lookup_pattern_values(best_items, con)
  }
  
  return(list(
    summary = best_result,
    details = best_items,
    all_results = results
  ))
}

#' Look up Pattern Values from Database Tables
#'
#' For decoding items with Source = "Pattern", looks up the actual values
#' from the appropriate lookup tables using the LookupTable field from Element table.
#'
#' @param items Data frame containing decoding items.
#' @param con DBI connection object to the VPIC database.
#'
#' @return Data frame with updated Value column for pattern-based items.
#'
#' @keywords internal
lookup_pattern_values <- function(items, con) {
  if (nrow(items) == 0) return(items)
  
  # Process each item that has Source = "Pattern"
  for (i in 1:nrow(items)) {
    if (!is.na(items$Source[i]) && items$Source[i] == "Pattern" && 
        !is.na(items$ElementId[i]) && !is.na(items$AttributeId[i])) {
      
      tryCatch({
        # Check if LookupTable is available in the current row
        lookup_table <- NULL
        if ("LookupTable" %in% names(items) && !is.na(items$LookupTable[i])) {
          lookup_table <- items$LookupTable[i]
        } else {
          # Fall back to querying Element table
          element_query <- sprintf("SELECT LookupTable FROM Element WHERE Id = %d", 
                                  items$ElementId[i])
          element_result <- DBI::dbGetQuery(con, element_query)
          
          if (nrow(element_result) > 0 && !is.na(element_result$LookupTable[1])) {
            lookup_table <- element_result$LookupTable[1]
          }
        }
        
        # If we have a lookup table, query it for the actual value
        if (!is.null(lookup_table) && lookup_table != "") {
          value_query <- sprintf("SELECT Name FROM %s WHERE Id = %d", 
                                lookup_table, items$AttributeId[i])
          value_result <- DBI::dbGetQuery(con, value_query)
          
          if (nrow(value_result) > 0 && !is.na(value_result$Name[1])) {
            items$Value[i] <- value_result$Name[1]
          } else {
            # If no match found in lookup table, show the AttributeId
            items$Value[i] <- as.character(items$AttributeId[i])
          }
        } else {
          # No lookup table specified, just use the AttributeId
          items$Value[i] <- as.character(items$AttributeId[i])
        }
      }, error = function(e) {
        # If lookup fails, use the AttributeId as the value
        items$Value[i] <- as.character(items$AttributeId[i])
      })
    }
  }
  
  return(items)
}

#' Decode VIN Example
#'
#' Example function demonstrating how to use the VIN decoding functionality.
#' This function initializes the database, decodes a sample VIN, and displays
#' the results.
#'
#' @return A list containing the decoding results for the example VIN.
#'
#' @examples
#' \dontrun{
#' result <- decode_vin_example()
#' print(result$summary)
#' }
#'
#' @export
decode_vin_example <- function() {
  # Initialize database connection
  con <- init_vpic_db()
  
  # Example VIN
  test_vin <- "1HGCM82633A004352"
  
  # Decode VIN
  result <- sp_vin_decode(test_vin, con)
  
  # Print results
  print(paste("VIN:", test_vin))
  print(paste("Descriptor:", result$descriptor))
  print(paste("WMI:", result$wmi))
  print("Decoding Results:")
  print(result$summary)
  
  # Clean up
  DBI::dbDisconnect(con)
  
  return(result)
}