# BRS Results Summary Script - Interactive Lag Selection per File
# This script searches for BRS_results.csv files, displays sequence counts,
# and allows selection of specific lag for each file

library(tidyverse)

# Function to find all BRS_results.csv files recursively
find_brs_files <- function(export_dir = file.path(getwd(), "export")) {
  list.files(path = export_dir, 
             pattern = "_BRS_results\\.csv$", 
             recursive = TRUE, 
             full.names = TRUE)
}

# Function to read and display a single BRS results file
display_brs_file <- function(file_path) {
  # Read the CSV
  brs_data <- read.csv(file_path, row.names = 1)
  
  # Extract filename without path and extension
  filename <- basename(file_path)
  base_filename <- sub("_BRS_results\\.csv$", "", filename)
  
  # Create summary with sequence counts
  summary_df <- data.frame(
    lag = rownames(brs_data),
    raw_sequences = brs_data$seq_num_raw,
    filtered_sequences = brs_data$seq_num,
    mean_slope = round(brs_data$mean_slope, 3),
    sd_slope = round(brs_data$sd_slope, 3),
    stringsAsFactors = FALSE
  )
  
  cat("\n========================================\n")
  cat("File:", base_filename, "\n")
  cat("========================================\n")
  print(summary_df, row.names = FALSE)
  cat("\n")
  
  return(list(filename = base_filename, data = brs_data))
}

# Function to get user's lag selection for a file
get_lag_selection <- function(available_lags) {
  while (TRUE) {
    cat("Select lag (", paste(available_lags, collapse = ", "), ") or 'skip' to skip this file: ")
    selection <- tolower(trimws(readline()))
    
    if (selection == "skip") {
      return(NULL)
    }
    
    if (selection %in% available_lags) {
      return(selection)
    }
    
    cat("Invalid selection. Please try again.\n")
  }
}

# Interactive function to process files one by one
interactive_file_by_file <- function(output_filename = "BRS_summary.csv") {
  cat("=== BRS Results Interactive Summary Tool ===\n")
  cat("You will be shown each file and can select which lag to use.\n\n")
  
  # Find all BRS results files
  brs_files <- find_brs_files()
  
  if (length(brs_files) == 0) {
    cat("No BRS results files found in export folder.\n")
    return(NULL)
  }
  
  # Create summary folder if it doesn't exist
  summary_dir <- file.path(getwd(), "export", "summary")
  if (!dir.exists(summary_dir)) {
    dir.create(summary_dir, recursive = TRUE)
  }
  
  # Check if summary file already exists and load previously processed files
  output_path <- file.path(summary_dir, output_filename)
  processed_files <- character(0)
  existing_summary <- NULL
  
  if (file.exists(output_path)) {
    existing_summary <- read.csv(output_path, stringsAsFactors = FALSE)
    processed_files <- unique(existing_summary$filename)
    cat("Found existing summary with", length(processed_files), "files already processed\n")
    cat("These files will be skipped automatically.\n\n")
  }
  
  cat("Total files found:", length(brs_files), "\n")
  cat("Files to process:", length(brs_files) - length(processed_files), "\n\n")
  
  # Process each file
  extracted_data <- list()
  skipped_count <- 0
  processed_count <- 0
  user_skipped_count <- 0
  
  for (i in seq_along(brs_files)) {
    # Extract filename
    filename <- sub("_BRS_results\\.csv$", "", basename(brs_files[i]))
    
    # Check if this file has already been processed
    if (filename %in% processed_files) {
      skipped_count <- skipped_count + 1
      cat("[", i, "/", length(brs_files), "] Skipping (already in summary):", filename, "\n")
      next
    }
    
    # Display the file information
    file_info <- display_brs_file(brs_files[i])
    
    # Get available lags
    available_lags <- rownames(file_info$data)
    
    # Ask user to select lag
    selected_lag <- get_lag_selection(available_lags)
    
    # If user skipped, continue to next file
    if (is.null(selected_lag)) {
      user_skipped_count <- user_skipped_count + 1
      cat("Skipped by user.\n")
      next
    }
    
    # Extract the selected lag row
    lag_row <- file_info$data[selected_lag, , drop = FALSE]
    
    # Add filename and lag columns
    lag_row <- cbind(filename = filename, lag = selected_lag, lag_row)
    
    extracted_data[[length(extracted_data) + 1]] <- lag_row
    processed_count <- processed_count + 1
    cat("✓ Added", selected_lag, "from", filename, "\n")
  }
  
  # Combine and save results
  if (length(extracted_data) > 0) {
    new_summary_data <- bind_rows(extracted_data)
    
    # If there's existing data, append new data
    if (!is.null(existing_summary)) {
      summary_data <- bind_rows(existing_summary, new_summary_data)
    } else {
      summary_data <- new_summary_data
    }
    
    # Save to summary folder
    write.csv(summary_data, output_path, row.names = FALSE)
    
    cat("\n=== Final Summary ===\n")
    cat("Files already in summary (auto-skipped):", skipped_count, "\n")
    cat("Files skipped by user:", user_skipped_count, "\n")
    cat("New files added to summary:", processed_count, "\n")
    cat("Total files in summary:", nrow(summary_data), "\n")
    cat("\nSummary saved to:", output_path, "\n")
    
    return(summary_data)
  } else {
    cat("\n=== Final Summary ===\n")
    if (skipped_count > 0) {
      cat("All files were either already processed or skipped by user.\n")
      cat("No new data added to summary.\n")
    } else {
      cat("No data was selected for processing.\n")
    }
    return(NULL)
  }
}

# Batch mode function - select same lag for all remaining files
batch_mode <- function(selected_lag = "lag0", output_filename = "BRS_summary.csv") {
  cat("=== BRS Results Batch Mode ===\n")
  cat("Extracting", selected_lag, "from all unprocessed files...\n\n")
  
  # Find all BRS results files
  brs_files <- find_brs_files()
  
  if (length(brs_files) == 0) {
    cat("No BRS results files found.\n")
    return(NULL)
  }
  
  # Create summary folder if it doesn't exist
  summary_dir <- file.path(getwd(), "export", "summary")
  if (!dir.exists(summary_dir)) {
    dir.create(summary_dir, recursive = TRUE)
  }
  
  # Check if summary file already exists
  output_path <- file.path(summary_dir, output_filename)
  processed_files <- character(0)
  existing_summary <- NULL
  
  if (file.exists(output_path)) {
    existing_summary <- read.csv(output_path, stringsAsFactors = FALSE)
    processed_files <- unique(existing_summary$filename)
    cat("Found existing summary with", length(processed_files), "files already processed\n\n")
  }
  
  # Extract selected lag from each file
  extracted_data <- list()
  skipped_count <- 0
  new_count <- 0
  
  for (i in seq_along(brs_files)) {
    filename <- sub("_BRS_results\\.csv$", "", basename(brs_files[i]))
    
    if (filename %in% processed_files) {
      skipped_count <- skipped_count + 1
      cat("Skipping (already processed):", filename, "\n")
      next
    }
    
    brs_data <- read.csv(brs_files[i], row.names = 1)
    
    if (!(selected_lag %in% rownames(brs_data))) {
      warning(paste("Lag", selected_lag, "not found in", filename))
      next
    }
    
    lag_row <- brs_data[selected_lag, , drop = FALSE]
    lag_row <- cbind(filename = filename, lag = selected_lag, lag_row)
    
    extracted_data[[length(extracted_data) + 1]] <- lag_row
    new_count <- new_count + 1
    cat("Processing:", filename, "\n")
  }
  
  # Combine and save results
  if (length(extracted_data) > 0) {
    new_summary_data <- bind_rows(extracted_data)
    
    if (!is.null(existing_summary)) {
      summary_data <- bind_rows(existing_summary, new_summary_data)
    } else {
      summary_data <- new_summary_data
    }
    
    write.csv(summary_data, output_path, row.names = FALSE)
    
    cat("\n=== Summary ===\n")
    cat("Files skipped (already processed):", skipped_count, "\n")
    cat("New files processed:", new_count, "\n")
    cat("Total files in summary:", nrow(summary_data), "\n")
    cat("Summary saved to:", output_path, "\n")
    
    return(summary_data)
  } else {
    if (skipped_count > 0) {
      cat("\nAll files have already been processed.\n")
    } else {
      cat("No data extracted.\n")
    }
    return(NULL)
  }
}

# ===== USAGE =====

# Interactive mode - select lag for each file individually
summary_data <- interactive_file_by_file()

# Batch mode - use same lag for all files (skips already processed)
# summary_data <- batch_mode("lag0", "BRS_summary.csv")