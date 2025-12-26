suppressMessages({
packages <- c("tidyverse", "ggpubr", "rstatix","stringr","magick","ggsci","ggplot2","fs","patchwork")
install.packages(setdiff(packages, rownames(installed.packages()))) 
library(ggplot2)
library(tidyverse)
library(reshape2)
library(stringr)
library(magick)
library(fs)
library(gridExtra)
library(patchwork)
#filesystem
folder <- "data_csv"

if (file.exists(folder)) {
  
  cat("")
  
} else {
  
  dir.create(folder)
  
}
# Set the directory path where the files are located
folder_path <- "./data_csv"

# List all the CSV files in the folder
files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)


# Iterate over the files and read them into a list
df_list <- list()

# Initialize progress bar
total_files <- length(files)
pb <- txtProgressBar(min = 0, max = total_files, style = 3)
on.exit(close(pb))

for (i in seq_along(files)) {
  file <- files[i]
  file.id <- str_replace_all(string=file, pattern=".csv", repl="")
  file.id <- str_replace_all(string=file.id, pattern="./data/", repl="")
  
  
  folder2 <- "export"
  
  if (file.exists(folder2)) {
    
    cat("")
    
  } else {
    
    dir.create(folder2)
    
  }
  #### DATA LOAD
  print(file.id)
  file_id <- file.id
  raw_df <- read_delim(file.path(getwd(), file))
  
  # Find columns containing "RRI" and "SBP" (case-insensitive)
  rri_col <- grep("RRI", names(raw_df), ignore.case = TRUE, value = TRUE)[1]
  sbp_col <- grep("SBP", names(raw_df), ignore.case = TRUE, value = TRUE)[1]
  
  # Check if both columns were found
  if (is.na(rri_col) || is.na(sbp_col)) {
    stop("Error: Could not find columns containing 'RRI' and/or 'SBP'. 
       Available columns: ", paste(names(raw_df), collapse = ", "))
  }
  
  # Print which columns were found
  cat("Using columns:", rri_col, "and", sbp_col, "\n")
  
  # Select and rename the columns to standardized names
  df <- data.frame(
    RRI = as.numeric(raw_df[[rri_col]]),
    SBP = as.numeric(raw_df[[sbp_col]])
  )
  
  # Process the data
  df$SBP <- round(df$SBP, 3)
  df$RRI <- round(df$RRI, 3) * 1000
  df <- na.omit(df)
  df$index <- 1:nrow(df)
  #Lag0_df
  lag0_df <- df
  #lag1_df
  lag1_df <- df %>% mutate(SBP = lead(SBP, 1))
  #lag2_df
  lag2_df <- df %>% mutate(SBP = lead(SBP, 2))
  
  # Unified function to find sequences of increasing or decreasing values
  find_sequences <- function(df, direction = "increasing") {
    # Initialize columns to indicate value trends based on direction
    if (direction == "increasing") {
      df$RRI_trend <- c(FALSE, diff(df$RRI) > 0)
      df$SBP_trend <- c(FALSE, diff(df$SBP) > 0)
      trend_indicator <- TRUE
    } else if (direction == "decreasing") {
      df$RRI_trend <- c(FALSE, diff(df$RRI) < 0)
      df$SBP_trend <- c(FALSE, diff(df$SBP) < 0)
      trend_indicator <- FALSE
    } else {
      stop("Invalid direction. Use 'increasing' or 'decreasing'.")
    }
    
    # Initialize a list to store sequences
    sequences <- list()
    
    i <- 1
    while (i <= nrow(df) - 2) {
      # Skip if the next values are NA
      if (any(is.na(df$RRI_trend[i + 1:2])) || any(is.na(df$SBP_trend[i + 1:2]))) {
        i <- i + 1
        next
      }
      
      # Check for a sequence starting at the current index
      if (df$RRI_trend[i + 1] && df$RRI_trend[i + 2] &&
          df$SBP_trend[i + 1] && df$SBP_trend[i + 2]) {
        sequence_start <- i
        
        # Move forward to find the end of the sequence
        while (i <= nrow(df) - 1 && !is.na(df$RRI_trend[i + 1]) &&
               df$RRI_trend[i + 1] && !is.na(df$SBP_trend[i + 1]) &&
               df$SBP_trend[i + 1]) {
          i <- i + 1
        }
        
        # Adjust the sequence end to exclude the last non-trending value
        sequence_end <- i
        
        # Store the sequence if it has at least three values
        if (sequence_end - sequence_start + 1 >= 3) {
          sequence <- df[sequence_start:sequence_end, ]
          sequence$increasing <- trend_indicator
          sequences <- append(sequences, list(sequence))
        }
      }
      
      i <- i + 1
    }
    
    return(sequences)
  }
  
  # Apply the function to each dataframe for increasing sequences
  lag0_inc_sequences <- find_sequences(lag0_df, "increasing")
  lag1_inc_sequences <- find_sequences(lag1_df, "increasing")
  lag2_inc_sequences <- find_sequences(lag2_df, "increasing")
  
  # Apply the function to each dataframe for decreasing sequences
  lag0_dec_sequences <- find_sequences(lag0_df, "decreasing")
  lag1_dec_sequences <- find_sequences(lag1_df, "decreasing")
  lag2_dec_sequences <- find_sequences(lag2_df, "decreasing")
  
  ######REGRESSION SLOPES
  # Function to fit linear models, extract slope and R-squared, and add increasing/decreasing indicator
  extract_lm_slope_rsquared <- function(sequences, is_increasing) {
    # Check if sequences is empty
    if (length(sequences) == 0) {
      return(data.frame(index = integer(), 
                        slope = numeric(), 
                        rsquared = numeric(), 
                        increasing = logical(), 
                        stringsAsFactors = FALSE))
    }
    
    # Create an empty list to store the linear regression models
    lm_list <- list()
    
    # Loop through the sequences using seq_along (safer than 1:length)
    for (i in seq_along(sequences)) {
      # Skip if sequence is NULL or has insufficient data
      if (is.null(sequences[[i]]) || nrow(sequences[[i]]) < 2) {
        lm_list[[i]] <- NULL
        next
      }
      
      # Fit a linear regression model to the current sequence
      tryCatch({
        lm_model <- lm(RRI ~ SBP, data = sequences[[i]])
        lm_list[[i]] <- lm_model
      }, error = function(e) {
        lm_list[[i]] <- NULL
        warning(paste("Error fitting model for sequence", i, ":", e$message))
      })
    }
    
    # Create an empty data frame to store the slope coefficients, R-squared values, and increasing indicator
    slope_df <- data.frame(index = integer(), 
                           slope = numeric(), 
                           rsquared = numeric(), 
                           increasing = logical(), 
                           stringsAsFactors = FALSE)
    
    # Loop through the linear regression models in lm_list
    for (i in seq_along(lm_list)) {
      # Skip if model is NULL
      if (is.null(lm_list[[i]])) {
        next
      }
      
      # Extract the slope coefficient from the linear regression model
      slope <- coef(lm_list[[i]])[2]  # Assuming the slope is the second coefficient
      rsquared <- summary(lm_list[[i]])$r.squared
      
      # Create a new row in the slope_df with the index, slope coefficient, R-squared value, and increasing indicator
      slope_df <- rbind(slope_df, data.frame(index = i, 
                                             slope = slope, 
                                             rsquared = rsquared, 
                                             increasing = is_increasing, 
                                             stringsAsFactors = FALSE))
    }
    
    return(slope_df)
  }
  
  
  # Apply the function to each set of sequences
  lag0_slope_df_inc <- extract_lm_slope_rsquared(lag0_inc_sequences,TRUE)
  lag1_slope_df_inc <- extract_lm_slope_rsquared(lag1_inc_sequences,TRUE)
  lag2_slope_df_inc <- extract_lm_slope_rsquared(lag2_inc_sequences,TRUE)
  lag0_slope_df_dec <- extract_lm_slope_rsquared(lag0_dec_sequences,FALSE)
  lag1_slope_df_dec <- extract_lm_slope_rsquared(lag1_dec_sequences,FALSE)
  lag2_slope_df_dec <- extract_lm_slope_rsquared(lag2_dec_sequences,FALSE)
  #Rsquared filter 0.85
  lag0_slope_df_inc_filtered <- filter(lag0_slope_df_inc,rsquared>=0.85)
  lag0_slope_df_dec_filtered <- filter (lag0_slope_df_dec,rsquared>=0.85)
  lag1_slope_df_inc_filtered <- filter(lag1_slope_df_inc,rsquared>=0.85)
  lag1_slope_df_dec_filtered <- filter (lag1_slope_df_dec,rsquared>=0.85)
  lag2_slope_df_inc_filtered <- filter(lag2_slope_df_inc,rsquared>=0.85)
  lag2_slope_df_dec_filtered <- filter (lag2_slope_df_dec,rsquared>=0.85)
  #combine
  lag0_final_full <- rbind(lag0_slope_df_inc_filtered,lag0_slope_df_dec_filtered)
  lag1_final_full <- rbind(lag1_slope_df_inc_filtered,lag1_slope_df_dec_filtered)
  lag2_final_full <- rbind(lag2_slope_df_inc_filtered,lag2_slope_df_dec_filtered)
  
  lag0_sequences<-c(lag0_inc_sequences,lag0_dec_sequences)
  lag1_sequences<-c(lag1_inc_sequences,lag1_dec_sequences)
  lag2_sequences<-c(lag2_inc_sequences,lag2_dec_sequences)
  
###Summary
  calculate_summary_stats <- function(final_full, slope_df_inc_filtered, slope_df_dec_filtered, sequences, inc_sequences, dec_sequences) {
    list(
      mean_slope = mean(final_full$slope),
      sd_slope = sd(final_full$slope),
      seq_num = nrow(final_full),
      seq_num_raw = length(sequences),
      mean_inc_slope = mean(slope_df_inc_filtered$slope),
      sd_inc_slope = sd(slope_df_inc_filtered$slope),
      mean_dec_slope = mean(slope_df_dec_filtered$slope),
      sd_dec_slope = sd(slope_df_dec_filtered$slope),
      inc_seq_num_raw = length(inc_sequences),
      inc_seq_num = nrow(slope_df_inc_filtered),
      dec_seq_num_raw = length(dec_sequences),
      dec_seq_num = nrow(slope_df_dec_filtered)
    )
  }
  # Calculate summaries for lag0
  lag0_summary <- calculate_summary_stats(
    lag0_final_full, 
    lag0_slope_df_inc_filtered, 
    lag0_slope_df_dec_filtered, 
    lag0_sequences, 
    lag0_inc_sequences, 
    lag0_dec_sequences
  )
  
  # Calculate summaries for lag1
  lag1_summary <- calculate_summary_stats(
    lag1_final_full, 
    lag1_slope_df_inc_filtered, 
    lag1_slope_df_dec_filtered, 
    lag1_sequences, 
    lag1_inc_sequences, 
    lag1_dec_sequences
  )
  
  # Calculate summaries for lag2
  lag2_summary <- calculate_summary_stats(
    lag2_final_full, 
    lag2_slope_df_inc_filtered, 
    lag2_slope_df_dec_filtered, 
    lag2_sequences, 
    lag2_inc_sequences, 
    lag2_dec_sequences
  )
  
  
  df_results_list <- rbind(lag0_summary,lag1_summary,lag2_summary)
  
  ###RAW EXPORT
  # Combine the lag0_list into a single dataframe
  lag0_beats <- do.call(rbind, lag0_sequences)
  lag1_beats <- do.call(rbind,lag1_sequences)
  lag2_beats <- do.call(rbind,lag2_sequences)
  #FILESYSTEM RESULTS
  # Check if file has been previously analyzed
  if (file.exists(file.path(getwd(), "export", file.id), recursive = TRUE)) {
    cat("File previously analyzed - skipping:", file.id, "\n")
    next  # Skip to the next file in the loop
  } else {
    dir.create(file.path(getwd(), "export", file.id), recursive = TRUE)
  }
  
  # Create plots directory
  if (!file.exists(file.path(getwd(), "export", file.id, "plots"), recursive = TRUE)) {
    dir.create(file.path(getwd(), "export", file.id, "plots"), recursive = TRUE)
  }
  # Extract the base filename without extension
  base_filename <- tools::file_path_sans_ext(basename(file))
  
  # Export the combined dataframe as a CSV file with filename prefix
  write.csv(lag0_beats, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_lag0_data.csv")), 
            row.names = FALSE)
  
  write.csv(lag1_beats, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_lag1_data.csv")), 
            row.names = FALSE)
  
  write.csv(lag2_beats, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_lag2_data.csv")), 
            row.names = FALSE)
  
  write.csv(lag0_final_full, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_lag0_slope.csv")), 
            row.names = FALSE)
  
  write.csv(lag1_final_full, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_lag1_slope.csv")), 
            row.names = FALSE)
  
  write.csv(lag2_final_full, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_lag2_slope.csv")), 
            row.names = FALSE)
  
  write.csv(df_results_list, 
            file.path(getwd(), "export", file.id, paste0(base_filename, "_BRS_results.csv")), 
            row.names = TRUE)
   #BRS Sequence plot

  # Define the plotting functions
  create_plot <- function(df) {
    lm_model <- lm(RRI ~ SBP, data = df)
    slope <- coef(lm_model)[2]
    r_squared <- summary(lm_model)$r.squared
    
    ggplot(df, aes(x = SBP, y = RRI, color = as.factor(increasing))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"), 
                         labels = c("Increasing", "Decreasing")) +
      theme_classic() +
      theme(
        text = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none"  # Adjust legend position as needed
      ) +
      guides(x = guide_axis(angle = 90)) +
      labs(color = "Trend") +
      annotate("text", x = min(df$SBP), y = max(df$RRI), 
               label = sprintf("Slope: %.2f\nR²: %.2f", slope, r_squared), 
               hjust = 0, vjust = 1, size = 3, color = "black", fontface = "italic")
  }
  
  create_plot_if <- function(df) {
    # Fit a linear model to get slope and R-squared
    lm_model <- lm(RRI ~ SBP, data = df)
    slope <- coef(lm_model)[2]
    r_squared <- summary(lm_model)$r.squared
    
    # Check if R-squared is greater than or equal to 0.85
    if (r_squared >= 0.85) {
      ggplot(df, aes(x = SBP, y = RRI, color = as.factor(increasing))) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"), 
                           labels = c("Increasing", "Decreasing")) +
        theme_classic() +
        theme(
          text = element_text(size = 8),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none"  # Adjust legend position as needed
        ) +
        guides(x = guide_axis(angle = 90)) +
        labs(color = "Trend") +
        annotate("text", x = min(df$SBP), y = max(df$RRI), 
                 label = sprintf("Slope: %.2f\nR²: %.2f", slope, r_squared), 
                 hjust = 0, vjust = 1, size = 3, color = "black", fontface = "italic")
    } else {
      # Return NULL if R-squared is below the threshold
      return(NULL)
    }
  }
  #title function
  create_plot_with_title <- function(plot, file_id) {
    plot + ggtitle(paste("Filtered Lag0 Plot for:", file_id))
  }
  # Extract the base filename without extension
  base_filename <- tools::file_path_sans_ext(basename(file))
  
  #plots
  
  lag0_plots_filtered <- map(lag0_sequences, create_plot_if)
  lag0_plot_filtered <- wrap_plots(Filter(Negate(is.null), lag0_plots_filtered))
  lag0_plot_filtered_with_title <- create_plot_with_title(lag0_plot_filtered, file_id)
  ggsave(file.path(getwd(), "export", file.id, "plots", paste0(base_filename, "_lag0_plot_filtered.png")), 
         plot = lag0_plot_filtered, width = 10, height = 10, dpi = 400)
  
  
  lag1_plots_filtered <- map(lag1_sequences, create_plot_if)
  lag1_plot_filtered <- wrap_plots(Filter(Negate(is.null), lag1_plots_filtered))
  lag1_plot_filtered_with_title <- create_plot_with_title(lag1_plot_filtered, file_id)
  ggsave(file.path(getwd(), "export", file.id, "plots", paste0(base_filename, "_lag1_plot_filtered.png")), 
         plot = lag1_plot_filtered, width = 10, height = 10, dpi = 400)
  
 
  lag2_plots_filtered <- map(lag2_sequences, create_plot_if)
  lag2_plot_filtered <- wrap_plots(Filter(Negate(is.null), lag2_plots_filtered))
  lag2_plot_filtered_with_title <- create_plot_with_title(lag2_plot_filtered, file_id)
  ggsave(file.path(getwd(), "export", file.id, "plots", paste0(base_filename, "_lag2_plot_filtered.png")), 
         plot = lag2_plot_filtered, width = 10, height = 10, dpi = 400)
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}
print("All BRS Analyzed")
})
