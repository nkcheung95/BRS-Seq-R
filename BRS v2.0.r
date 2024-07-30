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
###TESTING
folder <- "data_csv"

if (file.exists(folder)) {
  
  cat("The folder already exists")
  
} else {
  
  dir.create(folder)
  
}
# Set the directory path where the files are located
folder_path <- "./data_csv"

# List all the CSV files in the folder
files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)


# Iterate over the files and read them into a list of lag0_list
df_list <- list()
for (file in files) {
  
  file.id <- str_replace_all(string=file, pattern=".csv", repl="")
  file.id <- str_replace_all(string=file.id, pattern="./data/", repl="")
  
  
  folder2 <- "export"
  
  if (file.exists(folder2)) {
    
    cat("The folder already exists")
    
  } else {
    
    dir.create(folder2)
    
  }
  ####DATA LOAD
  print(file.id)
  raw_df <- read_delim(file.path(getwd(),file))
  
  df <- subset(raw_df, select = c(RRI,SBP) )
  df$SBP <- as.numeric(df$SBP)
  df$SBP <- round(df$SBP,3)
  df$RRI <- as.numeric(df$RRI)
  df$RRI <- round(df$RRI,3)*1000
  df <- na.omit(df)
  df$index <- c(1:nrow(df))
  
  #Lag0_df
  lag0_df <- df
  #lag1_df
  lag1_df <- df %>% mutate(SBP = lead(SBP, 1))
  lag1_df$SBP[is.na(lag1_df$SBP)] <- FALSE
  #lag2_df
  lag2_df <- df %>% mutate(SBP = lead(SBP, 2))
  lag2_df$SBP[is.na(lag2_df$SBP)] <- FALSE
  
  #lag0_seq  
  # Initialize columns to indicate increasing values
  lag0_df$RRI_increase <- c(FALSE, diff(lag0_df$RRI) > 0)
  lag0_df$SBP_increase <- c(FALSE, diff(lag0_df$SBP) > 0)

  # Initialize a list to store sequences
  lag0_sequences <- list()
  i <- 1
  while (i <= nrow(lag0_df) - 2) {
    # Check for a sequence starting at the current index
    if (lag0_df$RRI_increase[i + 1] && lag0_df$RRI_increase[i + 2] && lag0_df$SBP_increase[i + 1] && lag0_df$SBP_increase[i + 2]) {
      sequence_start <- i
      
      # Move forward to find the end of the sequence
      while (i <= nrow(lag0_df) - 1 && lag0_df$RRI_increase[i + 1] && lag0_df$SBP_increase[i + 1]) {
        i <- i + 1
      }
      
      # Adjust the sequence end to exclude the last non-increasing value
      sequence_end <- i
      
      # Store the sequence if it has at least three values
      if (sequence_end - sequence_start + 1 >= 3) {
        sequence <- lag0_df[sequence_start:sequence_end, ]
        lag0_sequences <- append(lag0_sequences, list(sequence))
      }
    }
    
    i <- i + 1
  }
  
  
  #lag1_seq  
  # Initialize columns to indicate increasing values
  lag1_df$RRI_increase <- c(FALSE, diff(lag1_df$RRI) > 0)
  lag1_df$SBP_increase <- c(FALSE, diff(lag1_df$SBP) > 0)
  
  # Initialize a list to store sequences
  lag1_sequences <- list()
  
  i <- 1
  while (i <= nrow(lag1_df) - 2) {
    # Check for a sequence starting at the current index
    if (lag1_df$RRI_increase[i + 1] && lag1_df$RRI_increase[i + 2] && lag1_df$SBP_increase[i + 1] && lag1_df$SBP_increase[i + 2]) {
      sequence_start <- i
      
      # Move forward to find the end of the sequence
      while (i <= nrow(lag1_df) - 1 && lag1_df$RRI_increase[i + 1] && lag1_df$SBP_increase[i + 1]) {
        i <- i + 1
      }
      
      # Adjust the sequence end to exclude the last non-increasing value
      sequence_end <- i
      
      # Store the sequence if it has at least three values
      if (sequence_end - sequence_start + 1 >= 3) {
        sequence <- lag1_df[sequence_start:sequence_end, ]
        lag1_sequences <- append(lag1_sequences, list(sequence))
      }
    }
    
    i <- i + 1
  }
  #lag1_seq  
  # Initialize columns to indicate increasing values
  lag2_df$RRI_increase <- c(FALSE, diff(lag2_df$RRI) > 0)
  lag2_df$SBP_increase <- c(FALSE, diff(lag2_df$SBP) > 0)
  
  # Initialize a list to store sequences
  lag2_sequences <- list()
  
  i <- 1
  while (i <= nrow(lag2_df) - 2) {
    # Check for a sequence starting at the current index
    if (lag2_df$RRI_increase[i + 1] && lag2_df$RRI_increase[i + 2] && lag2_df$SBP_increase[i + 1] && lag2_df$SBP_increase[i + 2]) {
      sequence_start <- i
      
      # Move forward to find the end of the sequence
      while (i <= nrow(lag2_df) - 1 && lag2_df$RRI_increase[i + 1] && lag2_df$SBP_increase[i + 1]) {
        i <- i + 1
      }
      
      # Adjust the sequence end to exclude the last non-increasing value
      sequence_end <- i
      
      # Store the sequence if it has at least three values
      if (sequence_end - sequence_start + 1 >= 3) {
        sequence <- lag2_df[sequence_start:sequence_end, ]
        lag2_sequences <- append(lag2_sequences, list(sequence))
      }
    }
    
    i <- i + 1
  }
  

  ######REGRESSION SLOPES
  ###WORKING
  #lag0
  # Create an empty list to store the linear regression models
  lm0_list <- list()
  
  # Loop through the data frames in df_list
  for (i in 1:length(lag0_sequences)) {
    # Fit a linear regression model to the current data frame
    lm_model <- lm(RRI ~ SBP, data = lag0_sequences[[i]])  #
    
    # Add the linear regression model to the list
    lm0_list[[i]] <- lm_model
  }
  # Create an empty data frame to store the slope coefficients
  lag0_slope_df <- data.frame(index = integer(), slope = numeric(), rsquared= numeric(), stringsAsFactors = FALSE)
  
  # Loop through the linear regression models in lm_list
  for (i in 1:length(lm0_list)) {
    # Extract the slope coefficient from the linear regression model
    slope <- coef(lm0_list[[i]])[2]  # Assuming the slope is the second coefficient
    rsquared <- summary(lm0_list[[i]])$r.squared
    # Create a new row in the slope_df with the index and slope coefficient
    lag0_slope_df <- rbind(lag0_slope_df, data.frame(index = i, slope = slope, rsquared = rsquared, stringsAsFactors = FALSE))
  }
  
  #lag1
  # Create an empty list to store the linear regression models
  lm1_list <- list()
  
  # Loop through the data frames in df_list
  for (i in 1:length(lag1_sequences)) {
    # Fit a linear regression model to the current data frame
    lm_model <- lm(RRI ~ SBP, data = lag1_sequences[[i]])  #
    
    # Add the linear regression model to the list
    lm1_list[[i]] <- lm_model
  }
  # Create an empty data frame to store the slope coefficients
  lag1_slope_df <- data.frame(index = integer(), slope = numeric(), rsquared= numeric(), stringsAsFactors = FALSE)
  
  # Loop through the linear regression models in lm_list
  for (i in 1:length(lm1_list)) {
    # Extract the slope coefficient from the linear regression model
    slope <- coef(lm1_list[[i]])[2]  # Assuming the slope is the second coefficient
    rsquared <- summary(lm1_list[[i]])$r.squared
    # Create a new row in the slope_df with the index and slope coefficient
    lag1_slope_df <- rbind(lag1_slope_df, data.frame(index = i, slope = slope, rsquared = rsquared, stringsAsFactors = FALSE))
  }
  
  #lag2
  # Create an empty list to store the linear regression models
  lm2_list <- list()
  
  # Loop through the data frames in df_list
  for (i in 1:length(lag2_sequences)) {
    # Fit a linear regression model to the current data frame
    lm_model <- lm(RRI ~ SBP, data = lag1_sequences[[i]])  #
    
    # Add the linear regression model to the list
    lm2_list[[i]] <- lm_model
  }
  # Create an empty data frame to store the slope coefficients
  lag2_slope_df <- data.frame(index = integer(), slope = numeric(), rsquared= numeric(), stringsAsFactors = FALSE)
  
  # Loop through the linear regression models in lm_list
  for (i in 1:length(lm2_list)) {
    # Extract the slope coefficient from the linear regression model
    slope <- coef(lm2_list[[i]])[2]  # Assuming the slope is the second coefficient
    rsquared <- summary(lm2_list[[i]])$r.squared
    # Create a new row in the slope_df with the index and slope coefficient
    lag2_slope_df <- rbind(lag2_slope_df, data.frame(index = i, slope = slope, rsquared = rsquared, stringsAsFactors = FALSE))
  }
  #Rsquared filter 0.85
  lag0_final_full <- filter(lag0_slope_df,rsquared>=0.85)
  lag1_final_full <- filter (lag1_slope_df,rsquared>=0.85)
  lag2_final_full <- filter (lag2_slope_df,rsquared>= 0.85)
  #Directionality
  
  #CLEAN
  ###REMOVE OUTLIER SLOPES
  lag0_final_full$outlier <- NA
  IQR <- IQR(lag0_final_full$slope)
  median <- median(lag0_final_full$slope)
  Q1 <- median - 0.5*IQR
  Q3 <- median + 0.5*IQR
  for (i in 1:nrow(lag0_final_full)) {
    if (lag0_final_full$slope[i] > Q3 | lag0_final_full$slope [i]<Q1) {
      # outlier
      lag0_final_full$outlier[i] <- "yes"
    } else {
      lag0_final_full$outlier[i] <- "no"
    }
  }
  
  lag1_final_full$outlier <- NA
  IQR <- IQR(lag1_final_full$slope)
  median <- median(lag1_final_full$slope)
  Q1 <- median - 0.5*IQR
  Q3 <- median + 0.5*IQR
  for (i in 1:nrow(lag1_final_full)) {
    if (lag1_final_full$slope[i] > Q3 | lag1_final_full$slope [i]<Q1) {
      # outlier
      lag1_final_full$outlier[i] <- "yes"
    } else {
      lag1_final_full$outlier[i] <- "no"
    }
  }
  
  lag2_final_full$outlier <- NA
  IQR <- IQR(lag2_final_full$slope)
  median <- median(lag2_final_full$slope)
  Q1 <- median - 0.5*IQR
  Q3 <- median + 0.5*IQR
  for (i in 1:nrow(lag2_final_full)) {
    if (lag2_final_full$slope[i] > Q3 | lag2_final_full$slope [i]<Q1) {
      # outlier
      lag2_final_full$outlier[i] <- "yes"
    } else {
      lag2_final_full$outlier[i] <- "no"
    }
  }
  
  lag0_final_full <- filter(lag0_final_full,outlier=="no")
  lag1_final_full <- filter (lag1_final_full,outlier=="no")
  lag2_final_full <- filter (lag2_final_full,outlier=="no")
  #Summary
  lag0_mean_slope <- mean(lag0_final_full$slope)
  lag0_sd_slope <- sd(lag0_final_full$slope)
  lag0_seq_num <- nrow(lag0_final_full)
  lag0_seq_num_raw <- length(lag0_sequences)
  lag1_mean_slope <- mean(lag1_final_full$slope)
  lag1_sd_slope <- sd(lag1_final_full$slope)
  lag1_seq_num <- nrow(lag1_final_full)
  lag1_seq_num_raw <- length(lag1_sequences)
  lag2_mean_slope <- mean(lag2_final_full$slope)
  lag2_sd_slope <- sd(lag2_final_full$slope)
  lag2_seq_num <- nrow(lag2_final_full)
  lag2_seq_num_raw <- length(lag2_sequences)

  
  lag0_results <- cbind("lag 0",lag0_mean_slope,lag0_sd_slope,lag0_seq_num,lag0_seq_num_raw)
  lag1_results <- cbind("lag 1",lag1_mean_slope,lag1_sd_slope,lag1_seq_num,lag1_seq_num_raw)
  lag2_results <- cbind("lag 2",lag2_mean_slope,lag2_sd_slope,lag2_seq_num,lag2_seq_num_raw)
  df_results <- rbind(lag0_results,lag1_results,lag2_results)
  df_results <- as.data.frame(df_results)
  df_results <- rename(df_results, "mean_slope"="lag0_mean_slope","sd_slope"="lag0_sd_slope","sequence count"="lag0_seq_num","raw sequence count"="lag0_seq_num_raw")
  ###RAW EXPORT
  
  
  # Combine the lag0_list into a single dataframe
  lag0_beats <- do.call(rbind, lag0_sequences)
  lag1_beats <- do.call(rbind,lag1_sequences)
  lag2_beats <- do.call(rbind,lag2_sequences)
  #FILESYSTEM RESULTS
  
  
  if (file.exists(file.path(getwd(),"export",file.id), recursive = TRUE)) {
    
    cat("The folder already exists")
    
  } else {
    
    dir.create(file.path(getwd(),"export",file.id), recursive = T)
  }
  
  if (file.exists(file.path(getwd(),"export",file.id,"plots"), recursive = TRUE)) {
    
    cat("The folder already exists")
    
  } else {
    
    dir.create(file.path(getwd(),"export",file.id,"plots"), recursive = T)
  }
  
  # Export the combined dataframe as a CSV file
  write.csv(lag0_beats, file.path(getwd(),"export",file.id,"lag0_data.csv"), row.names=FALSE)
  write.csv(lag1_beats, file.path(getwd(),"export",file.id,"lag1_data.csv"), row.names=FALSE)
  write.csv(lag2_beats, file.path(getwd(),"export",file.id,"lag2_data.csv"), row.names=FALSE)
  write.csv(lag0_slope_df,file.path(getwd(),"export",file.id,"lag0_slope.csv"), row.names=FALSE)
  write.csv(lag1_slope_df,file.path(getwd(),"export",file.id,"lag1_slope.csv"), row.names=FALSE)
  write.csv(lag2_slope_df,file.path(getwd(),"export",file.id,"lag2_slope.csv"), row.names=FALSE)
  write.csv(df_results,file.path(getwd(),"export",file.id,"BRS results.csv"), row.names=FALSE)
  
  #BRS Sequence plot
  #full file
  create_plot <- function(df) {
    ggplot(df, aes(x = SBP, y = RRI)) +
      geom_point(show.legend = FALSE) +
      geom_smooth(method = "lm", se = FALSE)+
      theme(text = element_text(size = 8),
            axis.title.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),axis.ticks.x=element_blank(),
            legend.position="none")+
      guides(x = guide_axis(angle = 90))
  }
  # Create separate plots for each dataframe in the list
  lag0_plots <- map(lag0_sequences, create_plot)
  
  # Arrange plots in a grid layout
  lag0_plot <- wrap_plots(lag0_plots)
  
  # Display the combined plot
  ggsave(file.path(getwd(),"export",file.id,"plots","lag0_plot.png"),width=10,height=10, dpi=400)
  
  create_plot <- function(df) {
    ggplot(df, aes(x = SBP, y = RRI)) +
      geom_point(show.legend = FALSE) +
      geom_smooth(method = "lm", se = FALSE)+
      theme(text = element_text(size = 8),
            axis.title.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),axis.ticks.x=element_blank(),
            legend.position="none")+
      guides(x = guide_axis(angle = 90))
  }
  # Create separate plots for each dataframe in the list
  lag1_plots <- map(lag1_sequences, create_plot)
  
  # Arrange plots in a grid layout
  lag1_plot <- wrap_plots(lag1_plots)
  
  # Display the combined plot
  ggsave(file.path(getwd(),"export",file.id,"plots","lag1_plot.png"),width=10,height=10, dpi=400)
  
  create_plot <- function(df) {
    ggplot(df, aes(x = SBP, y = RRI)) +
      geom_point(show.legend = FALSE) +
      geom_smooth(method = "lm", se = FALSE)+
      theme(text = element_text(size = 8),
            axis.title.y=element_blank(),axis.ticks.y=element_blank(),
            axis.title.x=element_blank(),axis.ticks.x=element_blank(),
            legend.position="none")+
      guides(x = guide_axis(angle = 90))
  }
  # Create separate plots for each dataframe in the list
  lag2_plots <- map(lag2_sequences, create_plot)
  
  # Arrange plots in a grid layout
  lag2_plot <- wrap_plots(lag2_plots)
  
  # Display the combined plot
  ggsave(file.path(getwd(),"export",file.id,"plots","lag2_plot.png"),width=10,height=10, dpi=400)
  
  
  
  
}
print("BRS Analyzed")
