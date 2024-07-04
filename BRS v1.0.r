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


###TREND DIRECTION
df$RRI_trend <- NA
# Iterate through the values
for (i in 2:nrow(df)) {
  if (df$RRI[i] > df$RRI[i-1]) {
    # Next value is higher
    df$RRI_trend[i] <- "asc"
  } else if (df$RRI[i] < df$RRI[i-1]) {
    # Next value is lower
    df$RRI_trend[i] <- "desc"
  } else {
    # Next value is the same
    df$RRI_trend[i] <- "same"
  }
}

df$SBP_trend <- NA
for (i in 2:nrow(df)) {
  if (df$SBP[i] > df$SBP[i-1]) {
    # Next value is higher
    df$SBP_trend[i] <- "asc"
  } else if (df$SBP[i] < df$SBP[i-1]) {
    # Next value is lower
    df$SBP_trend[i] <- "desc"
  } else {
    # Next value is the same
    df$SBP_trend[i] <- "same"
  }
}

df$RRI_trend <- replace_na(df$RRI_trend,"START_RRI")
df$SBP_trend <- replace_na(df$SBP_trend,"START_SBP")


###SEQ ID
df$RRI_seq <- NA
 for (i in 1:(nrow(df) - 2)) {
    if (df$RRI_trend[i] == df$RRI_trend[i+1] && df$RRI_trend[i] == df$RRI_trend[i+2]) {
      # Found a sequence of three identical categories
      df$RRI_seq[i:(i+2)] <- "seq"
    }
  }
df$RRI_seqdir <- paste(df$RRI_trend,df$RRI_seq)
df$SBP_seq <- NA
for (i in 1:(nrow(df) - 2)) {
  if (df$SBP_trend[i] == df$SBP_trend[i+1] && df$SBP_trend[i] == df$SBP_trend[i+2]) {
    # Found a sequence of three identical categories
    df$SBP_seq[i:(i+2)] <- "seq"
  }
}
df$SBP_seqdir <- paste(df$SBP_trend,df$SBP_seq)
df$RRI_seq <- replace_na(df$RRI_seq,"no")
df$SBP_seq <- replace_na(df$SBP_seq,"no")

# Specify the columns to compare and the criteria column
column_1 <- "SBP_seqdir"
column_2 <- "RRI_seqdir"
column_3 <- "RRI"
check_column <- "SBP_seq"
df$criteria_column <- "seq"
criteria_column <- "criteria_column"
df$RRI_seqlag1 <- c(NA, df[[column_2]][-nrow(df)])
#RRIlag
df$RRI_lag1 <- c(NA, df[[column_3]][-nrow(df)])
column_4 <- "RRI_lag1"
df$RRI_lag2 <- c(NA, df[[column_4]][-nrow(df)])
#RRI_trend lag
column_5<-"RRI_trend"
df$RRI_trend_lag1 <- c(NA, df[[column_5]][-nrow(df)])
column_6 <- "RRI_trend_lag1"
df$RRI_trend_lag2 <- c(NA, df[[column_6]][-nrow(df)])
#RRIseqlag
lag1column <- "RRI_seqlag1"
df$RRI_seqlag2 <- c(NA, df[[lag1column]][-nrow(df)])
lag2column <- "RRI_seqlag2"
# Check if the values in the two columns match the criteria column
#repeat lag 0,1,2
df$lag0_pair <- ifelse(df[[column_1]] == df[[column_2]] & df[[check_column]] == df[[criteria_column]], "Yes", "No")
df$lag1_pair<- ifelse(df[[column_1]] == df[[lag1column]] & df[[check_column]] == df[[criteria_column]], "Yes", "No")
df$lag2_pair<- ifelse(df[[column_1]] == df[[lag2column]] & df[[check_column]] == df[[criteria_column]], "Yes", "No")
df$lag1_pair <- replace_na(df$lag1_pair,"no")
df$lag2_pair <- replace_na(df$lag2_pair,"no")
lag0_seq_count <- nrow(filter(df,lag0_pair == "Yes"))
lag1_seq_count <- nrow(filter(df,lag1_pair == "Yes"))
lag2_seq_count <- nrow(filter(df,lag2_pair == "Yes"))


df$lag0_seq <- NA
for (i in 1:(nrow(df) - 2)) {
  if (df$lag0_pair[i] == df$lag0_pair[i+1] && df$lag0_pair[i] == df$lag0_pair[i+2]&& df$lag0_pair[i]=="Yes") {
    # Found a sequence of three identical categories
    df$lag0_seq[i:(i+2)] <- "seq"
  }
}

df$lag1_seq <- NA
for (i in 1:(nrow(df) - 2)) {
  if (df$lag1_pair[i] == df$lag1_pair[i+1] && df$lag1_pair[i] == df$lag1_pair[i+2]&& df$lag1_pair[i]=="Yes") {
    # Found a sequence of three identical categories
    df$lag1_seq[i:(i+2)] <- "seq"
  }
}
df$lag2_seq <- NA
for (i in 1:(nrow(df) - 2)) {
  if (df$lag2_pair[i] == df$lag2_pair[i+1] && df$lag2_pair[i] == df$lag2_pair[i+2]&& df$lag2_pair[i]=="Yes") {
    # Found a sequence of three identical categories
    df$lag2_seq[i:(i+2)] <- "seq"
  }
}

lag0<-cbind.data.frame(df$index,df$SBP,df$RRI,df$SBP_trend,df$RRI_trend,df$lag0_seq)
lag0<-na.omit(lag0)
lag1<-cbind.data.frame(df$index,df$SBP,df$RRI_lag1,df$SBP_trend,df$RRI_trend_lag1,df$lag1_seq)
lag1<-na.omit(lag1)
lag2<-cbind.data.frame(df$index,df$SBP,df$RRI_lag2,df$SBP_trend,df$RRI_trend_lag2,df$lag2_seq)
lag2<-na.omit(lag2)
#renames
names(lag0)=str_sub(names(lag0),4)
names(lag1)=str_sub(names(lag1),4)
names(lag2)=str_sub(names(lag2),4)
######SEQUENCE ISOLAION


# Get the total number of rows in the data frame
total_rows <- nrow(lag0)

# Initialize variables
prev_row_num <- lag0$index[1]
prev_dir <- lag0$SBP_trend[1]
current_df <- data.frame()
lag0_list <- list()

# Loop through the rows
for (i in 1:total_rows) {
  # Get the current row number
  current_row_num <- lag0$index[i]
  current_dir <- lag0$SBP_trend[i]
  # Check if the difference between the current and previous row numbers is greater than 1
  if (current_row_num - prev_row_num > 1|current_dir!=prev_dir) {
    # If the condition is met, create a new data frame and add it to the list
    lag0_list[[length(lag0_list) + 1]] <- current_df
    
    # Create a new empty data frame for the next iteration
    current_df <- data.frame()
  }
  
  # Add the current row to the current data frame
  current_df <- rbind(current_df, lag0[i, ])
  
  # Update the previous row number for the next iteration
  prev_row_num <- current_row_num
  prev_dir <- current_dir
}

# Add the last data frame to the list
lag0_list[[length(lag0_list) + 1]] <- current_df

#lag1 repeat

# Get the total number of rows in the data frame
total_rows <- nrow(lag1)

# Initialize variables
prev_row_num <- lag1$index[1]
prev_dir <- lag1$SBP_trend[1]
current_df <- data.frame()
lag1_list <- list()

# Loop through the rows
for (i in 1:total_rows) {
  # Get the current row number
  current_row_num <- lag1$index[i]
  current_dir <- lag1$SBP_trend[i]
  # Check if the difference between the current and previous row numbers is greater than 1
  if (current_row_num - prev_row_num > 1|current_dir!=prev_dir) {
    # If the condition is met, create a new data frame and add it to the list
    lag1_list[[length(lag1_list) + 1]] <- current_df
    
    # Create a new empty data frame for the next iteration
    current_df <- data.frame()
  }
  
  # Add the current row to the current data frame
  current_df <- rbind(current_df, lag1[i, ])
  
  # Update the previous row number for the next iteration
  prev_row_num <- current_row_num
  prev_dir <- current_dir
}

# Add the last data frame to the list
lag1_list[[length(lag1_list) + 1]] <- current_df

#lag2 repeat

# Get the total number of rows in the data frame
total_rows <- nrow(lag2)

# Initialize variables
prev_row_num <- lag2$index[1]
prev_dir <- lag2$SBP_trend[1]
current_df <- data.frame()
lag2_list <- list()

# Loop through the rows
for (i in 1:total_rows) {
  # Get the current row number
  current_row_num <- lag2$index[i]
  current_dir <- lag2$SBP_trend[i]
  # Check if the difference between the current and previous row numbers is greater than 1
  if (current_row_num - prev_row_num > 1|current_dir!=prev_dir) {
    # If the condition is met, create a new data frame and add it to the list
    lag2_list[[length(lag2_list) + 1]] <- current_df
    
    # Create a new empty data frame for the next iteration
    current_df <- data.frame()
  }
  
  # Add the current row to the current data frame
  current_df <- rbind(current_df, lag2[i, ])
  
  # Update the previous row number for the next iteration
  prev_row_num <- current_row_num
  prev_dir <- current_dir
}

# Add the last data frame to the list
lag2_list[[length(lag2_list) + 1]] <- current_df

######REGRESSION SLOPES
###WORKING
#lag0
# Create an empty list to store the linear regression models
lm0_list <- list()

# Loop through the data frames in df_list
for (i in 1:length(lag0_list)) {
  # Fit a linear regression model to the current data frame
  lm_model <- lm(RRI ~ SBP, data = lag0_list[[i]])  #
  
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
for (i in 1:length(lag1_list)) {
  # Fit a linear regression model to the current data frame
  lm_model <- lm(RRI_lag1 ~ SBP, data = lag1_list[[i]])  #
  
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
for (i in 1:length(lag2_list)) {
  # Fit a linear regression model to the current data frame
  lm_model <- lm(RRI_lag2 ~ SBP, data = lag2_list[[i]])  #
  
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
lag1_mean_slope <- mean(lag1_final_full$slope)
lag1_sd_slope <- sd(lag1_final_full$slope)
lag1_seq_num <- nrow(lag1_final_full)
lag2_mean_slope <- mean(lag2_final_full$slope)
lag2_sd_slope <- sd(lag2_final_full$slope)
lag2_seq_num <- nrow(lag2_final_full)


lag0_results <- cbind("lag 0",lag0_mean_slope,lag0_sd_slope,lag0_seq_num)
lag1_results <- cbind("lag 1",lag1_mean_slope,lag1_sd_slope,lag1_seq_num)
lag2_results <- cbind("lag 2",lag2_mean_slope,lag2_sd_slope,lag2_seq_num)
 df_results <- rbind(lag0_results,lag1_results,lag2_results)
 df_results <- as.data.frame(df_results)
 df_results <- rename(df_results, "mean_slope"="lag0_mean_slope","sd_slope"="lag0_sd_slope","sequence count"="lag0_seq_num")
###RAW EXPORT


# Combine the lag0_list into a single dataframe
lag0_beats <- do.call(rbind, lag0_list)
lag1_beats <- do.call(rbind,lag1_list)
lag2_beats <- do.call(rbind,lag2_list)
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
  ggplot(df, aes(x = SBP, y = RRI,col=SBP_trend)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE)+
    theme(text = element_text(size = 8),
      axis.title.y=element_blank(),axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),axis.ticks.x=element_blank(),
      legend.position="none")+
    guides(x = guide_axis(angle = 90))
}
# Create separate plots for each dataframe in the list
lag0_plots <- map(lag0_list, create_plot)

# Arrange plots in a grid layout
lag0_plot <- wrap_plots(lag0_plots)

# Display the combined plot
ggsave(file.path(getwd(),"export",file.id,"plots","lag0_plot.png"),width=10,height=10, dpi=400)

create_plot <- function(df) {
  ggplot(df, aes(x = SBP, y = RRI_lag1,col=SBP_trend)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE)+
    theme(text = element_text(size = 8),
          axis.title.y=element_blank(),axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),axis.ticks.x=element_blank(),
          legend.position="none")+
    guides(x = guide_axis(angle = 90))
}
# Create separate plots for each dataframe in the list
lag1_plots <- map(lag1_list, create_plot)

# Arrange plots in a grid layout
lag1_plot <- wrap_plots(lag1_plots)

# Display the combined plot
ggsave(file.path(getwd(),"export",file.id,"plots","lag1_plot.png"),width=10,height=10, dpi=400)

create_plot <- function(df) {
  ggplot(df, aes(x = SBP, y = RRI_lag2,color=SBP_trend)) +
    geom_point(show.legend = FALSE) +
    geom_smooth(method = "lm", se = FALSE)+
    theme(text = element_text(size = 8),
          axis.title.y=element_blank(),axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),axis.ticks.x=element_blank(),
          legend.position="none")+
    guides(x = guide_axis(angle = 90))
}
# Create separate plots for each dataframe in the list
lag2_plots <- map(lag2_list, create_plot)

# Arrange plots in a grid layout
lag2_plot <- wrap_plots(lag2_plots)

# Display the combined plot
ggsave(file.path(getwd(),"export",file.id,"plots","lag2_plot.png"),width=10,height=10, dpi=400)




}
print("BRS Analyzed")

