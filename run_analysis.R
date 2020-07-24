library(dplyr)

# Loads Inidividual Datasets
# Inputs: Path to data structure, name of data structure
# Output: Dataset
load_dataset <- function(path, name) {
  
  # Loads a single file and returns the dataset
  # Input: Filepath, Headers
  # Output: Dataframe of the file
  load_file <- function(x) {
    
    # Gets the number of columns and divides it by 16 since file are 
    # Evenly spaced into 16 character rows
    row_count <- ceiling(nchar(read.csv(x, header=FALSE, nrows = 1)[1,1])/16)

    # Creates a vector of 16s for each row
    row_widths <- rep(16, row_count)
    
    # Imports the data into out
    out <- read.fwf(x, header = FALSE, widths = row_widths)
    
    #return(out)
    return(out)
  }
  
  # Headers  & Activity Labels
  headers <- read.csv('UCI HAR Dataset/features.txt', sep = ' ', header = FALSE)$V2
  activity <- read.csv('UCI HAR Dataset/activity_labels.txt', sep = ' ', header = FALSE)
  colnames(activity) <- c('label', 'label_text')
  
  # List of files to import
  file_list <- c('subject_', 'x_', 'y_')
  
  # List of Headers
  header_list <-c('Subject', headers,'label')
  
  
  # Creates data frame of the 3 key data sources
  for (i in 1:length(file_list)) {
    data <- load_file(paste0(path, '/', (file_list[i]),  name, '.txt'))
    
    if (i == 1) out <- data
    else out <- data.frame(out, data)
  }

  # Renames Columns
  colnames(out) <- header_list
  
  # Joins in Activity
  out <- merge(out, activity, by='label')
  
  # Returns out DF
  return(out)
}

# Pulls the two data sets and merges them
dat_test <- load_dataset('UCI HAR Dataset/test', 'test') 
dat_train <- load_dataset('UCI HAR Dataset/train', 'train')

# Raw data (All Columns)
dat_raw <- dat_test %>% bind_rows(dat_train) 

# Data with just means & standard deviations
dat_mean_and_std <- dat_raw %>% select(Subject, label_text, contains('mean()'), contains('std()')) %>% 
  arrange(Subject, label_text)
# Displays top 5 observations
head(dat_mean_and_std, 5)

# Summarizes (by averageing) each numeric varaible by the Subject
# and the activity
dat_means_and_std_summary <- dat_mean_and_std %>% 
  group_by(Subject, label_text) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  arrange(Subject, label_text)
# Displays top 5 observations
head(dat_means_and_std_summary)
