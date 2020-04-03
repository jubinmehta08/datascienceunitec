pollutantmean <- function(directory, pollutant, id = 1:332) {
  directory <- paste(getwd(), "/", directory, "/", sep = "")
  #print(directory)
  
  files <- list.files(directory)
  #print(files)
  data <- NA
  
  for (i in id) {
    #Read the file
    file_dir <- paste(directory, files[i], sep = "")
    file_data <- read.csv(file_dir)
    
    #Combine the data
    data <- rbind(data, file_data)
  }
  
  #print(data)
  
  # Calculate the mean
  mean(data[[pollutant]], na.rm = TRUE)
  
}


source("pollutantmean.R")

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 23)

