corr <- function(directory, threshold = 0) {
  # Get full path of the specsdata folder
  directory <- paste(getwd(),"/",directory,"/",sep="")
  
  #Get observations and filter by threshold
  observations <- complete(directory)
  filtered_observations = subset(observations, observations$nobs > threshold)
  
  # List all the files
  file_list <- list.files(directory)
  correlation <- vector()
  
  # For each id in filtered observations:
  for (i in filtered_observations$id) {
    # Read the file
    file_dir <- paste(directory,file_list[i],sep="")
    file_data <- read.csv(file_dir)
    
    # remove NA
    file_data <- subset(file_data,complete.cases(file_data))
    
    # and calculate the cor and accumulate it in the corellation vector.
    correlation <- c(correlation, cor(file_data$nitrate,file_data$sulfate))    
  }
  #Finally, return the vector
  return(correlation)
}

source("complete.R")
source("corr.R")

cr <- corr("specdata", 150)

#Test scenarios
print(cr)

length(cr)
summary(cr)
head(cr)
