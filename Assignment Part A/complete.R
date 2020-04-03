complete <- function(directory, id = 1:332) {
  path <- paste(getwd(), "/", directory, "/", sep = "")
  
  files <- list.files(path)
  #print(files)
  
  # Create new Data Frame
  result <- data.frame()
  #result <- data.frame(id=numeric(0), nobs=numeric(0))

  for (i in id) {
    #Read the file
    file_dir <- paste(path, files[i], sep = "")
    file_data <- read.csv(file_dir)
    
    #Find all the records that have NOT NULL 'sulfate' AND 'nitrate' values
    final_data <- file_data[(!is.na(file_data$sulfate)), ]
    final_data <- final_data[(!is.na(final_data$nitrate)), ]
    nobs <- nrow(final_data)
    
    #Combine the data
    result <- rbind(result, data.frame(id=i, nobs=nobs))
  }

  #print(result)
  return(result)
  
}


source("complete.R")

complete("specdata", 1)
complete("specdata", 30:25)


      