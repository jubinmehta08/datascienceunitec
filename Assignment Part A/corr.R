corr <- function(directory, threshold = 0) {
  path <- paste(getwd(), "/", directory, "/", sep = "")
  
  files <- list.files(path)
  #print(files)
  
  #create empty vector
  num_vector <- vector(mode = "numeric", length = 0)
  #correlation <- vector()
  
  
  for (i in 1:length(files)) {
    #Read the file
    file_dir <- paste(path, files[i], sep = "")
    file_data <- read.csv(file_dir)
    
    #Calculate total non-null values    
    cal_sum <- sum((!is.na(file_data$sulfate)) & (!is.na(file_data$nitrate)))
    #print(cal_sum)
    
    if (cal_sum > threshold)
    {
      #Extract data of niteate and sulfate and calculate correlation between them
      sul <- file_data[which(!is.na(file_data$sulfate)), ]
      nit <- sul[which(!is.na(sul$nitrate)), ]
      num_vector <- c(num_vector, cor(nit$sulfate, nit$nitrate))
    }
    
  }
  
  num_vector
  
}

source("corr.R")
source("complete.R")

cr <- corr("specdata", 150)

#Test scenarios
print(cr)

length(cr)
summary(cr)
head(cr)
