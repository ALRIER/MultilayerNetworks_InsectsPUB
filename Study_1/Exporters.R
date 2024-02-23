library(readr)
setwd("Study_1/DataExporters/")
mydir = getwd()
myfiles = list.files(path=mydir, pattern="*.csv", full.names=FALSE)
myfiles

# Loop through each file
for (file in myfiles) {
  # Extract the filename without the extension
  filename <- str_remove(file, ".csv$")
  
  # Read the CSV file
  data <- read_csv(file)
  
  # Assign the data to an object named after the file
  assign(x = filename, value = data, envir = .GlobalEnv)
}



Top10Exporters <- list(BEL, CAN, COL, ECU, ISR, KEN, MAR, MEX, NDL, USA)
Exporters <- do.call("rbind", Top10Exporters)
rm(list=setdiff(ls(), "Exporters"))
