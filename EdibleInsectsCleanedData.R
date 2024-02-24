library(readxl)
EI1 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 1")
EI2 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 2")
EI3 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 3")
EI4 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 4")
EI5 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 5")
EI6 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 6")
EI7 <- read_excel("Data/EdibleInsects.xlsx", sheet = "Table 7")

EI <- list(EI1, EI2, EI3, EI4, EI5, EI6, EI7)
EI <- do.call("rbind", EI)
rm(list=setdiff(ls(), "EI"))


EI <- subset(EI, EI$Genus!='Genus')
save.image("~/Documents/GitHub/MultilayerNetworks_InsectsPUB/Data/EdibleInsectsList.RData")