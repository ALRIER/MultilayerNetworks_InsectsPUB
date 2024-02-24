library(readr)
paisesinsectos <- read_csv("Data/paisesinsectos.csv")
# Remove brackets and single quotes from the "Country" column
paisesinsectos$Country <- gsub("\\[|\\]|'", "", paisesinsectos$Country)

library(tidyverse)
# Split the "Country" column into multiple rows
paisesinsectos <- separate_rows(paisesinsectos, Country, sep = ",")
paisesinsectos$Country <- trimws(paisesinsectos$Country)
paisesinsectos <- subset(paisesinsectos, nchar(Country) >  0)
unique_countries <- unique(paisesinsectos$Country)


library(igraph)
bn <- graph_from_data_frame(paisesinsectos, directed = FALSE)
V(bn)$type <- bipartite_mapping(bn)$type
country_colors <- rainbow(length(unique(paisesinsectos$Country)))

#png("F1.png", width = 10, height = 10, units = 'in', res = 300)
set.seed(1510)
plot(bn,   
     vertex.color = country_colors,   
     vertex.size =   2,   
     vertex.label = NA,   
     edge.color = "lightgray",   
     edge.width =   1,
     layout = layout_components(bn))
#dev.off()




FamilyCountry <- paisesinsectos[c(2,3)]
library(countrycode)

FamilyCountry$country_code <- countrycode(FamilyCountry$Country, "country.name", "iso2c")

# Convert country codes to continent names
FamilyCountry$Continent <- countrycode(FamilyCountry$country_code, "iso2c", "continent")

FamilyContinent <- FamilyCountry[c(1,4)]
bn2 <- graph_from_data_frame(FamilyContinent, directed = FALSE)
V(bn2)$type <- bipartite_mapping(bn2)$type
V(bn2)$shape <- ifelse(V(bn2)$type, "square", "circle")
table(FamilyContinent$Continent)

set.seed(1510)
png("FS1A.png", width = 10, height = 10, units = 'in', res = 300)
plot(bn2,   
     vertex.color = ifelse(V(bn2)$type == FALSE, "green4", c("yellow3", "brown", "blue4")),  
     vertex.size =   7,   
     vertex.shape = V(bn2)$shape,
     vertex.label = NA,   
     edge.color = "lightgray",   
     edge.width =   1,
     layout = layout_components(bn2))
dev.off()

library(rgexf)
edge_list <- get.data.frame(bn2, what = "edges")
node_list <- get.data.frame(bn2, what = "vertices")
pave <- write.gexf(node_list, edge_list)
write_gexf(gexf_graph, "bn_bipartite.gexf")
