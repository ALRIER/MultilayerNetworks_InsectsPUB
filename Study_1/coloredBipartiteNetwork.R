load("Data/EdibleInsectsList.RData")
colnames(EI)[7] <- "Country"
variable.names(EI)

EI <- EI[c(3,7)]

library(dplyr)
library(tidyr)
EI <- EI %>%
  mutate(Country = str_extract(Country, "\\b\\w+\\b")) %>%
  na.omit()

library(countrycode)

EI$country_code <- countrycode(EI$Country, "country.name", "iso2c")
EI <- na.omit(EI)
EI$Continent <- countrycode(EI$country_code, "iso2c", "continent")


library(igraph)
bn <- graph_from_data_frame(EI[c(1,4)], directed = FALSE)
pave <- as.matrix(bn)
V(bn)$type <- bipartite_mapping(bn)$type
V(bn)$shape <- ifelse(V(bn)$type, "square", "circle")
V(bn)$labelcolor <- ifelse(V(bn)$type == TRUE, "black", "none")
table(EI$Continent)

set.seed(1510)
png("FS1A.png", width = 10, height = 10, units = 'in', res = 300)
plot(bn,   
     vertex.color = ifelse(V(bn)$type == FALSE, "green4", c("black", "red", "yellow3", "brown", "blue4")),  
     vertex.size =   5,   
     vertex.shape = V(bn)$shape,
     vertex.label.color = V(bn)$labelcolor,
     edge.color = "lightgray",   
     edge.width =   1,
     layout = layout_with_fr(bn))
dev.off()

library(rgexf)
edge_list <- get.data.frame(bn2, what = "edges")
node_list <- get.data.frame(bn2, what = "vertices")
pave <- write.gexf(node_list, edge_list)
write_gexf(gexf_graph, "bn_bipartite.gexf")
