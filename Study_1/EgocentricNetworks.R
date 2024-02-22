library(readr)
Exporters <- read_csv("Data/InsectsLeadingExporters.csv")
ExpImp <- Exporters[c(1,4)]
Col <- subset(ExpImp, grepl("Colombia", Exporter))
library(igraph)
int <- as.matrix(Col)
net1 <- graph_from_edgelist(int, directed = FALSE)
summary(net1)
plot(net1, 
     displaylabels = TRUE, 
     edge.width=0.5,
     vertex.label.cex=1,
     vertex.shape = "none",
     vertex.color = "white",
     vertex.frame.color = "gray",
     vertex.label.font=1, 
     layout =   layout_as_star)

