library(readr)
Exporters <- read_csv("Data/InsectsLeadingExporters.csv")
ExpImp <- Exporters[c(1,4)]
Col <- subset(ExpImp, grepl("Colombia", Exporter))
library(igraph)
Exporters <- read_csv("Data/InsectsLeadingExporters.csv")
ExpImp <- Exporters[c(1,4)]
Col <- subset(ExpImp, grepl("Colombia", Exporter))

# Create the graph
int <- as.matrix(Col)
net1 <- graph_from_edgelist(int, directed = FALSE)

V(net1)$Country <- c(Col$Exporter[[1]], unique(Col$Importer))
E(net1)$TradeValue <- as.numeric(log(Exporters$TradeValue[1:13]))
#V(net1)$Importer <- as.factor(Col$Importer)

# Now plot the graph with the new color for the egocentric node
plot(net1,   
     displaylabels = TRUE,   
     edge.width = E(net1)$TradeValue,
     vertex.label.cex=1,
     vertex.shape = ifelse(V(net1)$Country == "Colombia", "square", "none"),
     vertex.color = ifelse(V(net1)$Country == "Colombia", "red", "white"),
     vertex.frame.color = "gray",
     vertex.label.font=1,   
     layout = layout_as_star)











# Plot the graph with edge width based on TradeValue
plot(net1,  
     displaylabels = TRUE,  
     edge.width = E(net1)$TradeValue, # Use TradeValue for edge width
     vertex.label.cex=1,
     vertex.shape = "none",
     vertex.color = "white",
     vertex.frame.color = "gray",
     vertex.label.font=1,  
     layout = layout_as_star)
