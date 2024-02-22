library(readr)
Exporters <- read_csv("Data/InsectsLeadingExporters.csv")
ExpImp <- Exporters[c(2,7,8)]
Col <- subset(ExpImp, grepl("COL", ExporterCode))
Can <- subset(ExpImp, grepl("CAN", ExporterCode))
Ken <- subset(ExpImp, grepl("KEN", ExporterCode))
Mex <- subset(ExpImp, grepl("MEX", ExporterCode))
Zaf <- subset(ExpImp, grepl("ZAF", ExporterCode))

int <- as.matrix(Col[1:2])
net1 <- graph_from_edgelist(int, directed = FALSE)
V(net1)$Country <- c(Col$ExporterCode[[1]], unique(Col$ImporterCode))
V(net1)$Trade <- sum(Col$TradeValue)
E(net1)$TradeValue <- as.numeric(log(Col$TradeValue))

#V(net1)$Importer <- as.factor(Col$Importer)

# Now plot the graph with the new color for the egocentric node
png("FS1.png", width = 7, height = 7, units = 'in', res = 300)
plot(net1,   
     displaylabels = TRUE,   
     edge.width = E(net1)$TradeValue,
     vertex.label.cex=1,
     vertex.size =  ifelse(V(net1)$Country == "COL", log(sum(Col$TradeValue)), log(Col$TradeValue)),     
     vertex.shape = ifelse(V(net1)$Country == "COL", "square", "none"),
     vertex.color = ifelse(V(net1)$Country == "COL", "red", "white"),
     vertex.frame.color = "lightgray",
     vertex.label.font=2,   
     layout = layout_as_star,
     main = "Exports of edible insects from Colombia")
dev.off()

int2 <- as.matrix(Can[1:2])
net2 <- graph_from_edgelist(int2, directed = FALSE)
V(net2)$Country <- c(Can$ExporterCode[[1]], unique(Can$ImporterCode))
V(net2)$Trade <- sum(Can$TradeValue)
E(net2)$TradeValue <- as.numeric(log(Can$TradeValue))

#V(net1)$Importer <- as.factor(Col$Importer)

# Now plot the graph with the new color for the egocentric node
png("FS2.png", width = 7, height = 7, units = 'in', res = 300)
plot(net2,   
     displaylabels = TRUE,   
     edge.width = E(net2)$TradeValue,
     vertex.label.cex=1,
     vertex.size =  ifelse(V(net2)$Country == "CAN", log(sum(Can$TradeValue)), log(Can$TradeValue)),     
     vertex.shape = ifelse(V(net2)$Country == "CAN", "square", "none"),
     vertex.color = ifelse(V(net2)$Country == "CAN", "red", "white"),
     vertex.frame.color = "lightgray",
     vertex.label.font=2,   
     layout = layout_as_star,
     main = "Exports of edible insects from Canada")
dev.off()


int3 <- as.matrix(Ken[1:2])
net3 <- graph_from_edgelist(int3, directed = FALSE)
V(net3)$Country <- c(Ken$ExporterCode[[1]], unique(Ken$ImporterCode))
V(net3)$Trade <- sum(Ken$TradeValue)
E(net3)$TradeValue <- as.numeric(log(Ken$TradeValue))

#V(net1)$Importer <- as.factor(Col$Importer)

# Now plot the graph with the new color for the egocentric node
png("FS3.png", width = 7, height = 7, units = 'in', res = 300)
plot(net3,   
     displaylabels = TRUE,   
     edge.width = E(net3)$TradeValue,
     vertex.label.cex=1,
     vertex.size =  ifelse(V(net3)$Country == "KEN", log(sum(Ken$TradeValue)), log(Ken$TradeValue)),     
     vertex.shape = ifelse(V(net3)$Country == "KEN", "square", "none"),
     vertex.color = ifelse(V(net3)$Country == "KEN", "red", "white"),
     vertex.frame.color = "lightgray",
     vertex.label.font=2,   
     layout = layout_as_star,
     main = "Exports of edible insects from Kenya")
dev.off()


int4 <- as.matrix(Mex[1:2])
net4 <- graph_from_edgelist(int4, directed = FALSE)
V(net4)$Country <- c(Mex$ExporterCode[[1]], unique(Mex$ImporterCode))
V(net4)$Trade <- sum(Mex$TradeValue)
E(net4)$TradeValue <- as.numeric(log(Mex$TradeValue))

#V(net1)$Importer <- as.factor(Col$Importer)

# Now plot the graph with the new color for the egocentric node
png("FS4.png", width = 7, height = 7, units = 'in', res = 300)
plot(net4,   
     displaylabels = TRUE,   
     edge.width = E(net4)$TradeValue,
     vertex.label.cex=1,
     vertex.size =  ifelse(V(net4)$Country == "MEX", log(sum(Mex$TradeValue)), log(Mex$TradeValue)),     
     vertex.shape = ifelse(V(net4)$Country == "MEX", "square", "none"),
     vertex.color = ifelse(V(net4)$Country == "MEX", "red", "white"),
     vertex.frame.color = "lightgray",
     vertex.label.font=2,   
     layout = layout_as_star,
     main = "Exports of edible insects from Mexico")
dev.off()

int5 <- as.matrix(Zaf[1:2])
net5 <- graph_from_edgelist(int5, directed = FALSE)
V(net5)$Country <- c(Zaf$ExporterCode[[1]], unique(Zaf$ImporterCode))
V(net5)$Trade <- sum(Zaf$TradeValue)
E(net5)$TradeValue <- as.numeric(log(Zaf$TradeValue))

#V(net1)$Importer <- as.factor(Col$Importer)

# Now plot the graph with the new color for the egocentric node
png("FS5.png", width = 7, height = 7, units = 'in', res = 300)
plot(net5,   
     displaylabels = TRUE,   
     edge.width = E(net5)$TradeValue,
     vertex.label.cex=1,
     vertex.size =  ifelse(V(net5)$Country == "ZAF", log(sum(Zaf$TradeValue)), log(Zaf$TradeValue)),     
     vertex.shape = ifelse(V(net5)$Country == "ZAF", "square", "none"),
     vertex.color = ifelse(V(net5)$Country == "ZAF", "red", "white"),
     vertex.frame.color = "lightgray",
     vertex.label.font=2,   
     layout = layout_as_star,
     main = "Exports of edible insects from South Africa")
dev.off()