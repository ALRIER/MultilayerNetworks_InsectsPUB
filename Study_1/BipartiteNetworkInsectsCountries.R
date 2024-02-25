library(readr)
EdibleSpecies <- read_delim("Study_1/EdibleSpecies.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

load("Study_1/Study1A.RData")
Edibles <- EdibleSpecies %>% mutate(., Importance = ifelse(grepl("Moth", CommonName), Centralities$Eigen[10],
                                                    ifelse(grepl("Larvae", CommonName), Centralities$Eigen[1],
                                                    ifelse(grepl("Locust", CommonName), Centralities$Eigen[11],
                                                    ifelse(grepl("Beetle", CommonName), Centralities$Eigen[2],
                                                    ifelse(grepl("Cricket", CommonName), Centralities$Eigen[12],
                                                    ifelse(grepl("Ants", CommonName), Centralities$Eigen[4],
                                                    ifelse(grepl("Termites", CommonName), Centralities$Eigen[8],
                                                    ifelse(grepl("Flies", CommonName), Centralities$Eigen[5],
                                                                  median(Centralities$Eigen))))))))))

library(ggplot2)
ggplot(Edibles, aes(x=Energy, y=Protein, color=CommonName)) +
  geom_point(aes(size = Importance), alpha = 0.7) + theme_minimal()




variable.names(EdibleSpecies)
Country <- unique(EdibleSpecies$Country)
Edibles <- EdibleSpecies[c(5,2,4)]

library(igraph)
bn <- graph_from_data_frame(Edibles, directed = FALSE)
summary(bn)
V(bn)$name
E(bn)$Protein
V(bn)$type <- bipartite_mapping(bn)$type
V(bn)$size <- sd(E(bn)$Protein)
V(bn)$shape <- ifelse(V(bn)$type, "none", "none") 
V(bn)$labelcolor <- ifelse(V(bn)$type, "brown4", "#046A38")
E(bn)$color <- "gray"
E(bn)$width <- E(bn)$Protein/20
V(bn)$label.cex = igraph::betweenness(bn)


layout <- layout_nicely(bn)
rotated_layout <- cbind(layout[, 2], -layout[, 1])


png("FS1B.png", width = 7, height = 7, units = 'in', res = 300)
plot(bn, vertex.label = V(bn)$name, 
     layout = rotated_layout, 
     main = "",
     vertex.size = V(bn)$size,
     vertex.label.color = V(bn)$labelcolor, 
     edge.lty = E(bn)$linetype, 
     vertex.shape = V(bn)$shape,
     edge.size = E(bn)$width
dev.off()
