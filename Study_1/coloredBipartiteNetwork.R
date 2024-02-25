load("Data/EdibleInsectsList.RData")
colnames(EI)[7] <- "Country"
colnames(EI)[5] <- "Name"
colnames(EI)[9] <- "Stage"
variable.names(EI)
CommonNames <- data.frame(table(EI$Name))
EI <- subset(EI, !grepl("bee|bees|spider", Name))
EI <- subset(EI, !grepl("Apidae", Family))



EI <- EI %>% mutate(., CommonName = ifelse(grepl("ant", Name), "Ant",
                                    ifelse(grepl("beetle| beetle|beetles|weevil", Name), "Beetle",
                                    ifelse(grepl("bug|bugs|Bug|Bugs|scorpion", Name), "Bugs",
                                    ifelse(grepl("cicada|Cicada", Name), "Cicada", 
                                    ifelse(grepl("cricket", Name), "Cricket", 
                                    ifelse(grepl("flies|fly|midges", Name), "Flies", 
                                    ifelse(grepl("worm|grub|Grub", Name), "Worms", 
                                    ifelse(grepl("locust|Locust|grasshopper", Name), "Locust",
                                    ifelse(grepl("moth", Name), "Moths",
                                    ifelse(grepl("slug", Name), "Slugs",
                                    ifelse(grepl("larva", Stage), "Larvae",
                                    ifelse(grepl("Scarabaeidae", Family), "Beetle",
                                    ifelse(grepl("Acrididae|Pyrgomorphidae|Romaleidae", Family), "Locust",
                                    ifelse(grepl("Vespidae", Family), "Wasps",
                                    ifelse(grepl("Saturniidae|Hepialidae|Sphingidae|Noctuidae|Cossidae|Notodontidae|Lasiocampidae|Pyralidae", Family), "Moths",
                                    ifelse(grepl("Cerambycidae|Dytiscidae|Curculionidae|Lucanidae|Hydrophilidae|Tenebrionidae|Curculionidae|Dryophthoridae|Chrysomelidae", Family), "Beetle",
                                    ifelse(grepl("Formicidae", Family), "Ant",
                                    ifelse(grepl("Gryllidae|Tettigoniidae|Gryllotalpidae", Family), "Cricket",
                                    ifelse(grepl("Termitidae", Family), "Termite",
                                    ifelse(grepl("Pentatomidae|Passalidae|Belostomatidae|Coreidae|Tessaratomidae", Family), "Bugs",
                                    ifelse(grepl("Libellulidae|Aeschnidae", Family), "Flies",
                                    ifelse(grepl("Nymphalidae|Pieridae|Hesperiidae|Lycaenidae|Papilionidae", Family), "Butterflies",
                                    ifelse(grepl("Blattidae", Family), "Cockroach",
                                    ifelse(grepl("termite", Name), "Termite", "Other")))))))))))))))))))))))))

table(EI$CommonName)
EI <- subset(EI, !grepl("Slugs|Other", CommonName))

EIp <- EI[c(10,7)]

library(countrycode)

EIp$country_code <- countrycode(EIp$Country, "country.name", "iso2c")
EIp <- na.omit(EIp)
EIp$Continent <- countrycode(EIp$country_code, "iso2c", "continent")



library(dplyr)
library(tidyr)
library(tidyverse)
EIc <- EIp %>%
  mutate(country = str_extract(Country, "\\b\\w+\\b")) %>% na.omit()
table(EIc$CommonName)

EIn <- subset(EIc, grepl("Africa|Americas|Asia", Continent))
EIn <- EIn[c(3,4)]

EIna <- EIn[!duplicated(EIn), ]

table(EIna$country_code) == table(EIn$country_code)

library(igraph)
bn <- graph_from_data_frame(EIna, directed = FALSE)
V(bn)$type <- bipartite_mapping(bn)$type
V(bn)$labelcolor <- ifelse(V(bn)$type == FALSE, "black", c("chocolate4", "red",  "gold2"))
V(bn)$betweenness <- igraph::betweenness(bn)
table(EI$Continent)

# Figure2A (Published) ----
set.seed(1510)
layout <- layout_components(bn)
rotated_layout <- cbind(-layout[, 2], layout[, 1])
png("FS1B.png", width = 6, height = 6, units = 'in', res = 300)
plot(bn,   
     vertex.color = ifelse(V(bn)$type == FALSE, "black", c("chocolate4", "red",  "gold2")),  
     vertex.size =   25,   
     vertex.shape = ifelse(V(bn)$type == FALSE, "none", "circle"),
     vertex.label.color = ifelse(V(bn)$type == FALSE, "black", "black"),
     vertex.label.size = V(bn)$betweenness,
     edge.color = "black",   
     edge.width =   1,
     layout = layout)
dev.off()
# Veamos------
Degree <- igraph::degree(bn)
Betweenness  <-  igraph::betweenness(bn)
Closeness <-  igraph::closeness(bn)
Eigen <-  igraph::eigen_centrality(bn)$vector


Centralities <- head(data.frame(Degree, Betweenness, Closeness, Eigen), 14)


adjacency_matrix <- get.adjacency(bn, sparse = FALSE)
ncol(adjacency_matrix)
AM <- adjacency_matrix[1:19, (ncol(adjacency_matrix) - 4):ncol(adjacency_matrix)]
am <- data.frame(AM)
am2 <- am[, c(4, 3, 1, 2, 5)]
am3 <- am[, c(5, 2, 1, 3, 4)]

# Figure 2A (Published Figure) ----

library(bipartite)
png("FS1A.png", width = 25, height = 10, units = 'in', res = 300)
plotweb(am3, method = "normal", 
        ybig = 1, 
        col.high = c("#7c0cc7",  "blue3", "chocolate4", "red",  "gold2"), 
        bor.col.high = c("#7c0cc7",  "blue3", "chocolate4", "red",  "gold2"),
        col.low = "green4", 
        bor.col.low = "green4",
        col.interaction = "grey85",
        bor.col.interaction = "grey85",
        #low.lablength = 10,
        text.rot = 90,
        labsize = 2.5,
        low.plot = TRUE, 
        text.high.col="black",
        text.low.col = "black",
        low.spacing = 0.005,
        high.spacing = 0.005,
        y.width.low=0.1, y.width.high=0.1, 
        y.lim=c(0,2), arrow="none", adj.high=c(0.5,0.5),
        high.lablength=11,high.lab.dis=0.25,
        low.lab.dis = 0.15)
dev.off()


save.image("Study_1/Study1A.RData")







library(rgexf)
edge_list <- get.data.frame(bn2, what = "edges")
node_list <- get.data.frame(bn2, what = "vertices")
pave <- write.gexf(node_list, edge_list)
write_gexf(gexf_graph, "bn_bipartite.gexf")
