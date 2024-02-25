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

library(igraph)
bn <- graph_from_data_frame(EIc[c(1,4)], directed = FALSE)
V(bn)$type <- bipartite_mapping(bn)$type
V(bn)$shape <- ifelse(V(bn)$type, "square", "circle")
V(bn)$labelcolor <- ifelse(V(bn)$type == TRUE, "black", "none")
table(EI$Continent)

set.seed(1510)
plot(bn,   
     vertex.color = ifelse(V(bn)$type == FALSE, "green4", c("black", "red", "yellow3", "brown", "blue4")),  
     vertex.size =   5,   
     vertex.shape = V(bn)$shape,
     vertex.label.color = V(bn)$labelcolor,
     edge.color = "lightgray",   
     edge.width =   1,
     layout = layout_as_bipartite(bn))


adjacency_matrix <- get.adjacency(bn, sparse = FALSE)
ncol(adjacency_matrix)
AM <- adjacency_matrix[1:19, (ncol(adjacency_matrix) - 4):ncol(adjacency_matrix)]
am <- data.frame(AM)
am2 <- am[, c(4, 3, 1, 2, 5)]
am3 <- am[, c(5, 2, 1, 3, 4)]

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










library(rgexf)
edge_list <- get.data.frame(bn2, what = "edges")
node_list <- get.data.frame(bn2, what = "vertices")
pave <- write.gexf(node_list, edge_list)
write_gexf(gexf_graph, "bn_bipartite.gexf")
