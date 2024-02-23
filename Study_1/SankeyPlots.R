library(readr)
Exporters <- read_csv("Data/InsectsLeadingExporters.csv")
BestBuyers <- subset(Exporters, grepl("CAN|USA|CRI|DEU|ARE", ImporterCode))
COL <- subset(BestBuyers, grepl("COL", ExporterCode))
CAN <- subset(BestBuyers, grepl("CAN", ExporterCode))
MEX <- subset(BestBuyers, grepl("MEX", ExporterCode))
KEN <- subset(BestBuyers, grepl("KEN", ExporterCode))



eicol <- COL[c(2,9,7,8)]
eican <- CAN[c(2,9,7,8)]
eimex <- MEX[c(2,9,7,8)]
eiken <- KEN[c(2,9,7,8)]

library(tidyverse)
EIcol <- eicol %>% 
  unite(pattern, ExporterCode, ImporterCode, sep = "-")

EIcan <- eican %>% 
  unite(pattern, ExporterCode, ImporterCode, sep = "-")

EImex <- eimex %>% 
  unite(pattern, ExporterCode, ImporterCode, sep = "-")

EIken <- eiken %>% 
  unite(pattern, ExporterCode, ImporterCode, sep = "-")

library(alluvial)
set.seed(48)
num_colors <- unique(EIcol$pattern)
cols <- hsv(h = runif(num_colors), s = runif(num_colors, 0.1, 1), v = runif(num_colors, 0.1, 1))
png("FS01.png", width = 7, height = 7, units = 'in', res = 300)
alluvial_ts(EIcol, wave = .3, ygap = 5, col = cols, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 5, xmargin = 0.2, lab.cex = .7, xlab = '',
            ylab = '', border = NA, axis.cex = .8, leg.cex = .7,
            leg.col='none', 
            title = "Edible Insects Exports from Colombia")
dev.off()

set.seed(63)
num_colors <- unique(EIcan$pattern)
cols <- hsv(h = runif(num_colors), s = runif(num_colors, 0.1, 1), v = runif(num_colors, 0.1, 1))
png("FS02.png", width = 7, height = 7, units = 'in', res = 300)
alluvial_ts(EIcan, wave = .3, ygap = 5, col = cols, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 5, xmargin = 0.2, lab.cex = .7, xlab = '',
            ylab = '', border = NA, axis.cex = .8, leg.cex = .7,
            leg.col='none', 
            title = "Edible Insects Exports from Canada")
dev.off()

set.seed(24)
num_colors <- unique(EImex$pattern)
cols <- hsv(h = runif(num_colors), s = runif(num_colors, 0.1, 1), v = runif(num_colors, 0.1, 1))
png("FS03.png", width = 7, height = 7, units = 'in', res = 300)
alluvial_ts(EImex, wave = .3, ygap = 5, col = cols, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 5, xmargin = 0.2, lab.cex = .7, xlab = '',
            ylab = '', border = NA, axis.cex = .8, leg.cex = .7,
            leg.col='none', 
            title = "Edible Insects Exports from Mexico")
dev.off()

set.seed(28)
num_colors <- unique(EIken$pattern)
cols <- hsv(h = runif(num_colors), s = runif(num_colors, 0.1, 1), v = runif(num_colors, 0.1, 1))
png("FS04.png", width = 7, height = 7, units = 'in', res = 300)
alluvial_ts(EIken, wave = .3, ygap = 5, col = cols, plotdir = 'centred', alpha=.9,
            grid = TRUE, grid.lwd = 5, xmargin = 0.2, lab.cex = .7, xlab = '',
            ylab = '', border = NA, axis.cex = .8, leg.cex = .7,
            leg.col='none', 
            title = "Edible Insects Exports from Kenya")
dev.off()
