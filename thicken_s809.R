library(ggplot2)
library(plyr)
library(dplyr)
library(numDeriv)
library(reshape2)
library(ggvis)
library(foreach)

setwd("C:/Users/ndeve/Dropbox/Research/Data/Aifoil_Coordinates/ibl_blade_write")
originalSlice <- read.csv(file="s809_C1_xyz_30percentZero.csv",sep = ",",head = TRUE)
originalCirc <- read.csv(file="circle1_nrelphase6_2.csv",sep = ",",head = TRUE)
originalCircBlend <- read.csv(file="circle1_nrelphase6.csv",sep = ",",head = TRUE)
bladeMap <- read.csv(file="blade_map_withtip.csv",sep = ",",head = TRUE)

oslength = length(originalSlice$y)

addition = c(0.0014,0.0022,0.0022,0.0017,0.0004,0.00009,0.0000003)
subtraction = rev(addition)

originalSlice$y[2:8] = originalSlice$y[2:8]+0.8*addition

originalSlice$y[(oslength-7):(oslength-1)] = originalSlice$y[(oslength-7):(oslength-1)]-0.5*subtraction

plot(originalSlice$x,originalSlice$y,xlim=c(0.5,0.7),ylim=c(-0.04,0.04),asp=1)

write.csv(originalSlice, file="s809_C1_xyz_30percentZero_addedTail.csv", row.names = F,quote = FALSE)
