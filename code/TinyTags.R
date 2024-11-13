library(readxl)
library(tidyverse)


setwd("~/DataAnalysis")

TTtemp <- read.csv("./Data/combined_temperature_data_august2024.csv")

str(TTtemp)


ggplot(TTtemp, aes(x=as.Date(Timestamp)  , y=Temperature,color=as.character(SerialNumber)))+
  geom_point()

broken <- subset(TTtemp, Temperature<0)

unique(TTtemp[,c(1:3,8)])

unique(broken[,c(1:3,8)])
