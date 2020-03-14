if(!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, tidyverse, data.table, dplyr, ggplot2, shiny)

ebola <- read_csv("ebola_2014_2016.csv")
summary(ebola)
str(ebola)
ebola$Date <- as.Date(ebola$Date,format = '%m/%d/%y')
summary(is.na(ebola))

#No. suspected cases - 119 NA
#No. probable cases - 49 NA
#No. confirmed cases - 1 NA
#No. conf, prob, and sus cases - 8 NA
#No. suspected deaths - 1177 NA
#No. probable deaths - 959 NA
#No. confirmed deaths 837 NA


#Changing Names

ebola$`No. of suspected cases` <- ebola$suscases
colnames(ebola)
colnames(ebola) <- c("country", "date", "suscases", "probcases", "confcases","cpscases","susdeaths","probdeaths","confdeaths","cpsdeaths")


