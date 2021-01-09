library(shiny)
library(shinydashboard)
library(shinythemes)
library(quantmod)
library(zoo)
library(R.utils)
library(tableHTML)
library(svDialogs)
library(stringr)
library(plyr)
library(reshape2)
library(formattable)
library(matrixStats)
library(broom)
library(tibble)
library(Rmisc)
library(DT)
library(data.table)
library(dplyr)
library(DT)
library(tidyverse)
library(lubridate) #for ms
library(hms)
library(scales)
library(plotly)
library(htmlwidgets)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rdrop2)
library(RColorBrewer)

token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
db_folder <- "shinyshop"

save_db <- function(dat) {
  if(exists("mydata")) dat <- rbind(mydata$x, dat)
  file_path <- file.path(tempdir(), "data.csv") # create temporary file
  write.csv(dat, file_path, row.names = FALSE)
  drop_upload(file_path, dest = db_folder)
}

load_db <- function(x) {
  dd <- drop_read_csv(file.path(db_folder,x), header=TRUE, stringsAsFactors=FALSE,dtoken=token)
  return(dd)
}

#Athlete <-  isolate(fread(input$Athletes$datapath,  header=TRUE, stringsAsFactors=FALSE))

Prognostic.Velocities <- data.frame(fread("Prognostic Velocities.csv", header = TRUE, stringsAsFactors=FALSE))
