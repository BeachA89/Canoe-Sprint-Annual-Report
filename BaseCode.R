
library(data.table)
library(dplyr)
library(DT)
library(tidyverse)
library(lubridate) #for ms
library(hms)
library(scales)
library(plotly)
library(htmlwidgets)
library(rdrop2)


token <- drop_auth()
# Yo, don't share your token!
saveRDS(token, "droptoken.rds")
dbfolder <- "shinyshop"
drop_create(dbfolder)


token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
db_folder <- "shinyshop"

save_db <- function(dat) {
  dat <- Athlete
  file_path <- file.path(tempdir(), "data.csv") # create temporary file
  write.csv(dat, file_path, row.names = FALSE)
  drop_upload(file_path, dest = db_folder)
}

load_db <- function(x) {
  dd <- drop_read_csv(file.path(db_folder,x))
  return(dd)
}


load_db("Athlete.csv")


load_db2 <- function() {
  dd <- drop_read_csv(file.path(db_folder,"Benchmark.csv"))
  return(dd)
}

load_db()
load_db2()


#### Code ####
  
Book1 <- fread("data/Book1.csv", colClasses = c(Place = "numeric", Year = "numeric"))

Benchmark <-  fread("data/Benchmark.csv", colClasses = c(Placing = "numeric", Year = "numeric", Time = "character"))
#'Time dc' = "character"

names(Book1)[9] <- "Time"


Athlete <-  Book1 %>% dplyr::filter(Event == "MK1 1000m") %>% dplyr::filter(Name == "Murray Stewart") %>% dplyr::group_by(Year) %>% dplyr::filter(Time_sec == min(Time_sec))


Benchmark_Nationals_Medal <-  Benchmark %>% dplyr::filter(Event == "MK1 1000m") %>% dplyr::filter(Competition == "Nationals") %>% dplyr::group_by(Year) %>% dplyr::filter(Placing == 3)
Benchmark_Nationals_Top8 <-  Benchmark %>% dplyr::filter(Event == "MK1 1000m") %>% dplyr::filter(Competition == "Nationals") %>% dplyr::group_by(Year) %>% dplyr::filter(Placing == 8)
Benchmark_Worlds_Medal <-  Benchmark %>% dplyr::filter(Event == "MK1 1000m") %>% dplyr::filter(Competition == "World Championships") %>% dplyr::group_by(Year) %>% dplyr::filter(Placing == 3)
Benchmark_Worlds_Top8 <-  Benchmark %>% dplyr::filter(Event == "MK1 1000m") %>% dplyr::filter(Competition == "World Championships") %>% dplyr::group_by(Year) %>% dplyr::filter(Placing == 8)

#Benchmark_Nationals_Medal$Time <- as.POSIXct(Benchmark_Nationals_Medal$Time, format = "%M:%OS")

Benchmark_Nationals_Medal$Competition[Benchmark_Nationals_Medal$Competition %in% "Nationals"] <- "Nationals Medal"
Benchmark_Nationals_Top8$Competition[Benchmark_Nationals_Top8$Competition %in% "Nationals"] <- "Nationals Top 8"
Benchmark_Worlds_Medal$Competition[Benchmark_Worlds_Medal$Competition %in% "World Championships"] <- "Worlds Medal"
Benchmark_Worlds_Top8$Competition[Benchmark_Worlds_Top8$Competition %in% "World Championships"] <- "Worlds Top 8"




Benchmark_Nationals_Medal <-  Benchmark_Nationals_Medal[order(Benchmark_Nationals_Medal$Year),]
Benchmark_Nationals_Top8 <-  Benchmark_Nationals_Top8[order(Benchmark_Nationals_Top8$Year),]
Benchmark_Worlds_Medal <-  Benchmark_Worlds_Medal[order(Benchmark_Worlds_Medal$Year),]
Benchmark_Worlds_Top8 <-  Benchmark_Worlds_Top8[order(Benchmark_Worlds_Top8$Year),]
Athlete <-  Athlete[order(Athlete$Year),]

names(Benchmark_Nationals_Medal)[names(Benchmark_Nationals_Medal) == "Competition"] <-  "Name"
names(Benchmark_Nationals_Top8)[names(Benchmark_Nationals_Top8) == "Competition"] <-  "Name"
names(Benchmark_Worlds_Medal)[names(Benchmark_Worlds_Medal) == "Competition"] <-  "Name"
names(Benchmark_Worlds_Top8)[names(Benchmark_Worlds_Top8) == "Competition"] <-  "Name"


Annual_plot_data <-  bind_rows(Athlete, Benchmark_Nationals_Medal,Benchmark_Nationals_Top8, Benchmark_Worlds_Medal,Benchmark_Worlds_Top8)




Annual_plot_data$Time <- as.POSIXct(Annual_plot_data$Time, format='%M:%OS')


################

Annualplot  <-  ggplot(data=Annual_plot_data, aes(x=Year, y=Time, group=Name, colour=Name, 
                                                  text = paste('Year: ', Year, '\n',
                                                               'Time: ', format(Time, "%M:%S.%OS"), '\n',
                                                               'Name: ', Name, '\n'))) + geom_line(size=1) + scale_y_datetime(date_labels="%M:%S.%OS")

ggplotly(Annualplot, tooltip = 'text') %>% layout(hovermode = 'closest')

typeof(Annual_plot_data$Time)
