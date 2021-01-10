server <- function(input, output) {
  v <- reactiveValues(value=NULL)
  #observeEvent(input$goButton, {
  #### Read data into a list ####
  Athlete <-  load_db("Athlete.csv")
  Benchmark <-load_db("Benchmark.csv")
  
  
  
  #Benchmark <-  isolate(fread(input$Benchmark$datapath,  header=TRUE, stringsAsFactors=FALSE))
  #Athlete$Time <- as.POSIXct(Athlete$Time, format='%M:%OS')
  #Benchmark$Time <- as.POSIXct(Benchmark$Time, format='%M:%OS')
  
  names(Athlete)[10] <- "Time"
  
  
  Benchmark_Nationals <-  Benchmark %>% dplyr::filter(Competition == "Nationals")
  Benchmark_Worlds <-  Benchmark %>%  dplyr::filter(Competition == "World Championships")
  
  
  names(Benchmark_Nationals)[names(Benchmark_Nationals) == "Competition"] <-  "Name"
  names(Benchmark_Worlds)[names(Benchmark_Worlds) == "Competition"] <-  "Name"
  
  
  #    Athlete_data <-  reactiveValues()
  
  #################FILTER DATA WBT#############################################
  
  
  tab_WBT <-  reactive({
    Prognostic.Velocities %>% filter(BoatClass==input$Event) %>% select(WBT, WBTsec,WBTSpeed)
    
  })
  tab_Prog <-  reactive({
    a <- Prognostic.Velocities %>% filter(BoatClass==input$Event) %>% select(ProgTime100, ProgTimesec100, ProgSpeed100, 
                                                                             ProgTime97, ProgTimesec97, ProgSpeed97, 
                                                                             ProgTime95, ProgTimesec95, ProgSpeed95, 
                                                                             ProgTime93, ProgTimesec93, ProgSpeed93)
    Prog100 = a[,1:3]
    Prog97 <-  a[,4:6]
    Prog95 <- a[,7:9]
    Prog93 <- a[,10:12]
    
    colnames(Prog100)=colnames(Prog97)=colnames(Prog95)=colnames(Prog93) = c("Prognostic Time", "in seconds", "Avg Speed (km/h)")
    b <- rbind(Prog100, Prog97, Prog95, Prog93)
    
    return(b)
    
  })
  
  
  
  output$table_WBT <- renderDataTable({ 
    
    WBTdata <-  tab_WBT()
    datatable(WBTdata,  rownames = NULL, colnames = c("World's Best Time", "in seconds", "Avg Speed (km/h)"), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', ordering=F))
  })
  output$table_Prog <- renderDataTable({ 
    
    Progdata <-  tab_Prog()
    datatable(Progdata,  rownames = c("100%", "97% (Open)", "95% (U23)", "93% (U18)"), colnames = c("Prognostic Time", "in seconds", "Avg Speed (km/h)"), options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', ordering=F))
  })
  
  #################FILTER DATA ATHLETE 1#############################################
  
  
  tab_Athlete <-  reactive({
    
    ClassProgTimeSec <-  Prognostic.Velocities %>% filter(BoatClass==input$Event)%>% select(ProgTimesec100)
    ClassProgTimeSec <- ClassProgTimeSec[1,1]
    
    # ClassProgTimeSec <- ClassProgTimeSec[1,'ProgTimesec']
    # WBTsec <-  Prognostic.Velocities %>% filter(BoatClass=="MK1 500m")
    # WBTsec <- WBTsec[1,'WBTsec']
    
    Athlete %>%
      filter(Event == input$Event) %>% 
      filter(Name == input$Name) %>% 
      dplyr::group_by(Year) %>% dplyr::filter(Time_sec == min(Time_sec)) %>%
      dplyr::mutate("% of Prog" := round(((ClassProgTimeSec/Time_sec)*100),digits = 2))
    
  })  
  
  
  # testa <- Athlete%>%
  #   filter(Event == "MK1 500m") %>% 
  #   filter(Name == "Murray Stewart") %>% 
  #   filter(Age == "Open") %>% 
  #   dplyr::group_by(Year) %>% dplyr::filter(Time_sec == min(Time_sec)) %>%
  # mutate(OffWBT = (((Time_sec-WBTsec)/Time_sec)*100))
  
  
  
  tab_AthletePlot <-  reactive({
    
    Round_order <- c("Final", "A Final", "B Final", "C Final", "Semi-Final")
    Athlete%>%
      filter(Event == input$Event) %>% 
      filter(Name == input$Name) %>% 
      filter(Comp == "Nationals") %>% 
      dplyr::group_by(Year) %>% 
      arrange(factor(Round, levels = Round_order), .by_group = TRUE) %>%
      slice(1)
  })  
  
  tab_AthleteTop10 <-  reactive({
    ClassProgTimeSec <-  Prognostic.Velocities %>% filter(BoatClass==input$Event)%>% select(ProgTimesec100)
    ClassProgTimeSec <- ClassProgTimeSec[1,1]
    
    Athlete%>%
      filter(Event == input$Event) %>%
      filter(Name == input$Name) %>%
      dplyr::mutate("% of Prog" := round(((ClassProgTimeSec/Time_sec)*100),digits = 2)) %>%
      arrange(Time) %>%
      head(10)
    
    
  })
  
  tab_AthleteLast10 <-  reactive({
    ClassProgTimeSec <-  Prognostic.Velocities %>% filter(BoatClass==input$Event)%>% select(ProgTimesec100)
    ClassProgTimeSec <- ClassProgTimeSec[1,1]
    
    Athlete%>%
      filter(Event == input$Event) %>%
      filter(Name == input$Name) %>%
      dplyr::mutate("% of Prog" := round(((ClassProgTimeSec/Time_sec)*100),digits = 2)) %>%
      arrange(Year) %>%
      head(10)
    
    
  })
  
  
  #     
  
  output$select_Event <-  renderUI({
    
    selectizeInput('Event', 'Select Event', choices = c("select" = "", unique(Athlete$Event)))  
  }) 
  
  output$select_Name <-  renderUI({
    inputEvent = as.character(input$Event)
    choice_Name <- reactive({
      Athlete%>% 
        filter(Event == inputEvent) %>% 
        pull(Name) %>% 
        as.character()
      
      
    })
    
    selectizeInput('Name', 'Select Name', choices = c("select" = "", choice_Name()))
    
  })
  
  
  #################OUTPUT TABLES FOR ATHLETE 1#############################################
  
  
  
  
  # output$table_Athlete <- renderDataTable({ 
  #   
  #   Athletedata <-  tab_Athlete() %>% select(Name, Year, Comp, Age, Location, Place, Round, Time, Partner, `% of Prog`)
  #   ({datatable(Athletedata,  rownames = NULL, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', order = list(list(1, 'asc')))) %>% formatStyle(
  #     columns = c(10),
  #     #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
  #     valueColumns = 0,
  #     target = 'cell',
  #     backgroundColor = 'gold',
  #     fontWeight = 'bold')
  #   
  #   })
  # })
  
  output$table_AthleteTop10 <- renderDataTable({
    
    AthletedataTop10 <-  tab_AthleteTop10() %>% select(Name, Year, Comp, Age, Location, Place, Round, Time, Partner, `% of Prog`)
    ({datatable(AthletedataTop10,  rownames = NULL, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', order = list(list(7, 'asc')))) %>% formatStyle(
      columns = c(10),
      #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
      valueColumns = 0,
      target = 'cell',
      backgroundColor = 'gold',
      fontWeight = 'bold')
    })
  })
  
  output$table_AthleteLast10 <- renderDataTable({
    
    AthletedataLastp10 <-  tab_AthleteLast10() %>% select(Name, Year, Comp, Age, Location, Place, Round, Time, Partner,  `% of Prog`)
    ({datatable(AthletedataLastp10,  rownames = NULL, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', order = list(list(1, 'asc')))) %>% formatStyle(
      columns = c(10),
      #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
      valueColumns = 0,
      target = 'cell',
      backgroundColor = 'gold',
      fontWeight = 'bold')
    })
  })
  
  
  #################FILTER DATA FOR ATHLETE 2############################################
  
  tab_Athlete2 <-  reactive({
    
    ClassProgTimeSec <-  Prognostic.Velocities %>% filter(BoatClass==input$Event)%>% select(ProgTimesec100)
    ClassProgTimeSec <- ClassProgTimeSec[1,1]
    
    Athlete%>%
      filter(Event == input$Event) %>% 
      filter(Name == input$Name2) %>% 
      dplyr::group_by(Year) %>% dplyr::filter(Time_sec == min(Time_sec)) %>%
      dplyr::mutate("% of Prog" := round(((ClassProgTimeSec/Time_sec)*100),digits = 2))
    
  })  
  tab_Athlete2Plot <-  reactive({
    
    Round_order <- c("Final", "A Final", "B Final", "C Final", "Semi-Final")
    Athlete%>%
      filter(Event == input$Event) %>% 
      filter(Name == input$Name2) %>% 
      filter(Comp == "Nationals") %>% 
      dplyr::group_by(Year) %>% 
      arrange(factor(Round, levels = Round_order), .by_group = TRUE) %>%
      slice(1)
  })  
  
  tab_Athlete2Top10 <-  reactive({
    
    ClassProgTimeSec <-  Prognostic.Velocities %>% filter(BoatClass==input$Event)%>% select(ProgTimesec100)
    ClassProgTimeSec <- ClassProgTimeSec[1,1]
    
    Athlete%>%
      filter(Event == input$Event) %>%
      filter(Name == input$Name2) %>%
      dplyr::mutate("% of Prog":= round(((ClassProgTimeSec/Time_sec)*100),digits = 2)) %>%
      arrange(Time) %>%
      head(10)
    
  })
  
  
  tab_Athlete2Last10 <-  reactive({
    ClassProgTimeSec <-  Prognostic.Velocities %>% filter(BoatClass==input$Event)%>% select(ProgTimesec100)
    ClassProgTimeSec <- ClassProgTimeSec[1,1]
    
    Athlete%>%
      filter(Event == input$Event) %>%
      filter(Name == input$Name2) %>%
      dplyr::mutate("% of Prog" := round(((ClassProgTimeSec/Time_sec)*100),digits = 2))%>%
      arrange(Year) %>%
      head(10) 
    
  })
  
  
  
  output$select_Event <-  renderUI({
    
    selectizeInput('Event', 'Select Event', choices = c("select" = "", unique(Athlete$Event)))  
  }) 
  
  output$select_Name2 <-  renderUI({
    inputEvent = as.character(input$Event)
    choice_Name2 <- reactive({
      Athlete%>% 
        filter(Event == inputEvent) %>% 
        pull(Name) %>% 
        as.character()
      
      
    })
    
    if (input$Report_Type == "Two Athletes"){
      selectizeInput('Name2', 'Select Name 2', choices = c("select" = "", choice_Name2()))
    }
  })  
  
  
  
  
  #################OUTPUT TABLES FOR ATHLETE 1#############################################
  
  # output$table_Athlete2 <- renderDataTable({ 
  #   if (input$Report_Type == "Two Athletes"){
  #   Athlete2data <-  tab_Athlete2() %>% select(Name, Year, Comp, Age, Location, Place, Round, Time, Partner,  `% of Prog`)
  #   datatable(Athlete2data, rownames = NULL, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', order = list(list(1, 'asc')))) %>% formatStyle(
  #     columns = c(10),
  #     #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
  #     valueColumns = 0,
  #     target = 'cell',
  #     backgroundColor = 'gold',
  #     fontWeight = 'bold')
  #   }
  # })
  
  output$table_Athlete2Top10 <- renderDataTable({
    if (input$Report_Type == "Two Athletes"){
      Athlete2dataTop10 <-  tab_Athlete2Top10() %>% select(Name, Year, Comp, Age, Location, Place, Round, Time, Partner,  `% of Prog`)
      datatable(Athlete2dataTop10,  rownames = NULL, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', order = list(list(7, 'asc')))) %>% formatStyle(
        columns = c(10),
        #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
        valueColumns = 0,
        target = 'cell',
        backgroundColor = 'gold',
        fontWeight = 'bold')
    }
  })
  
  output$table_Athlete2Last10 <- renderDataTable({
    if (input$Report_Type == "Two Athletes"){
      
      Athletedata2Last10 <-  tab_Athlete2Last10() %>% select(Name, Year, Comp, Age, Location, Place, Round, Time, Partner,  `% of Prog`)
      datatable(Athletedata2Last10,  rownames = NULL, options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),dom='t', order = list(list(1, 'asc')))) %>%formatStyle(
        columns = c(10),
        #c('250m splits (secs)', "Race 2", "Avg Velocity (m/s)", "Avg Prog Speed (%)"),
        valueColumns = 0,
        target = 'cell',
        backgroundColor = 'gold',
        fontWeight = 'bold')
    }
  })
  
  #################FILTER BENCHMARK DATA#############################################
  
  
  
  
  tab_Nationals_Medal <-  reactive({
    
    Benchmark_Nationals%>%
      filter(Event == input$Event) %>% 
      dplyr::group_by(Year) %>% 
      dplyr::filter(Placing == 3) %>%
      mutate(Name=replace(Name, Name == "Nationals", "Nationals Medal"))
  }) 
  
  tab_Nationals_Top8 <-  reactive({
    
    Benchmark_Nationals%>%
      filter(Event == input$Event) %>% 
      dplyr::group_by(Year) %>% 
      dplyr::filter(Placing == 8) %>%
      mutate(Name=replace(Name, Name == "Nationals", "Nationals Top 8"))
  }) 
  
  tab_Worlds_Medal <-  reactive({
    
    Benchmark_Worlds%>%
      filter(Event == input$Event) %>% 
      dplyr::group_by(Year) %>% 
      dplyr::filter(Placing == 3)%>%
      mutate(Name=replace(Name, Name == "World Championships", "Worlds Medal"))
  }) 
  
  tab_Worlds_Top8 <-  reactive({
    
    Benchmark_Worlds%>%
      filter(Event == input$Event) %>% 
      dplyr::group_by(Year) %>% 
      dplyr::filter(Placing == 8)%>%
      mutate(Name=replace(Name, Name == "World Championships", "Worlds Top 8"))
  }) 
  
  
  #    tab_Nationals_Medal()$Competition[tab_Nationals_Medal()$Competition %in% "Nationals"] <- "Nationals Medal"
  #    tab_Nationals_Top8()$Competition[tab_Nationals_Top8()$Competition %in% "Nationals"] <- "Nationals Top 8"
  #    tab_Worlds_Medal()$Competition[tab_Worlds_Medal()$Competition %in% "World Championships"] <- "Worlds Medal"
  #    tab_Worlds_Top8()$Competition[tab_Worlds_Top8()$Competition %in% "World Championships"] <- "Worlds Top 8"
  
  #################OUTPUT PLOTS #############################################
  
  
  Annual_plot_data <- reactive({
    
    
    if (input$Report_Type == "Two Athletes"){
      if (input$Events == "TRUE"){
        v$Annual_plot_data <-  bind_rows(tab_AthletePlot(), tab_Athlete2Plot(), tab_Nationals_Medal(), tab_Nationals_Top8(), tab_Worlds_Medal(), tab_Worlds_Top8()) %>% mutate(Time = as.POSIXct(Time, format='%M:%OS'))
        
      }else {
        v$Annual_plot_data <-  bind_rows(tab_AthletePlot(), tab_Athlete2Plot(), tab_Nationals_Medal(),tab_Nationals_Top8()) %>% mutate(Time = as.POSIXct(Time, format='%M:%OS'))
        
      }
    }else if (input$Report_Type == "Single Athlete"){
      if (input$Events == "TRUE"){
        
        v$Annual_plot_data <-  bind_rows(tab_AthletePlot(), tab_Nationals_Medal(), tab_Nationals_Top8(), tab_Worlds_Medal(), tab_Worlds_Top8()) %>% mutate(Time = as.POSIXct(Time, format='%M:%OS'))
      } else{
        v$Annual_plot_data <-  bind_rows(tab_AthletePlot(), tab_Nationals_Medal(), tab_Nationals_Top8()) %>% mutate(Time = as.POSIXct(Time, format='%M:%OS'))
        
      }
    } 
  })
  
  
  output$ggplot <-  renderPlotly({
    Name_order <- c(input$Name, input$Name2, "Nationals Medal", "Nationals Top 8", "Worlds Medal","Worlds Top 8")
    
    if (input$Report_Type == "Single Athlete"){
      mycolors <- c("red", "blue", "steelblue2", "green4", "limegreen")
      linetype2 <- c("solid", "dashed", "dashed", "dashed", "dashed")
    }else{
      mycolors <- c("red", "black", "blue", "steelblue2", "green4", "limegreen")
      linetype2 <- c("solid", "solid", "dashed", "dashed", "dashed", "dashed")
    }
    Annualplotdata <- Annual_plot_data()
    Annualplotdata$Name_order <- Annualplotdata$Name %>%
      factor(levels = c(input$Name, input$Name2, "Nationals Medal", "Nationals Top 8", "Worlds Medal","Worlds Top 8"))
    
    Annualplot  <-  ggplot(data=Annualplotdata, aes(x=Year, y=Time, group=Name_order, colour=Name_order,linetype = Name_order, 
                                                    text = paste('Year: ', Year, '\n',
                                                                 'Time: ', format(Time, "%M:%S.%OS"), '\n',
                                                                 'Age: ', Age, '\n',
                                                                 'Round: ', Round, '\n'))) + geom_point(size=2) + geom_line(size=1) + 
      scale_y_datetime(date_labels="%M:%S.%OS") +
      scale_colour_manual(name = "", values = mycolors, labels = Name_order) + scale_linetype_manual(name = "", values = linetype2, labels = Name_order)
    
    
    ggplotly(Annualplot, tooltip = 'text') %>% layout(hovermode = 'compare') %>% config(displayModeBar = F)
    
  })
  
  
  Annual_plot_dataLast10 <- reactive({
    
    
    if (input$Report_Type == "Single Athlete"){
      
      v$Annual_plot_dataLast10 <-  bind_rows(tab_AthleteLast10())
      
    }else if (input$Report_Type == "Two Athletes"){
      
      
      v$Annual_plot_dataLast10 <-  bind_rows(tab_AthleteLast10(), tab_Athlete2Last10())
      
      
    } 
  })
  
  
  output$ggplotLast10 <-  renderPlotly({
    Name_order <- c(input$Name, input$Name2)
    if (input$Report_Type == "Single Athlete"){
      mycolors <- c("red")
      linetype2 <- c("solid")
    }else{
      mycolors <- c("red", "blue")
      linetype2 <- c("solid", "solid")
    }
    AnnualplotdataLast10 <<- Annual_plot_dataLast10()
    AnnualplotdataLast10$Name_order <- AnnualplotdataLast10$Name %>%
      factor(levels = c(input$Name, input$Name2))
    AnnualplotdataLast10$Date = as.Date(AnnualplotdataLast10$Date, format = "%d/%m/%Y")
    AnnualplotdataLast10 <-  arrange(AnnualplotdataLast10, Date)
    AnnualplotLast10  <-  ggplot(data=AnnualplotdataLast10, aes(x=Date, y=`% of Prog`, group=Name_order, colour=Name_order,linetype = Name_order, 
                                                                text = paste('Name: ', Name_order, '\n',
                                                                             'Date: ', Date, '\n',
                                                                             'Comp: ', Comp, '\n',
                                                                             'Round: ', Round, '\n',
                                                                             '% of Prog: ', `% of Prog`, '\n'
                                                                ))) + geom_point(size=2) + geom_line(size=1) + 
      scale_colour_manual(name = "", values = mycolors, labels = Name_order) + scale_linetype_manual(name = "", values = linetype2, labels = Name_order)
    
    
    ggplotly(AnnualplotLast10, tooltip = 'text') %>% layout(hovermode = 'compare') %>% config(displayModeBar = F)
    
  })
  
  
  
  #################OUTPUT TITLES#############################################
  
  output$BoxTitleWBT <- renderText({
    paste(input$Event, "World Best")
  })
  output$BoxTitleProg <- renderText({
    paste(input$Event, "Prognostic")
  })
  output$BoxTitle1 <- renderText({
    paste(input$Name,"&",input$Name2, "National Final vs Benchmarks -", input$Event)
  })
  output$BoxTitleBest <- renderText({
    paste(input$Name, "-", input$Event, "Best per Year")
  })
  output$BoxTitleTop10 <- renderText({
    paste(input$Name, "-", input$Event, "All Time Top 10")
  })
  output$BoxTitleLast10 <- renderText({
    paste(input$Name, "-", input$Event, "Last 10 Times")
  })
  output$BoxTitleLast10plot <- renderText({
    paste(input$Name,"&", input$Name2, "-", input$Event, "Last 10 Times")
  })
  output$BoxTitleBest2 <- renderText({
    if (input$Report_Type == "Two Athletes"){
      paste(input$Name2, "-", input$Event, "Best per year")}
  })
  output$BoxTitleTop102 <- renderText({
    if (input$Report_Type == "Two Athletes"){
      paste(input$Name2, "-", input$Event, "All Time Top 10")}
  })
  output$BoxTitleLast102 <- renderText({
    if (input$Report_Type == "Two Athletes"){
      paste(input$Name2, "-", input$Event, "Last 10 Times")}
  })
  
  #### Output headers ####
  #output$Summaryhead <- renderText({paste(tab()$First_Name[1], tab()$Last_Name[1], tab()$Competition[1], tab()$Class[1], tab()$Distance[1], tab()$Phase[1], "vs Top 10 average")
  #})
  #output$Timehead <- renderText({paste0("Time vs Top 10 average")
  #})    
  #
  #output$Summaryheadgap <- renderText({
  #  paste(" ")
  #})
  #output$Splithead <- renderText({paste0("Splits vs Top 10 average")
  #})
  
  #})
  
}

