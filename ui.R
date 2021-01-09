#Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

ui <- dashboardPage(
  dashboardHeader (title = "Canoe Sprint Annual Progress Report", titleWidth = 450),
  dashboardSidebar(
    #fileInput("Athletes", "Choose Athlete Data"),
    #fileInput("Benchmark", "Choose Benchmark Data"),
    #actionButton("goButton", "Go!"),
    
    uiOutput("select_Event"),
    selectInput("Report_Type", "Report Type:",
                c("Single Athlete" = "Single Athlete",
                  "Two Athletes" = "Two Athletes")),
    uiOutput("select_Name"),
    uiOutput("select_Age"),
    uiOutput("select_Name2"),
    uiOutput("select_Age2"),
    checkboxInput("Events", label = "World", value = TRUE)),
    
  dashboardBody(
    fluidRow(
      column(6,box(title = textOutput("BoxTitleWBT"), background = "orange", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          dataTableOutput("table_WBT"))),
          column(6,box(title = textOutput("BoxTitleProg"), background = "green", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          dataTableOutput("table_Prog")))
    ),
    fluidRow(
      box(title = textOutput("BoxTitle1"), status = "success", solidHeader = TRUE,width = 12,
          collapsible = FALSE,plotlyOutput("ggplot"))
    ),
    fluidRow(
      box(title = textOutput("BoxTitleTop10"), status = "warning", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          dataTableOutput("table_AthleteTop10"))
    ),
    fluidRow(
      box(title = textOutput("BoxTitleTop102"), status = "warning", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          dataTableOutput("table_Athlete2Top10"))
    ),
    # fluidRow(
    #   box(title = textOutput("BoxTitleBest"), status = "primary", solidHeader = TRUE, width = 12,
    #       collapsible = FALSE,
    #       dataTableOutput("table_Athlete"))
    # ),
    # fluidRow(
    #   box(title = textOutput("BoxTitleBest2"), status = "primary", solidHeader = TRUE, width = 12,
    #       collapsible = FALSE,
    #       dataTableOutput("table_Athlete2"))
    #   
    # ),
    fluidRow(
      box(title = textOutput("BoxTitleLast10plot"), status = "danger", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          plotlyOutput("ggplotLast10"))
    ),
    fluidRow(
      box(title = textOutput("BoxTitleLast10"), status = "danger", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          dataTableOutput("table_AthleteLast10"))
    ),
    fluidRow(
      box(title = textOutput("BoxTitleLast102"), status = "danger", solidHeader = TRUE, width = 12,
          collapsible = FALSE,
          dataTableOutput("table_Athlete2Last10"))
    ),






  )
  
 
  #   fluidRow(
  #     box(title = "Splits vs Top 10 average", status = "primary", solidHeader = TRUE,width = 12,
  #         collapsible = TRUE,dataTableOutput("table3split"))
  #   ),
  #   fluidRow(
  #     box(title = "Splits vs Top 10 average", status = "primary", solidHeader = TRUE,width = 12,
  #         collapsible = TRUE,plotOutput("ggplotsplit"))
  #     
  #   )
  #   
  # )
)









# Create Shiny app ----
# shinyApp(ui, server)