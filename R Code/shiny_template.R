climatic_shiny <- function(){
  ui <- secure_app(fluidPage(
    
    dashboardPage(
      header = dashboardHeader(title = "Download Climatic Data"),
      skin = "blue",
      sidebar = dashboardSidebar(),
      
      # Main panel for displaying outputs ----
      dashboardBody(
        fluidRow(
          box(width = 6, 
              # Input: Choose dataset ----
              selectInput("dataset", "Choose a dataset:",
                          choices = c("Station Metadata",  "Rainfall Data",
                                      "Tmin Data", "Tmax Data", "All Elements",
                                      "Test rain", "Test temperature", "Test rain temp")),
              # Button
              downloadButton("downloadData", "Download")),
          box(width = 6,
              textInput("raw_name", "File name:", "raw_json"),
              downloadButton("downloadRawData", "Download Raw Data"))),
        fluidRow(box(width = 12,
                     dataTableOutput("table")))
      )
      
    )
  )
  )
  
  server <- function(input, output) {
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
      reactiveValuesToList(result_auth)
    })
    
    updated_data <- update_data()
    station <- updated_data[[1]]
    rain_df <- updated_data[[2]]
    tmin_df <- updated_data[[3]]
    tmax_df <- updated_data[[4]]
    all_elements_df <- dplyr::bind_rows(rain_df, tmin_df, tmax_df)
    nest_df <- all_elements_df %>% nest(data = !Station)
    nest_df_i <- NULL
    for (i in unique(nest_df$Station)){
      nest_df_i[[i]] <- data.frame(nest_df$data[nest_df$Station==i])
    }
    names(nest_df_i) <- unique(nest_df$Station)
    
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
      switch(input$dataset,
             "Station Metadata" = station,
             "Rainfall Data" = rain_df,
             "Tmin Data" = tmin_df,
             "Tmax Data" = tmax_df,
             "All Elements" = all_elements_df,
             "Test rain" = nest_df_i$`Test rain`,
             "Test temperature" = nest_df_i$`Test temperature`,
             "Test rain temp" = nest_df_i$`Test rain temp`)
    })
    
    # Table of selected dataset ----
    output$table <- renderDataTable({
      datasetInput()
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    output$downloadRawData <- downloadHandler(
      filename = function() {
        paste0(input$raw_name, ".json")
      },
      content = function(file) {
        write(raw_data(), file)
      }
    )
  }
  
  shiny::shinyApp(ui = ui, server = server)
}
