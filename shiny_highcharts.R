

ui <- fluidPage(
  h1("Highcharts Demo"),
  fluidRow(
    column(width = 4, class = "panel",
           selectInput("type", label = "Type", width = "100%",
                       choices = c("line", "column", "bar", "spline")), 
           selectInput("stacked", label = "Stacked",  width = "100%",
                       choices = c(FALSE, "normal", "percent")),
           selectInput("theme", label = "Theme",  width = "100%",
                       choices = c(FALSE, "fivethirtyeight", "economist",
                                   "darkunica", "gridlight", "sandsignika",
                                   "null", "handdrwran", "chalk","thm2")
           ),
           textInput('img',label = 'Img-url',value = "https://i.loli.net/2019/07/18/5d2fd16c1b97490683.jpeg",width = "100%")
    ),
    column(width = 8,
           highchartOutput("hcontainer",height = "500px")
    

)
)
)

server = function(input, output) {
  
  output$hcontainer <- renderHighchart({
    
    thm2 <- hc_theme_merge(
      hc_theme_darkunica(),

      hc_theme(
        chart = list(
          backgroundColor = "transparent",
          divBackgroundImage = input$img
        ),
        title = list(
          style = list(
            color = 'white',
            fontFamily = "Open Sans"
 
          )
        )
      )
    )
    
    hc <- highcharts_demo() %>%
      hc_rm_series("Berlin") %>% 
      hc_chart(type = input$type)
    
    if (input$stacked != FALSE) {
      hc <- hc %>%
        hc_plotOptions(series = list(stacking = input$stacked))
    }
    
    
    if (input$theme != FALSE) {
      theme <- switch(input$theme,
                      null = hc_theme_null(),
                      darkunica = hc_theme_darkunica(),
                      gridlight = hc_theme_gridlight(),
                      sandsignika = hc_theme_sandsignika(),
                      fivethirtyeight = hc_theme_538(),
                      economist = hc_theme_economist(),
                      chalk = hc_theme_chalk(),
                      handdrwran = hc_theme_handdrawn(),
                      thm2=thm2
      
      )
      
      hc <- hc %>% hc_add_theme(theme)
      
    }
    
    hc
    
  })
  
  
}

shinyApp(ui = ui, server = server)
