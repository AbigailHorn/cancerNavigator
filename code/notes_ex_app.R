
#########################
## UI

tabItem(tabName = "scatter",
        fluidRow(
          tabBox(
            side = "left",
            id = "tabset1", width = 12, 
            tabPanel("Two-Week Crude Incidence Rate", 
                     selectInput("scatter_ci_neighbor", "Neighborhood: ", choices = unique(LAC_SummaryTable$`Neighborhood in Los Angeles City`),
                                 multiple = TRUE),
                     plotOutput("scatter_ci", width = "1200", height = "800")),
            
            tabPanel("Two-Week Mortality Rate", 
                     selectInput("scatter_m_neighbor", "Neighborhood: ", choices = unique(LAC_SummaryTable$`Neighborhood in Los Angeles City`),
                                 multiple = TRUE),
                     plotOutput("scatter_m", width = "1200", height = "800"))                                    
          )
          
        )
)






#########################
## SERVER

CI_scatter <- reactive({
  
  # modified data step 
  test <- LAC_SummaryTable %>% filter(`Data Type` == "Two Week Crude IR (per 100k)") %>%
    mutate(Neighborhood = fct_relevel(as.factor(`Neighborhood in Los Angeles City`), "City of LA")) %>% 
    filter(Neighborhood %in% c("City of LA", input$scatter_ci_neighbor)) %>%
    select(lon, lat, everything()) %>%
    select(-c(1:5)) %>%
    mutate_at(vars(-Neighborhood), as.numeric) %>% # some NA by coercion
    # rename_all(function(x) as.Date.character(x, tryFormats = c("%d-%b"))) %>%
    pivot_longer(-Neighborhood, names_to = "date", values_to = "IR") %>%
    mutate(date = as.Date(date, tryFormats = c("%d-%b")))
  
  
  # plot
  ggplot(test, aes(x = date, y = IR, group = Neighborhood, color = Neighborhood)) + 
    geom_line(size = 1.3) +
    scale_y_continuous("Two-week Incidence Rate (per 100k)") + 
    scale_x_date(date_breaks = "2 weeks") +
    theme_bw() + 
    theme(text = element_text(size = 14), 
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.text = element_text(size=14))
})

output$scatter_ci <- renderPlot({
  CI_scatter()
})

