#############################
## Packages

library(reshape2)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggrepel)
library(bindata)
library(odin)
library(fitR)
library(knitr)
library(EasyABC)
library(gridExtra)
library(odin)
library(lubridate)
library(EasyABC)
library(gridExtra)
library(kableExtra)
library(plyr)
library(dplyr)
library(data.table)
library(scales)
library(EasyABC)
library(patchwork)

library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(network)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(networkD3)
library(ggmosaic)
library(formattable)
library(DT)
library(reshape)
library(here)
library(fs)
library(hrbrthemes)

library(plotly)

#############################
## Data
navigator.dir = path("/Volumes/GoogleDrive/Shared drives/Disparities Navigator")
d <- read.csv(here::here(navigator.dir, "LAC_AAIRs_bySiteSex5YearRangeRaceEth_20002019_03FEB22.csv"), na.strings = c("NA", "~"))
d$csp_race_mod <- recode_factor(d$csp_race_mod, "South Asian (Indian/Pakistani/Sri Lankan/Bangladesh/etc)" = "South Asian")
d$csp_race_mod <- factor(d$csp_race_mod, levels=c(sort(levels(d$csp_race_mod), decreasing=F)))

sites_arrange = d %>% filter(sex == "Male and Female",csp_race_mod == "All Races/Ethnicities", YEARRANGE == "All (2000-2019)") %>% 
  #group_by("SEERCODE") %>% 
  arrange(desc(aair)) 
cancer_sites <- sites_arrange$SEERCODE


#############################
## UI
ui <- fluidPage(
  
  titlePanel("Cancer Disparities Navigator"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("site_select", "Cancer site: ", choices = cancer_sites,
                  multiple = FALSE, selected = "All Cancer Sites"),
      selectInput("race_select", "Race/ethnicity: ", choices = unique(d$csp_race_mod),
                  multiple = TRUE, selected = c("All Races/Ethnicities", "Hispanic White", "Non-Hispanic White", "Black"))
    ),

    mainPanel(
      plotlyOutput("scatter_ci") #, width = "1200", height = "800")
    )
  )
)

#site_select
#race_select

#############################
## SERVER

server <- function(input, output) {
  
  CI_scatter <- reactive({
    
    d.all = d %>% filter(SEERCODE == input$site_select, sex == "Male and Female", YEARRANGE != "All (2000-2019)") %>% 
      filter(csp_race_mod %in% input$race_select)

    
    g1 =
      ggplot(d.all, aes(x = YEARRANGE, y = aair, group=csp_race_mod, color = csp_race_mod,
                           text=paste( 
                             paste("Cancer site: ", SEERCODE),
                             paste("Race/ethnicity: ", csp_race_mod),
                             paste("Cases: ", cases, sep=""), 
                             paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),sep = "<br>"))) + 
      geom_line(size = .5) + geom_point(size = 1.5, alpha = 0.5) + 
      geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.5,position=position_dodge(0.1)) +
      labs(color = "User-Selected Races", title = paste0("Cancer Site: ", input$site_select, ", User Selected Races"), x = "Year Range", y = "Age-Adjusted Incidence Rate") +
      #scale_color_viridis(discrete = TRUE, option="turbo") +
      theme_ipsum(axis_text_size = 15, axis_title_size = 15)
    ggplotly(g1, tooltip = "text")
    # g2
  
  })
  
  output$scatter_ci <- renderPlotly({
    CI_scatter()
  })
  
}

shinyApp(ui = ui, server = server)








