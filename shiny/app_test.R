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
library(shinydashboard)
library(ggmap)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)
library(data.table)

#############################
## Data
# navigator.dir = path("/Volumes/GoogleDrive/Shared drives/Disparities Navigator")
# d <- read.csv(here::here(navigator.dir, "LAC_AAIRs_bySiteSex5YearRangeRaceEth_20002019_03FEB22.csv"), na.strings = c("NA", "~"))
# d$csp_race_mod <- recode_factor(d$csp_race_mod, "South Asian (Indian/Pakistani/Sri Lankan/Bangladesh/etc)" = "South Asian")
# d$csp_race_mod <- factor(d$csp_race_mod, levels=c(sort(levels(d$csp_race_mod), decreasing=F)))
d2 = readRDS("d.rds")
d = d2

## Get arranged list of cancer sites
sites_arrange = d %>% filter(sex == "Male and Female",csp_race_mod == "All Races/Ethnicities", YEARRANGE == "All (2000-2019)") %>% 
  #group_by("SEERCODE") %>% 
  arrange(desc(aair)) 
cancer_sites <- sites_arrange$SEERCODE

#d[is.na(d)] <- 0

d = na.omit(d)




#############################
## UI DASHBOARD
ui = dashboardPage(skin = 'blue',
                   dashboardHeader(title = "Cancer Disparities Navigator", titleWidth = 300),
                   
                  ## Sidebar content
                   dashboardSidebar(
                     width = 300,
                     sidebarMenu(
                       menuItem("About", tabName = "about", icon = icon("question-circle")),
                       menuItem("Cancer Trends", tabName = "scatter1", icon = icon("chart-line")),
                       menuItem("Cancers by Sex, Race", tabName = "bars1", icon = icon("chart-bar")) #,
                       
                     )
                   ), # End Sidebard
                   
                   ## Body Content
                   dashboardBody(
                     tabItems(
                       
                       
                       tabItem(tabName = "about", 
                               fluidRow(
                                 box(title = "Cancer Disparities Navigator", width = 12, 
                                     
                                     strong("Case Selection:"), br(),
                                     "Los Angeles County", br(),
                                     "Years: 2000-2019", br(),
                                     "Sex: Male or Female", br(),
                                     "Cancer stage: All", br(),
                                     "Behavior: All", br(),
                                     "Age-Adjusted Incidence Rates", br(), br(),
                                     strong("Notes"), br(),
                                     "Case counts are based on tumor-level records, patients diagnosed with a second tumor are recounted.", br(),
                                     "Cancer site was determined using SEER Site Recodes ICD-O-3/WHO 2008 (SEERWHO).", br(),
                                     "Rates standardized to the US 2000 Standard Population (18 age groups).", br(),
                                     "Population denominators were calculated using the US Decennial Census Population for LA County for 2000 and 2010.", br(),
                                     "Interim years (2001-09) were interpolated and subsequent years (2011-2019) were extrapolated.", br(),
                                     "Race/Ethnicity-specific population counts are averages of the Census 'Alone' and 'Alone or in Combination' counts.", br()
                                     
                                 ))),
                       # First tab content
                       tabItem(tabName = "scatter1",
                               fluidRow(
                                 tabBox(
                                   side = "left",
                                   id = "tabset1", width = 12, 
                                   
                                   tabPanel("Trends by Race/Ethnicity", 
                                            
                                            selectInput("site_select", "Cancer site: ", choices = cancer_sites,
                                                        multiple = FALSE, selected = "All Cancer Sites"),
                                            selectInput("sex_select_trendRace", "Sex: ", choices = unique(d$sex),
                                                        multiple = FALSE, selected = "Male and Female"),
                                            
                                            
                                            # conditionalPanel(
                                            #   condition = "input.site_select != 'Prostate'",
                                            #   selectInput(
                                            #     "sex_select_trendRace", "Sex: ", choices = unique(d$sex),
                                            #     multiple = FALSE, selected = "Male and Female")
                                            # ),
                                            # conditionalPanel(
                                            #   condition = "input.site_select == 'Prostate'",
                                            #   selectInput(
                                            #     "sex_select_trendRace", "Sex: ", choices = "Male",
                                            #     multiple = FALSE, selected = "Male")
                                            # ),
                                            
                                            
                                            # selectInput("sex_select_trendRace", "Sex: ", choices = unique(d$sex),
                                            #             multiple = FALSE, selected = "Male and Female"),
                                            selectInput("race_select", "Race/ethnicities: ", choices = unique(d$csp_race_mod),
                                                        multiple = TRUE, selected = c("All Races/Ethnicities", "Hispanic White", "Non-Hispanic White", "Black")),
                                            plotlyOutput("scatter_ci", width = "900", height = "550")
                                   ),
                                   
                                   tabPanel("Trends by Cancer Site",
                                            selectInput("race_select_trendSite", "Race/ethnicity: ", choices = unique(d$csp_race_mod),
                                                        multiple = FALSE, selected = "All Races/Ethnicities"),
                                            selectInput("sex_select_trendSite", "Sex: ", choices = unique(d$sex),
                                                        multiple = FALSE, selected = "Male and Female"),
                                            selectInput("site_select_trendSite", "Cancer sites: ", choices = cancer_sites,
                                                        multiple = TRUE, selected = c("Breast", "Prostate", "Lung and Bronchus",
                                                                                      "Colon excluding Rectum",
                                                                                      "Melanomas of the Skin",
                                                                                      "Non-Hodgkins Lymphomas",
                                                                                      "Urinary Bladder")
                                            ),
                                            plotlyOutput("trendSite", width = "900", height = "550")
                                   ) 
                                   
                                 )
                                 
                               )
                       ), # End scatter tab1,
                       
                       # Second tab content
                       tabItem(tabName = "bars1",
                               fluidRow(
                                 tabBox(
                                   side = "left",
                                   # The id lets us use input$tabset1 on the server to find the current tab
                                   id = "tabset1", width = 12, 
                                   tabPanel("Cancer Site by Race/Ethnicity", 
                                            
                                            selectInput("race_select_barSite", "Race/ethnicity: ", choices = unique(d$csp_race_mod),
                                                        multiple = TRUE, selected = c("Hispanic White", "Non-Hispanic White")),
                                            selectInput("sex_select_barSite", "Sex: ", choices = unique(d$sex),
                                                        multiple = FALSE, selected = "Male and Female"),
                                            selectInput("site_select_barSite", "Cancer sites: ", choices = cancer_sites,
                                                        multiple = TRUE, selected = c("Breast", "Prostate", "Lung and Bronchus",
                                                                                      "Colon excluding Rectum",
                                                                                      "Melanomas of the Skin",
                                                                                      "Non-Hodgkins Lymphomas",
                                                                                      "Urinary Bladder")
                                            ),
                                            plotlyOutput("barplot_site", width = "900", height = "550")
                                   ),
                                   tabPanel("Race/Ethnicity by Cancer Site", 
                                            
                                            selectInput("race_select_barRace", "Race/ethnicity: ", choices = unique(d$csp_race_mod),
                                                        multiple = TRUE, selected = c("Hispanic White", "Non-Hispanic White", "Black", 
                                                                                      "Chinese", "Hawaiian/Samoan/Pacific Islander", "Korean", "Vietnamese"   
                                                        )),
                                            selectInput("sex_select_barRace", "Sex: ", choices = unique(d$sex),
                                                        multiple = FALSE, selected = "Male and Female"),
                                            selectInput("site_select_barRace", "Cancer sites: ", choices = cancer_sites,
                                                        multiple = TRUE, selected = c("Breast", "Prostate", "Lung and Bronchus")
                                            ),
                                            plotlyOutput("barplot_all", width = "900", height = "550")
                                   )
                                   
                                   
                                 )
                                 
                               )
                       ) # End scatter tab2
                       
                       
                       
                       
                       
                     ) # End Tab Items
                   ) # End Body
)


#############################
## SERVER

not_prostate_female <- function(in1, in2){
  if ((in1 == "Prostate")&&(in2=="Female")){ "Not a valid selection. Please select 'Male' or 'Male and Female'." }
}

# not_prostateIncluded_female <- function(in1, in2){
#   if ((in1 == "Female")&&(in2 == "Prostate" )){ "Not a valid selection, please remove 'Prostate', or select 'Male' or 'Male and Female'." }
# }


server <- function(input, output) {
  
  ###########################
  ## Trends by race/ethnicity
  
  
  CI_scatter <- reactive({
    
    validate(
      not_prostate_female(input$site_select, input$sex_select_trendRace),
      need(input$race_select != "", "Please select a race/ethnicity")
    )
    
    
    d.all = d %>% filter(SEERCODE == input$site_select, sex == input$sex_select_trendRace, YEARRANGE != "All (2000-2019)") %>% 
      filter(csp_race_mod %in% input$race_select)
    
    d.all$yearNum = unclass(d.all$YEARRANGE)
    d2 = data.table(d.all)
    d3 = d2[,as.list(coef(lm(aair~yearNum))),by=csp_race_mod]
    colnames(d3) = c("csp_race_mod","Intercept","Trend")
    d4 = merge(d2, d3, by="csp_race_mod")
    d4 = data.frame(d4)
    
    g1 = ggplot(d4, aes(x = YEARRANGE, y = aair, group=csp_race_mod, color = csp_race_mod,
                        text=paste( 
                          paste("Cancer site: ", SEERCODE),
                          paste("Race/ethnicity: ", csp_race_mod),
                          paste("Cases: ", cases, sep=""), 
                          paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),
                          paste("Trend: ", round(Trend/5,2), "units change in rate per year" ) ,
                          sep = "<br>"))) + 
      geom_smooth(aes(YEARRANGE, aair), method = "lm", se = F,  size = .5) +
      geom_point(size = 1.5, alpha = 0.5) + 
      geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.5,position=position_dodge(0.1)) +
      labs(color = "User-Selected Races", title = paste0("Cancer Site: ", input$site_select, ", Sex: ", input$sex_select_trendRace,  ", User Selected Races"), x = "Year Range", y = "Age-Adjusted Incidence Rate") +
      theme_ipsum(axis_text_size = 15, axis_title_size = 15) +
      expand_limits(y=0)
    ggplotly(g1, tooltip = "text")
    # g2
    
  })
  
  output$scatter_ci <- renderPlotly({
    CI_scatter()
  })
  
  
  ##############################################
  
 
  
  
  
  #################################
  ## Trends by Cancer Site
  TREND_SITE <- reactive({
    
    validate(
      need(input$site_select_trendSite != "", "Please select a cancer site")
    )
    
    d.all = d %>% filter(SEERCODE %in% input$site_select_trendSite, sex == input$sex_select_trendSite, YEARRANGE != "All (2000-2019)") %>% 
      filter(csp_race_mod == input$race_select_trendSite)
    
    d.all$yearNum = unclass(d.all$YEARRANGE)
    d2 = data.table(d.all)
    d3 = d2[,as.list(coef(lm(aair~yearNum))),by=SEERCODE]
    colnames(d3) = c("SEERCODE","Intercept","Trend")
    d4 = merge(d2, d3, by="SEERCODE")
    d4 = data.frame(d4)
    
    g1 =
      ggplot(d4, aes(x = YEARRANGE, y = aair, group=SEERCODE, color = SEERCODE,
                     text=paste( 
                       paste("Cancer site: ", SEERCODE),
                       paste("Sex: ", sex),
                       paste("Race/ethnicity: ", csp_race_mod),
                       paste("Cases: ", cases, sep=""), 
                       paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),
                       paste("Trend: ", round(Trend/5,2), "units change in rate per year" ) ,
                       sep = "<br>"))) + 
      geom_smooth(aes(YEARRANGE, aair), method = "lm", se = F,  size = .5) +
      geom_point(size = 1.5, alpha = 0.5) + 
      geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.5,position=position_dodge(0.1)) +
      labs(color = "User-Selected Cancer Sites", title = paste0("Sex: ", input$sex_select_trendSite,  ", Race/ethnicity: ", input$race_select_trendSite, ", User Selected Cancers"), x = "Year Range", y = "Age-Adjusted Incidence Rate") +
      #scale_color_viridis(discrete = TRUE, option="turbo") +
      theme_ipsum(axis_text_size = 15, axis_title_size = 15) +
      expand_limits(y=0)
    ggplotly(g1, tooltip = "text")
    # g2
    
    
    
  })
  
  output$trendSite <- renderPlotly({
    TREND_SITE()
  })  
  
  
  # ############################
  # ## Bar plots: Cancers grouped by race/ethnicity, sex
  
  ##############################
  ## Bar plot: Site
  
  barplot_site1 <- reactive({
    
    validate(
      need(input$site_select_barSite != "", "Please select a cancer site")
    )
    
    d.all = d %>% filter(SEERCODE %in% input$site_select_barSite, sex == input$sex_select_barSite, YEARRANGE == "All (2000-2019)") %>% 
      filter(csp_race_mod %in% input$race_select_barSite)
    
    b1 = ggplot(d.all, aes(x=reorder(SEERCODE,aair), y=aair, fill=csp_race_mod,
                           text=paste( 
                             paste("Cancer site: ", SEERCODE),
                             paste("Sex: ", sex),
                             paste("Race/ethnicity: ", csp_race_mod),
                             paste("Cases: ", cases, sep=""), 
                             paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),
                             sep = "<br>")
    )) +
      geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
      geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +
      labs(fill = "User-Selected Race/Ethnicity", title = paste0(input$sex_select_barSite,  " Cancer by Race/Ethnicity, 2000-2019"), x = "", y = "Age-Adjusted Incidence Rate") +
      coord_flip() + 
      theme_ipsum(axis_text_size = 15, axis_title_size = 10)
    
    ggplotly(b1, tooltip = "text")
    
  })
  
  output$barplot_site <- renderPlotly({
    barplot_site1()
  })  
  
  
  
  
  
  
  
  
  
  ##############################
  ## Bar plot: Race
  barplot_race <- reactive({
    
    validate(
      need(input$site_select_barRace != "", "Please select a cancer site")
    )
    
    d.all = d %>% filter(SEERCODE %in% input$site_select_barRace, sex == input$sex_select_barRace, YEARRANGE == "All (2000-2019)") %>% 
      filter(csp_race_mod %in% input$race_select_barRace)
    
    b1 = ggplot(d.all, aes(x=reorder(csp_race_mod,aair), y=aair, fill=SEERCODE,
                           text=paste( 
                             paste("Cancer site: ", SEERCODE),
                             paste("Sex: ", sex),
                             paste("Race/ethnicity: ", csp_race_mod),
                             paste("Cases: ", cases, sep=""), 
                             paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),
                             sep = "<br>")
    )) +
      geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
      geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +
      labs(fill = "User-Selected Cancer Sites", title = paste0(input$sex_select_barSite,  " Race/Ethnicity by Cancer, 2000-2019"), x = "", y = "Age-Adjusted Incidence Rate") +
      coord_flip() + 
      theme_ipsum(axis_text_size = 15, axis_title_size = 10)
    
    ggplotly(b1, tooltip = "text")
    
  })
  
  
  output$barplot_all <- renderPlotly({
    barplot_race()
  })  
  
}

shinyApp(ui = ui, server = server)



# library(rsconnect)
# rsconnect::deployApp('shiny')




