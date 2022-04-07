
# Top N cancer sites
d.top = d %>% filter(SEERCODE != "All Cancer Sites", sex == "Male and Female",csp_race_mod == "All Races/Ethnicities", YEARRANGE == "All (2000-2019)") %>% 
  #group_by("SEERCODE") %>% 
  arrange(desc(aair)) %>% 
  top_n(10, aair) %>% 
  select(SEERCODE) %>% 
  as.matrix()

# Grouped barplot
## Grouped barplot groups = races and years + errorbars 
d %>% filter(SEERCODE =="All Cancer Sites", sex == "Male and Female") %>% 
  ggplot(aes(x=csp_race_mod, y=aair, fill=YEARRANGE)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8))

# Not grouped 

## Top N sites -- all races
d %>% filter(SEERCODE %in% d.top, sex == "Male and Female", csp_race_mod == "All Races/Ethnicities", YEARRANGE == "All (2000-2019)") %>% 
  ggplot(aes(x = reorder(SEERCODE, aair), y=aair, fill=SEERCODE)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +  
  coord_flip()

## Top N sites -- SELECT RACE
d %>% filter(SEERCODE %in% d.top, sex == "Male and Female", csp_race_mod == "Black", YEARRANGE == "All (2000-2019)") %>% 
  ggplot(aes(x = reorder(SEERCODE, aair), y=aair, fill=SEERCODE)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +  
  coord_flip()

## Top N sites -- FACET RACES
d %>% filter(SEERCODE %in% d.top, sex == "Male and Female", YEARRANGE == "All (2000-2019)") %>% 
  ggplot(aes(x = reorder(SEERCODE, aair), y=aair, fill=SEERCODE)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +  
  facet_wrap(~csp_race_mod) +
  coord_flip()

## Top N sites -- facet by site (Y = races)
test = d %>% filter(SEERCODE %in% d.top, sex == "Male and Female", YEARRANGE == "All (2000-2019)") %>% 
  ggplot(aes(x = reorder(csp_race_mod, aair), y=aair, fill=csp_race_mod)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +  
  facet_wrap(~SEERCODE) +
  coord_flip() 
ggplotly(test)

## Races for a specific cancer site (CHOOSE CANCER SITE)
d %>% filter(SEERCODE == "Breast", sex == "Male and Female", csp_race_mod != "All Races/Ethnicities", YEARRANGE == "All (2000-2019)") %>% 
  ggplot(aes(x = reorder(csp_race_mod, aair), y=aair, fill=csp_race_mod)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +  
  coord_flip()

d %>% filter(SEERCODE == "Prostate", sex == "Male and Female", csp_race_mod != "All Races/Ethnicities", YEARRANGE == "All (2000-2019)") %>% 
  ggplot(aes(x = reorder(csp_race_mod, aair), y=aair, fill=csp_race_mod)) +
  geom_bar(position=position_dodge(width=.8), stat="identity", width=.7, colour='black') +
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=.2,position=position_dodge(.8)) +  
  coord_flip()

# # Facetwrap (don't like this)
# ggplot(test, aes(fill=csp_race_mod, y=aair, x=csp_race_mod)) + 
#   geom_bar(position="dodge", stat="identity") +
#   scale_fill_viridis(discrete = T, option = "E") +
#   ggtitle("Studying 4 species..") +
#   facet_wrap(~SEERCODE) +
#   theme_ipsum() +
#   theme(legend.position="none") +
#   xlab("")

# Heatmaps

# Heatmat 
dmat = d %>% filter(SEERCODE == "All Cancer Sites", sex == "Male and Female") %>% select(c(YEARRANGE, csp_race_mod, aair)) %>% pivot_wider(names_from = YEARRANGE, values_from = aair) %>% as.data.frame()
rownames(dmat) = dmat$csp_race_mod
dmat$csp_race_mod = NULL
dmat = as.matrix(dmat)

plot_ly(x=colnames(dmat), y=rownames(dmat),
        z=~dmat,
        type="heatmap",
        showscale=T)

## Heatmap top 10 cancer sites x races
dmat = d %>% filter(YEARRANGE == "All (2000-2019)", sex == "Male and Female", SEERCODE %in% d.top) %>% 
  select(c(SEERCODE, csp_race_mod, aair)) %>% 
  filter(SEERCODE!="All Cancer Sites") %>% 
  pivot_wider(names_from = csp_race_mod, values_from = aair) %>% 
  as.data.frame()
rownames(dmat) = dmat$SEERCODE
dmat$SEERCODE = NULL
dmat = as.matrix(dmat)

plot_ly(x=colnames(dmat), y=rownames(dmat),
        z=~dmat,
        type="heatmap",
        showscale=T)

# Heatmap top 10 sites (ordered by all) compare across races 
# Single bar all races / specific race top 10 sites (later will be allowed to select race)
# Line comparison

# Line plots
## Line plot comparing races
d.all = d %>% filter(SEERCODE == "All Cancer Sites", sex == "Male and Female", YEARRANGE != "All (2000-2019)")
g1 = ggplot(d.all, aes(x = YEARRANGE, y = aair, group=csp_race_mod, color = csp_race_mod,
                           text=paste( 
                             paste("Cancer site: ", SEERCODE),
                             paste("Race/ethnicity: ", csp_race_mod),
                             paste("Cases: ", cases, sep=""), 
                                       paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),sep = "<br>"))) + 
  geom_line(size = .5) + geom_point(size = 1.5, alpha = 0.5) + 
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=2,position=position_dodge(0.1)) +
  theme_ipsum()
g2<-ggplotly(g1, tooltip = "text")
g2

## Line plot comparing races for specific cancer site (select site)
d.all = d %>% filter(SEERCODE == "Breast", sex == "Female", YEARRANGE != "All (2000-2019)")
g1 = ggplot(d.all, aes(x = YEARRANGE, y = aair, group=csp_race_mod, color = csp_race_mod,
                       text=paste( 
                         paste("Cancer site: ", SEERCODE),
                         paste("Race/ethnicity: ", csp_race_mod),
                         paste("Cases: ", cases, sep=""), 
                         paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),sep = "<br>"))) + 
  geom_line(size = .5) + geom_point(size = 1.5, alpha = 0.5) + 
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=2,position=position_dodge(0.1)) +
  #scale_color_viridis(discrete = TRUE, option="turbo") +
  theme_ipsum()
g2<-ggplotly(g1, tooltip = "text")
g2



## Line plot top 10 cancer sites 
d.all = d %>% filter(sex == "Male and Female",csp_race_mod == "All Races/Ethnicities", SEERCODE %in% as.matrix(d.top), YEARRANGE != "All (2000-2019)")

g1 = ggplot(d.all, aes(x = YEARRANGE, y = aair, group=SEERCODE, color = SEERCODE,
                       text=paste( 
                         paste("Cancer site: ", SEERCODE),
                         paste("Race/ethnicity: ", csp_race_mod),
                         paste("Cases: ", cases, sep=""), 
                         paste(" Age-adjusted IR (95% CI): ", aair, " (",lclair, ", ",uclair,")", sep=""),sep = "<br>"))) + 
  geom_line(size = .5) + geom_point(size = 1.5, alpha = 0.5) + 
  geom_errorbar(aes(ymin=lclair, ymax=uclair), width=2,position=position_dodge(0.1)) +
  #scale_color_viridis(discrete = TRUE, option="turbo") +
  theme_ipsum()
g2<-ggplotly(g1, tooltip = "text")
g2









##########################################################################################
##########################################################################################

grouped.var.figs <- function(full.data, var, var.name, times.dates){
  
  ## Name the time periods depicted
  times.names <- times.dates
  names(times.names) <- times.dates
  
  ## Enable using aes_string()
  stage.str <- "stage"
  values.str <- "values"
  var.str <- as.character(var)
  
  ## Fig
  grouped.fig <- ggplot(full.data, aes_string(x = stage.str, y = values.str, fill = var)) +
    geom_bar(position="stack", stat = 'identity') + 
    facet_wrap(DATE ~ ., labeller=labeller(DATE = times.names)) +
    labs(title = paste0(var.name, " by stage of disease"), x = NULL, y = "Frequency") +
    scale_x_discrete(labels = c("Prevalence", "Infected", "Hospitalized", "ICU", "Dead")) +
    theme(axis.text.x = element_text(angle = 90))
  grouped.fig
}


