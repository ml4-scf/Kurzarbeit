library(tidyverse)
library(gganimate)
library(extrafont)


loadfonts(device="win")

# Turn off scientific notation
options(scipen=999)

# Loading data, not available in one excel --> processing and merging data
# (SOURCE: https://statistik.arbeitsagentur.de/nn_32018/SiteGlobals/Forms/Rubrikensuche/Rubrikensuche_Form.html?view=processForm&resourceId=210368&input_=&pageLocale=de&topicId=17558&year_month=202003&year_month.GROUP=1&search=Suchen)

# Most recent data (March 2020, April 2020), will be updated permanently
kug_0 <- readxl::read_excel("data\\kurzarbeit-d-0-202004-xlsx.xlsx", sheet = "Tab-04-Pers-ZR", range = "B55:C56", col_names = c("Monat","Personen"))
kug_0$Monat <- str_sub(kug_0$Monat, 1, 10)
kug_0$Monat <- lubridate::mdy(kug_0$Monat)

# February 2020 - September 2016
kug_1 <- readxl::read_excel("data\\kurzarbeit-d-0-202003-xlsx.xlsx", sheet = "Tab-04-Pers-ZR", range = "B15:C56", col_names = c("Monat","Personen"))
kug_1$Monat <- lubridate::mdy(kug_1$Monat)

# August 2016 - April 2013
kug_2 <- readxl::read_excel("data\\kurzarbeit-d-0-201609-xls.xls", sheet = "Tab-04-Pers-ZR", range = "B15:C55", col_names = c("Monat","Personen"))
kug_2$Monat <- lubridate::mdy(kug_2$Monat)

# March 2013 - December 2009
kug_3 <- readxl::read_excel("data\\kurzarbeit-d-0-201304-xls.xls", sheet = "Tab-04-Pers-ZR", range = "B15:C54", col_names = c("Monat","Personen"))
kug_3$Monat <- lubridate::mdy(kug_3$Monat)

# November 2009 - January 2007
kug_4 <- readxl::read_excel("data\\kurzarbeit-d-0-200912-xls.xls", sheet = "Tab-04-Pers-ZR", range = "B15:C49", col_names = c("Monat","Personen"))
kug_4$Monat <- lubridate::mdy(kug_4$Monat)

# December 2006 - January 2003
kug_5 <- readxl::read_excel("data\\kurzarbeit-d-0-200701-xls.xls", sheet = "Seite 4-5", range = "F22:F72", col_names = c("Personen"))
kug_5 <- drop_na(kug_5)

# adding date column for kug_5
start_date <- as.Date("2003-01-20")
kug_5$Monat <- seq(start_date, by = "month", length.out = 48)

# joining data sets
kug <- list(
  kug_0,
  kug_1,
  kug_2,
  kug_3,
  kug_4,
  kug_5
) %>% reduce(full_join)

kug <- kug %>%
  arrange(Monat)


# save data as csv for later purpose
write_csv(kug, path = "data\\Kug.csv")

# create plot

kug <- kug %>%
  mutate(ind = ifelse(Monat==max(Monat),Monat+months(24),Monat))

p <- ggplot(kug, aes(x=Monat,y=Personen))+
  geom_line(color="aquamarine3", size=1.5)+
  geom_point(color="darkorange2", size=2)+
  labs(x="",y="",title= "In Kurzarbeit angezeigte Personen", subtitle = "Stand 30.4.2020", caption = "Quelle: Bundesagentur für Arbeit")+
  theme(text = element_text(color = "ghostwhite", family = "sans"),
        axis.text = element_text(color = "slategray2"),
        axis.ticks = element_line(color="slategray2"),
        axis.line.x = element_line(color = "slategray2"),
        axis.line.y = element_line(color = "slategray2"),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color="slategray2", linetype = "dotted"),
        panel.background= element_rect(fill="gray40"),
        plot.background = element_rect(fill = "gray40", color = "ghostwhite"),
        plot.title = element_text(face="bold", size = 18))

p

# animate plot
animation <- p +  view_follow()+
                  transition_reveal(ind)

animation <-  animate(animation, end_pause = 30, nframes=250)

anim_save("Kug_animation.gif",animation = animation,path = "output")

#Test themes and viz

theme_set(theme_classic())

ggplot(kug, aes(x=Monat,y=Personen))+
  geom_line(color="blue")+
  geom_point(color="red", size = 2)+
  labs(x="",y="",title="Total number of registered short-time workers per month", caption = "@laym1904: Source Bundesagentur für Arbeit" )+
  annotate(geom = "curve", xend=as.Date("2020-03-20"),  yend=1043892, x=as.Date("2015-05-20"),y=500000,curvature = -0.5, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom="text",  x=as.Date("2015-05-20"),y=450000, label="March 2020: 1.043.892" )+
  theme(
    text = element_text(color = "white", family = "mono"),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color="white"),
    axis.line.x = element_line(color = "lightgrey"),
    axis.line.y = element_line(color = "lightgrey"),
    panel.grid.major.y = element_line(color="lightgrey", linetype = "dotted"),
    panel.background= element_rect(fill="gray20"),
    plot.background = element_rect(fill = "gray20", color = "white"),
    plot.title = element_text(face="bold", size = 18))


# Ribbon chart

#  Need:
#   - column on registered Kurzarbeit (check)
#   - column on realized Kurzarbeit (can be easily retreived)
#   - column indicating whether realized or registered is higher ? --> as input for ymax (value of higher type) and ymin (value of lower type)
#         --> the latter maybe feasible via an ifelse statement(see below direct ifesle statement probably the ebst way), is always between 2 lines
#
# Example:
#         ggplot(data, aes(x=Monat))+
#           geom_line(aes(y=registered))+
#           geom_line(aes(y=realized))+
#           geom_ribbon(aes(ymin=ifelse(registered>realized,registered,realized),ymax=ifelse(registered<realized,registered,realized)))
#


ggplot(kug, aes(x=Monat,y=Personen))+
  geom_ribbon(fill="blue",aes(ymin=0,ymax=Personen))+
  scale_y_continuous()
