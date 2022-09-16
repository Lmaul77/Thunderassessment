library(tidyverse)
library(sp)
library(scales)

#Working through this technical assessment my skillset involved Visual Studio Code
#and working with SQL Data. I have spent the last week studying and learning how to program
#with Rstudio to analyze data. Multiple attempts were made to define shot zones utilizing
#greater than, less than, equal to, or, and & operators. I kept running into road blocks with the
#three point arc line. I spent around 4-5 hours researching for a way to define the distance from
#one point to another on a grid in Rstudio. The resulting research did not lead to any outcomes 
#that I could make work for this dataset. I tried creating specific arches from one point to
#another, I tried utilizing the euclidean distance method to define the distance from the hoop
#to the arc. Below is a solution provided by a Taylor Kinney on Rpubs.com, I am going to go through
#the following solution and define how the solution was provided. I do not take credit for this 
#solution but, considering all of my attempts failed I wanted to at least provide that I understand
#how this solution was created. I would be more then happy to provide all of my attempts and
#research throughout this technical assessment if we move past this technical assessment.

assessment <- shots_data
data <- assessment

#Function to define a circle by defining a center point on the grid(the hoop), a sequence
#generation, and finally returning a data_frame of the circle.

circle = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

#Variable to define the circle arc of the three point line while regarding the corner line.

threepoint = circle(center = c(0, 0), 
                     radius = 23.75) %>%
  filter(y >= 7.8)

#Variable to define threepointline to reference corner three lines.

threepointline = data_frame(
  x = c(22, 
        22, 
        threepoint$x,  
        -22, 
        -22),
  y = c(min(data$y), 
        22, 
        threepoint$y, 
        22, 
        min(data$y)))

#Mutating original data to create shotzones in reference to the variables above(definition of court)
#point.in.polygon is able to factor if a given point lies within the given polygon variables.
#Shotzones defines a case point from the arc variables to ultimately give the specific shotzones of
#the court. tpm is created to select the specific data from the table to provide for the ggplot.

data <- data %>% 
  mutate(inthreearc = factor(point.in.polygon(data$x,data$y,threepointline$x,threepointline$y)),
         inthreearc = factor(replace(inthreearc, inthreearc == 2:3, 1)),
         incorner = factor(if_else(inthreearc == 0 & y <= 7.8, 1, 0)),
         shotzones = case_when(inthreearc == 1 & incorner == 0 ~ "2PT",
                               inthreearc == 0 & incorner == 0 ~ "NC3",
                               inthreearc == 0 & incorner == 1 ~ "C3"),
         tpm = if_else(fgmade == 1 & shotzones == c("NC3", "C3"), 1, 0)) %>% 
  select(team:fgmade,tpm,inthreearc:shotzones)

#ggplot to show defined shotzones.

ggplot(data,aes(x,y, color = shotzones)) +
  geom_point()

#Variable to seperate the shotzones and teams then mutate them into a percentage.

shotdistribution <- data %>% 
  group_by(team,shotzones) %>% 
  summarise(N = n()) %>%
  mutate(pct = N / sum(N)) %>% 
  arrange(desc(pct))

#Creation of theme to show shotdistrubtion.

teamtheme <- theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.border = element_blank(),
  panel.grid=element_blank(),
  axis.ticks = element_blank(),
  plot.title=element_text(size=14, face="bold"))

#Plotting the shotdistribution data on the created theme.

ggplot(shotdistribution, aes(x = "", y = pct, fill = shotzones)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  teamtheme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = pct, label = percent(pct)),
            position = position_stack(vjust = 0.5), size=5) +
  facet_wrap(vars(team))

#Variable to group the teams and shotzones to define the efg(effective field goal percentage).

efg <- data %>% 
  group_by(team,shotzones) %>% 
  summarise(fgm = sum(fgmade),
            fga = n(),
            threes = sum(tpm)) %>% 
  mutate(efg = (fgm + (0.5 * threes))/fga) %>% 
  arrange(desc(efg))

#Variable plotted on chart.

ggplot(efg, aes(shotzones,efg,fill = team)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_flip() +
  geom_text(aes(y = efg, label = percent(efg)),
            position = position_dodge(width = 1), size = 5) +
  scale_x_discrete(limits = c("C3","2PT","NC3"))