#Jason Wells
#10/30/2019
#Lab 7
library(tidyverse) 
library(maps)
library(readxl) 
library(mapproj)
library(RColorBrewer) 
library(usmap)
library(socviz) 
library(cowplot) 
organ <- read_xlsx("data/OrganDonorData.xlsx")
us_pop <- read_csv("data/state_pop.csv")
us_pop <- us_pop %>%
  select(state,pop)

organ <- merge(organ,us_pop)
us_states <- us_map(region = "states")
us_states <- us_states %>%
  rename("state" = abbr,
         "long" = x,
         "lat" = y)
organ_new <- left_join(us_states,organ,by = "state")

organ_new %>%
  ggplot(mapping = aes(x=long,
                       y=lat,
                       group = group, fill = Total)) +
  geom_polygon(color = "gray") +
  scale_color_brewer(palette = "Set1") +
  coord_equal() +
  theme_map()+
  scale_fill_distiller(palette = "Set1") +
  labs(title = "US Living Organ Donors")
#Which state appears to have the highest number of living donors?
#California looks like it has the highest number of living donors.
organ_asc <- organ_new %>%
  group_by(state) %>%
  summarize(sum = sum(Total)) %>%
  arrange(desc(sum)) %>%
  head(5)
organ_desc <- organ_new %>%
  group_by(state) %>%
  summarize(sum = sum(Total)) %>%
  arrange(sum) %>%
  head(5)
#state
#sum
#1	CA	6979602
#2	TX	5959035
#3	NY	3806500
#4	MI	2954700
#5	IL	2683870

#state
#sum
#1	DC	4710
#2	DE	24910
#3	WY	25608
#4	RI	37700
#5	VT	52000