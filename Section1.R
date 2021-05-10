library(readxl)

spending <- read_excel("State-by-State Spending on Kids.xlsx", sheet = 1)

library(tidyr)

statepops <- read.csv("acs2017_census_tract_data.csv", header = TRUE)

statepops <- statepops %>%
  group_by(State) %>%
  summarise(popn = sum(TotalPop, na.rm = TRUE)) %>%
  filter(State != "Puerto Rico")

library(gtrendsR)
library(tidyverse)
library(usmap)

orange <- "#C9592E"

spending$fips <- fips(spending$state)

spending <- left_join(spending,statepops, by = c("state" = "State"))
spending$spending2016_percapita <- spending$`2016`/spending$popn

plot_usmap(data = spending, values = "spending2016_percapita",  color = orange, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Spending", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "State spending on education per capita", caption = "Source: @ayalamars")
