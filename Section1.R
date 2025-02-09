library(readxl)
library(tidyr)
library(tidyverse)
library(usmap)

##########################
spending <- read_excel("State-by-State Spending on Kids.xlsx", sheet = 1)
statepops <- read.csv("acs2017_census_tract_data.csv", header = TRUE)

statepops <- statepops %>%
  group_by(State) %>%
  summarise(popn = sum(TotalPop, na.rm = TRUE)) %>%
  filter(State != "Puerto Rico")

orange <- "#C9592E"
blue <- "#0000CD"
spending$fips <- fips(spending$state) # get geo IDs

spending <- left_join(spending,statepops, by = c("state" = "State"))
spending$spending2016_percapita <- spending$`2016`/spending$popn # normalize

plot_usmap(data = spending, values = "spending2016_percapita",  color = orange, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Spending", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "State spending on education per capita", caption = "Source: @ayalamars")

##########################
retention <- read_excel("Retention-in-Grade-12.xlsx", sheet = 2)
library(tidyr)
ret_tidy<- gather(retention, demographic_feature, proportions, 2:16)


ggplot(data=retention, aes(x=state, y=native_per)) +
  geom_bar(stat="identity") +
  coord_flip()

ret_states <- retention[-1,] # remove totals for easy plotting
ret_states$fips <- fips(ret_states$state)
library(tidyr)
library(dplyr)
ret_states <- left_join(ret_states,statepops, by = c("state" = "State"))
ret_states$HispRetpercapita <- as.numeric(ret_states$hispanic_num)/ret_states$popn # normalize
plot_usmap(data = ret_states, values = "hispanic_per",  color = blue, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = blue, 
                         name = "Retention", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Hispanic or Latino Student retention", caption = "Source: @ayalamars")

##########################
calc_states<- read_excel("Enrollment-in-Calculus.xlsx", sheet = 4)

calc_states$fips <- fips(calc_states$state)# geo tags

calc_states<- left_join(calc_states,statepops, by = c("state" = "State"))
calc_states$enrollpercap<- as.numeric(calc_states$black_num)/calc_states$popn # normalize

green <- "#228B22"

plot_usmap(data = calc_states, values = "enrollpercap",  color = green, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = green, 
                         name = "Enrollment", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Enrollment in Calculus for Black Females", caption = "Source: @ayalamars")
