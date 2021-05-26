library(randomForest)
library(dplyr)
library(ggplot2)
library(naniar)
library(tidyr)

#df <- read.csv("CRDC 2015-16 School Data.csv", header = TRUE)

# dftrunc <- df %>%
#   select(2,1778,1796:1828) %>%
#   replace_with_na_all(condition = ~.x < 0) %>%
#   drop_na(TOT_RET_G12_M) %>%
#   drop_na()
#   # group_by(LEA_STATE_NAME) %>%
#   # mutate(stateret = mean(TOT_RET_G12_M, na.rm = TRUE))
#write.csv(dftrunc, "TDI proposal/dftrunc.csv", row.names = F)

dftrunc <- read.csv("TDI proposal/dftrunc.csv", header = TRUE)
set.seed(99)
train <- sample(nrow(dftrunc), 0.7*nrow(dftrunc), replace = FALSE)
TrainSet <- dftrunc[train,]
ValidSet <- dftrunc[-train,]

model1 <- randomForest(TOT_RET_G12_M ~ ., data = TrainSet, importance = TRUE)

model2 <- randomForest(TOT_RET_G12_M ~ ., data = TrainSet, 
                       ntree = 500, mtry = 6, importance = TRUE)

save(model2,file = "model2.RData")
# Predicting on train set
predTrain <- predict(model2, TrainSet)
# Checking classification accuracy
table(predTrain, TrainSet$TOT_RET_G12_M)  

predValid <- predict(model2, ValidSet)
# Checking classification accuracy
mean(predValid == ValidSet$TOT_RET_G12_M)                    
table(predValid,ValidSet$TOT_RET_G12_M)

# a=c()
# i=5
# for (i in 3:8) {
#   model3 <- randomForest(TOT_RET_G12_M ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
#   predValid <- predict(model3, ValidSet, type = "class")
#   a[i-2] = mean(predValid == ValidSet$TOT_RET_G12_M)
# }
# a
# plot(3:8,a)


model3 <- randomForest(TOT_RET_G12_M ~ ., data = TrainSet, 
                       ntree = 500, mtry = 3, importance = TRUE)
varImpPlot(model3, n.var = 5)
save(model3,file = "TDI proposal/model3.RData")

# save(model2,file = "TDI proposal/rfmodel2.RData")

## make more geo plots
dftrunc.sum <- dftrunc %>%
  group_by(LEA_STATE_NAME) %>%
  summarise(state_mean = mean(TOT_RET_G12_M, na.rm = T))

statepops$fips <- fips(statepops$State)
dftrunc.sum$fips <- fips(dftrunc.sum$LEA_STATE_NAME)

dftrunc.sum <- inner_join(dftrunc.sum, statepops, by="fips")
dftrunc.sum$state_mean_n <- dftrunc.sum$state_mean/dftrunc.sum$popn
orange <- "#C9592E"


plot_usmap(data = dftrunc.sum, values = "state_mean_n",  color = orange, labels=FALSE) + 
  scale_fill_continuous( low = "white", high = orange, 
                         name = "Retention per capita", label = scales::comma
  ) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Grade 12 Retention for Males", caption = "Source: @ayalamars")