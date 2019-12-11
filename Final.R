library(ggplot2)
library(tidyverse)
library(dplyr)
library(corrplot)
library(readr)

epa.cars <- read_csv("big_epa_cars.csv")

epa.cars <- epa.cars%>%
  mutate(
    barrels = barrels08,
    fuel_type = fuelType1,
    mpg = comb08,
    c02 = co2,
    fuel_cost = fuelCost08
  ) %>%
  
  select(
    barrels,
    fuel_type,
    mpg,
    c02,
    fuel_cost,
    drive,
    make,
    model,
    year,
    VClass
  )

corrplot(cor(select(epa.cars,
                    year,
                    barrels,
                    mpg,
                    c02,
                    fuel_cost
                    ), 
             use="complete.obs"))






makefrequency <- aggregate(epa.cars$mpg, 
                           by = list(epa.cars$make),
                           length)
colnames(makefrequency) <- c("Make", "Frequency")

makefrequency <- makefrequency[order(makefrequency$Frequency, decreasing = TRUE),]
makefrequency <- makefrequency[1:15,]

pie(makefrequency$Frequency, labels = makefrequency$Make)






top5makes <- subset(epa.cars, 
                    epa.cars$make %in%
                      c(
                        "Chevrolet",
                        "Ford",
                        "Dodge",
                        "GMC",
                        "Toyota"
                      ))

medianmpg <- aggregate(top5makes$mpg,
                       by = list(top5makes$make, top5makes$year),
                       median)
colnames(medianmpg) <- c("make", "year", "median_mpg")

line <- ggplot(medianmpg, aes(x = year, y = median_mpg, group = make, col = make)) + 
  geom_line(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))
ggplotly(line)








ggplot(epa.cars, aes(x=fuel_type)) + 
  geom_bar()




gasoline <- subset(epa.cars, 
                    epa.cars$fuel_type %in%
                      c(
                        "Regular Gasoline",
                        "Premium Gasoline"
                      ))

ggplot(gasoline, aes(x=fuel_type, y=mpg)) + 
  geom_boxplot()


avgs <- aggregate(gasoline$mpg,
          by = list(gasoline$fuel_type),
          mean)
colnames(avgs) <- c("Fuel_Type", "Mean MPG")
avgs

summary(aov(gasoline$mpg ~ gasoline$fuel_type))






