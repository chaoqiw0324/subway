library(tidyverse)
library(lubridate)

passenger <- read.csv("subway_passenger/data/passenger_time1.csv") %>% 
  mutate(
    date =ymd(date)
  )


nrow(passenger)/4*3

passenger1 <- passenger[1:700430,]
passenger2 <- passenger[700431:1400861,]
passenger3 <- passenger[1400862:2101292,]
passenger4 <- passenger[2101293:2801722,]

write_csv(passenger1,"subway_passenger/data/passenger_timeI.csv")
write_csv(passenger2,"subway_passenger/data/passenger_timeII.csv")
write_csv(passenger3,"subway_passenger/data/passenger_timeIII.csv")
write_csv(passenger4,"subway_passenger/data/passenger_timeIV.csv")


a<- rbind(passenger1,passenger2,passenger3,passenger4)

dplyr::all_equal(a,passenger)