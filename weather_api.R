# add in weather data from openweather API
require(jsonlite)
require(dplyr)
require(tidyr)

# empty data frame to start collecting the data
hourly_comb <- data.frame(matrix(ncol = 3, nrow = 0))
names(hourly_comb2) <- c("dt", "main", "description")

lat = 40.77
lon = -73.97
# this must be as.numeric from datetime
my_dt = as.numeric(as.POSIXct(strftime(Sys.Date()-0)))
API_key = "36bf32cbd519415a6dc9fff766fb6c16"

# for loop from 0 (yesterday) to 4 (5 days ago)

for (i in 0:5) {
  
  # changing datetime
  new_dt <- as.numeric(my_dt - i * 86400)
  
  url <- paste0("http://api.openweathermap.org/data/2.5/onecall/timemachine?lat=",lat
                , "&lon=", lon
                , "&dt=", new_dt
                , "&appid=", API_key)
  ow <- fromJSON(url)
  
  # only historical hourly data
  hourly <- ow$hourly
  hourly$dt <- as.POSIXct(hourly$dt, origin = "1970-01-01", tz = 'EST')
  hourly <- hourly %>% unnest(weather)
  hourly <- hourly[c("dt", "main", "description")]
  
  # combination of all hourly data
  hourly_comb <- rbind(hourly_comb, hourly)
  
}

# in case there is two events in same dt
hourly_comb <- hourly_comb %>%
  group_by(dt) %>%
  mutate(
    main = paste(main, collapse = " | "),
    description = paste(description, collapse = " | ")
  ) %>%
  distinct(dt, .keep_all = T)

write.csv(hourly_comb, "data/weather2.csv")
