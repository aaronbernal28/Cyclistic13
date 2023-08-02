library(tidyverse)

##########preparar##########
# uso los datos históricos de los viajes de Cyclistic para analizar e identificar tendencias de todo el 2022

tripdata_01 <- read_csv("tripdata/202201-divvy-tripdata.csv")
tripdata_02 <- read_csv("tripdata/202202-divvy-tripdata.csv")
tripdata_03 <- read_csv("tripdata/202203-divvy-tripdata.csv")
tripdata_04 <- read_csv("tripdata/202204-divvy-tripdata.csv")
tripdata_05 <- read_csv("tripdata/202205-divvy-tripdata.csv")
tripdata_06 <- read_csv("tripdata/202206-divvy-tripdata.csv")
tripdata_07 <- read_csv("tripdata/202207-divvy-tripdata.csv")
tripdata_08 <- read_csv("tripdata/202208-divvy-tripdata.csv")
tripdata_09 <- read_csv("tripdata/202209-divvy-tripdata.csv")
tripdata_10 <- read_csv("tripdata/202210-divvy-tripdata.csv")
tripdata_11 <- read_csv("tripdata/202211-divvy-tripdata.csv")
tripdata_12 <- read_csv("tripdata/202212-divvy-tripdata.csv")

str(tripdata_01)

##########procesar##########
#fusiono las 12 tablas

tripdata <- bind_rows(tripdata_01, tripdata_02, tripdata_03, tripdata_04, tripdata_05, tripdata_06, tripdata_07, tripdata_08, tripdata_09, tripdata_10, tripdata_11, tripdata_12)

#limpio el ambiente global

rm(tripdata_01, tripdata_02, tripdata_03, tripdata_04, tripdata_05, tripdata_06, tripdata_07, tripdata_08, tripdata_09, tripdata_10, tripdata_11, tripdata_12)

# creo la columna de longitud de cada viaje: ride_length, dia de la semama: day_of_week, mes del año: month_of_year

days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

tripdata <- tripdata %>% mutate(ride_length = abs(round(as.numeric(difftime(ended_at, started_at, units = "mins")),3)),
                                day_of_week = wday(started_at),
                                month_of_year = month(started_at))

# filtro las variable que podria usar

tripdata <- tripdata %>% 
  select(ride_id,
         started_at,
         start_station_name,
         end_station_name,
         member_casual,
         ride_length,
         day_of_week,
         month_of_year) %>% 
  arrange(started_at)

View(tripdata)

##########analizar##########
# obtengo datos estidisticos de ride_length

summary(tripdata)

# la media de ride_length

tripdata %>%
  summarize(mean(ride_length))

# el maximo de ride_length

tripdata %>%
  summarize(max = max(ride_length))

# el modo de day_of_week es sabado

days[(tripdata %>%
        group_by(day_of_week) %>% 
        count(day_of_week) %>% 
        arrange(n))$day_of_week[7]]

# promedio de ride_length para clientes miembros

tripdata %>% 
  filter(member_casual == "member") %>% 
  summarise(average = mean(ride_length))

# promedio de ride_length para clientes casuales

tripdata %>% 
  filter(member_casual == "casual") %>% 
  summarise(average = mean(ride_length))

# promedio de ride_length por day_of_week

tripdata %>% 
  group_by(day_of_week) %>% 
  summarise(average = mean(ride_length)) %>% 
  arrange(day_of_week)

tripdata %>% 
  filter(member_casual == "member") %>% 
  group_by(day_of_week) %>% 
  summarise(average = mean(ride_length)) %>% 
  arrange(day_of_week)

tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_week) %>% 
  summarise(average = mean(ride_length)) %>% 
  arrange(day_of_week)

average_trip <- tripdata %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise(average = mean(ride_length)) %>% 
  arrange(day_of_week)

head(average_trip)

ggplot(data = average_trip) +
  geom_col(mapping = aes(x = day_of_week, y = average, fill = member_casual)) +
  facet_wrap(~member_casual) +
  labs(y = "average ride length (mins)")

# cantidad de viajes por day_of_week (en gral, para miembros y casuales)

tripdata %>% 
  group_by(day_of_week) %>% 
  count(day_of_week) %>% 
  arrange(day_of_week)

cant_trips_member <- tripdata %>%
  filter(member_casual == "member") %>% 
  group_by(day_of_week) %>% 
  count(day_of_week) %>% 
  arrange(day_of_week)

cant_trips_casual <- tripdata %>%
  filter(member_casual == "casual") %>%
  group_by(day_of_week) %>% 
  count(day_of_week) %>% 
  arrange(day_of_week)

cant_trips <- data.frame(day_of_week = cant_trips_member$day_of_week, member = cant_trips_member$n, casual = cant_trips_casual$n)
str(cant_trips)

#grafico la comparacion de cantidad de viajes entre los clintes casuales y mienbros

cant_trips <- tripdata %>% 
  group_by(day_of_week, member_casual) %>% 
  count(day_of_week) %>% 
  arrange(day_of_week)

ggplot(data = cant_trips) +
  geom_col(mapping = aes(x = day_of_week, y = n, fill = member_casual)) +
  facet_wrap(~member_casual) +
  labs(y = "cant_trips_per_day")
