#a
library(dplyr)
library(readr)
library(lubridate)
file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
year <- "2023"
tail <- ".txt.gz&dir=data/historical/stdmet/"
path <- paste0(file_root, year, tail)
header <- read_lines(path, n_max = 1)
buoy <- read_table(path, skip = 2, col_names = c("YY", "MM", "DD", "hh", "mm", "WDIR", "WSPD","GST","WVHT","DPD","APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE"))
buoy <- buoy %>%
  mutate(Year = as.integer(YY),
         Month = as.integer(MM),
         Day = as.integer(DD),
         Hour = as.integer(hh),
         Minute = as.integer(mm),
         Date = make_datetime(Year, Month, Day, Hour, Minute))
head(buoy)

#b
library(dplyr)
buoy1 <- buoy %>%
  mutate(across(where(is.numeric), ~na_if(., 999)))
head(buoy1)
na_count <- sapply(buoy, function(x) sum(is.na(x)))
print(na_count)
library(ggplot2)
library(naniar)
vis_miss(buoy, warn_large_data = FALSE)
#As a bonus part, it indicates that the lost data pattern may be related to an external event, such as a government shutdown or budget change. Investigating these periods gives an idea of whether certain years or months are associated with these events. Data sources can also be added to cross-check against missing data periods.

#c
library(ggplot2)
library(dplyr)
annual_data <- buoy %>%
  group_by(Year = year(Date)) %>%
  summarize(
    Mean_ATMP = mean(ATMP, na.rm = TRUE),
    Mean_WTMP = mean(WTMP, na.rm = TRUE),
    Mean_WSPD = mean(WSPD, na.rm = TRUE)
  )
ggplot(annual_data, aes(x = Year)) +
  geom_point(aes(y = Mean_ATMP, color = "Air Temperature")) +
  geom_point(aes(y = Mean_WTMP, color = "Water Temperature")) +
  geom_line(aes(y = Mean_ATMP, color = "Air Temperature")) +
  geom_line(aes(y = Mean_WTMP, color = "Water Temperature")) +
  labs(title = "Annual Mean Air and Water Temperature",
       y = "Temperature (°C)",
       color = "Parameter") +
  theme_minimal()


#d
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

rainfall_data <- read_csv("Rainfall.csv")
rainfall_data <- rainfall_data %>%
  mutate(DATE = ymd(DATE))
rainfall_data <- rainfall_data %>%
  filter(year(DATE) >= 1985, year(DATE) <= 2013)
rainfall_stats <- rainfall_data %>%
  summarize(
    Total_Days = n(),
    Rain_Days = sum(HPCP > 0, na.rm = TRUE),
    No_Rain_Days = sum(HPCP == 0, na.rm = TRUE),
    Avg_Rainfall = mean(HPCP, na.rm = TRUE),
    Max_Rainfall = max(HPCP, na.rm = TRUE)
  )

print(rainfall_stats)
ggplot(rainfall_data, aes(x = DATE, y = HPCP)) +
  geom_line() +
  labs(title = "Daily Rainfall Over Time", x = "Year", y = "Rainfall (inches)")