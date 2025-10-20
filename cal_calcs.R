cal_calc <- function(this_yr){
  start_date <- lubridate::make_date(year = this_yr, month = 1, day = 1)
  end_date <- lubridate::make_date(year = this_yr, month = 12, day = 31)
  yr_dates <- seq(from = start_date, to = end_date, by = 1)
  wk_date <- data.frame(day_of_yr = seq(1:length(yr_dates)),
                        week_date = yr_dates,
                        epiwk = epiweek(yr_dates),
                        weekday = wday(yr_dates, 
                                       label = TRUE, 
                                       abbr = TRUE))
  return(wk_date)
}

df_cal2025 <- as_tibble(cal_calc(2025))
write.csv(df_cal2025,"C:/Users/chuck/LocalFiles/df_cal2025.csv", row.names = FALSE)
