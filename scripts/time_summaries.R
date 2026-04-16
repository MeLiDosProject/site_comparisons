time_median <- function(datetime){
  datetime |> 
    LightLogR:::datetime_to_circular() |> 
    median(na.rm = TRUE) |> 
    LightLogR:::circular_to_hms() |> 
    hms::round_hms(1)
}

nighttime_p25 <- function(datetime){
  morning <- hms::as_hms(datetime) < 12*60*60
  date(datetime) <- ifelse(morning, as.Date("2000-01-02"), as.Date("2000-01-01")) |> as.Date()
  datetime |> 
    quantile(0.25, na.rm = TRUE) |> 
    hms::as_hms() |> hms::round_hms(1)
}

nighttime_p75 <- function(datetime){
  morning <- hms::as_hms(datetime) < 12*60*60
  date(datetime) <- ifelse(morning, as.Date("2000-01-02"), as.Date("2000-01-01")) |> as.Date()
  datetime |> 
    quantile(0.75, na.rm = TRUE) |> 
    hms::as_hms() |> hms::round_hms(1)
}

daytime_p25 <- function(datetime){
  hms::as_hms(datetime) |> quantile(0.25, na.rm = TRUE) |> hms::as_hms() |> hms::round_hms(1)
}

daytime_p75 <- function(datetime){
  hms::as_hms(datetime) |> quantile(0.75, na.rm = TRUE) |> hms::as_hms() |> hms::round_hms(1)
}
