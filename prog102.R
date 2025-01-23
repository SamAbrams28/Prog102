library(marinecs100b)

# Extracting data ---------------------------------------------------------

# What would you pick for the temperature extraction function name and parameter
# names?
# temp_extraction(site)

# Writing extraction functions --------------------------------------------

# Put all of datetime into a format R recognizes as a date time, this is helpful
# for everything
datetime_obj <- as.POSIXct(kefj_datetime, tz = "Etc/GMT+8")

# Function for converting strings into times in Alaskan standard time
alaskan_time_conversion <- function(string){
  result <- as.POSIXct(string, tz = "Etc/GMT+8")
  return(result)
}


# Write your temperature extraction function here

temp_extraction <- function (site, start, end) {

  # Put the date inputs into datetime format
  starttime <- alaskan_time_conversion(start)
  endtime <- alaskan_time_conversion(end)

  # Pull out the temperatures at the site in the range
  temps <- kefj_temperature[datetime_obj >= starttime & datetime_obj <= endtime
                            & kefj_site == site]

  return(temps)
}

temp_aialik <- temp_extraction("Aialik", "2018-06-01", "2018-06-03")

# Write your exposure extraction function here
exposure_extraction <- function (site, start, end) {

  # Put the date inputs into datetime format
  starttime <- alaskan_time_conversion(start)
  endtime <- alaskan_time_conversion(end)

  # Pull out the times at the site in the range
  exposures <- kefj_exposure[datetime_obj >= starttime & datetime_obj <= endtime
                             & kefj_site == site]

  return(exposures)
}
exposure_aialik <- exposure_extraction("Aialik", "2018-07-01","2018-07-03")


# Write your datetime extraction function here
time_extraction <- function (site, start, end) {

  # Put the date inputs into datetime format
  starttime <- alaskan_time_conversion(start)
  endtime <- alaskan_time_conversion(end)

  # Pull out the times at the site in the range
  times <- datetime_obj[datetime_obj >= starttime & datetime_obj <= endtime
                        & kefj_site == site]

  return(times)
}
time_aialik <- time_extraction("Aialik", "2018-07-01", "2018-07-03")
print(time_aialik)

# Visualize Nuka Pass temperatures on 2018-07-01
print(time_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"),
      temp_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"),
      exposure_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"))

plot_kefj(time_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"),
          temp_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"),
          exposure_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"))

print(time_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"))
print(temp_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"))
print(exposure_extraction("Nuka_Pass", "2018-07-01", "2018-07-02"))

# How does the visualization code above compare to what you wrote in PROG101? In
# prog101, we looked for the highest and lowest temps. Now, we can make
# exposure/temperature over time plots for any date range we want. This code is
# simpler and more compartmentalized.

# Functions within functions (optional) -----------------------------------

# How would you parameterize the visualization code into its own function?
# Specifically, what would you call that function and its parameters?

# Write your wrapper function for plot_kefj(). Call it to compare the
# visualizations for Nuka Pass on July 1, 2018 versus January 1, 2018.

day_plot <- function (site, date){

  # Put the date input into
  date <- alaskan_time_conversion(date)
  date <- format(date, "%Y-%m-%d")

  # Pull out just the days so we can search for all times from one day
  day <- format(datetime_obj, "%Y-%m-%d")

  # Pull out the data needed for plotting at the given parameters
  times <- datetime_obj[day == date & kefj_site == site] # Datetimes
  temps <- kefj_temperature[day == date & kefj_site == site] # Temperatures
  exposures <- kefj_exposure[day == date & kefj_site == site] # Exposures

  return(plot_kefj(times, temps, exposures))
}

# Visualize each day
day_plot("Nuka_Pass","2018-01-03")
day_plot("Nuka_Pass","2018-07-03")

range_plot <- function (site, start, end){
  # Put the date inputs into datetime format
  starttime <- alaskan_time_conversion(start)
  endtime <- alaskan_time_conversion(end)

  # Pull out the data needed for plotting at the given parameters
  times <- datetime_obj[datetime_obj >= starttime & datetime_obj <= endtime &
                          kefj_site == site] # Datetimes
  temps <- kefj_temperature[datetime_obj >= starttime &
                              datetime_obj <= endtime &
                            kefj_site == site] # Temperatures
  exposures <- kefj_exposure[datetime_obj >= starttime &
                               datetime_obj <= endtime &
                               kefj_site == site] # Exposures

  return(plot_kefj(times, temps, exposures))
}

range_plot("Nuka_Pass", "2018-08-01", "2018-08-03")


