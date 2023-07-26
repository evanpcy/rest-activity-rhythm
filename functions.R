library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(RAR)
library(nparACT)

ReadRawData <- function(file){
  firstLine <- 
    "\"Line\",\"Date\",\"Time\",\"Off-Wrist Status\",\"Activity\",\"Marker\",\"White Light\",\"Red Light\",\"Green Light\",\"Blue Light\",\"Sleep/Wake\","
  content <- readLines(file)
  dataLines <- content[charmatch(firstLine, content):length(content)]
  df <- read.csv(textConnection(dataLines[-2]), header = TRUE)
  output <- df %$%
    paste(Date, Time) %>%
    dmy_hms() %>%
    cbind(df[5])
  colnames(output) <- c("Datetime", "Activity")
  return(output)
}

ReadData <- function(file){
  df <- read.csv(file)
  df$Datetime <- as.POSIXct(df$Datetime, format = "%Y-%m-%d %H:%M:%S")
  return(df)
}

WriteData <- function(df, file){
  write.csv(df, file, row.names = FALSE, na = "NaN")
}

BulkExtract <- function(dir, subdir = "extracted"){
  if(!dir.exists(dir)){
    stop("Directory does not exist.")
  }
  output_dir <- file.path(dir, subdir)
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  for(i in list.files(dir, pattern = ".csv")){
    df <- ReadRawData(file.path(dir, i))
    WriteData(df, file.path(output_dir, i))
  }
}

PlotZeros <- function(df, cutoff = 59) {
  # Plot and print periods with continuous zero activities over cutoff.
  
  # Get the variable name of the argument.
  title <- deparse(substitute(df))
  
  df$Datetime <- as.POSIXct(df$Datetime)
  
  # Create vector which highlights zero passing cutoff
  highlight <- rep("normal", nrow(df))
  periods <- df[, 2] %>%
    rle() %>%
    with({
      ok <- values == 0 & lengths > cutoff
      ends <- cumsum(lengths)
      starts <- ends - lengths + 1
      data.frame(starts, ends)[ok,]
    })
  
  if (nrow(periods) != 0) {
    for (row in 1:nrow(periods)) {
      highlight[periods[row, 1]:periods[row, 2]] <- "suspect"  # change suspect periods
      paste(df[periods[row, 1], 1], "to", df[periods[row, 2], 1],
            "Duration:", periods[row, 2]-periods[row, 1] + 1,
            "Start:", periods[row, 1],
            "End:", periods[row, 2]) %>%
        print()
    }
  } else print("No period satisfy the requirement.")
  
  highlight %>%
    cbind(df, .) %>%
    qplot(x = Datetime, y = Activity,
          data = ., ylab = "Activity",
          main = title, color = highlight) +
    scale_color_manual(values = c("#000000", "#FF5733"))
}

PlotNaN <- function(df) {
  # Plot and print all NaN data.
  
  # Get the variable name of the argument.
  title <- deparse(substitute(df))
  
  MarkNaN <- function(activities) {
    # Change NaN to -1 for better visualization.
    activities[is.nan(activities)] <- -1
    return(activities)
  }
  
  # Print NaN periods with corresponding duration, start and end time.
  if(any(is.nan(df[, 2]))) {
    periods <- df %>%
      pull(2) %>%
      is.nan() %>%
      rle() %>%
      with({
        ends <- cumsum(lengths)
        starts <- ends - lengths + 1
        data.frame(starts, ends)[values, ]})
    for (row in 1:nrow(periods)) {
      paste(df[periods[row, 1], 1], "to", df[periods[row, 2], 1],
            "Duration:", periods[row, 2] - periods[row, 1] + 1,
            "Start:", periods[row, 1],
            "End:", periods[row, 2]) %>%
        print()
    }
  } else print("No NaN in the data frame.")
  
  # Form new data frame with marked activities.
  df.NaN <- df %>%
    {MarkNaN(.[, 2])} %>%
    cbind(df, NaN_activity = .)
  
  qplot(x = Datetime, y = NaN_activity,
        data = df.NaN, ylab = "Activity",
        main = title, color = NaN_activity < 0) +
    scale_color_manual(values = c("#000000", "#FF5733"))
}

CheckInterval <- function(df, interval = 1440, cutoff = 180){
  # Check if any successive interval has an no. of NaN >= cutoff.
  # 30s epoch: interval = 2880, cutoff = 360
  # Default: Any 24-hour interval has more than or exactly 180 NaNs.
  list <- df[,2]
  if (length(list) > interval) {
    for (i in 1:(length(list) - interval + 1)) {
      checking <- list[i:(i + interval - 1)]
      if (sum(is.nan(checking)) >= cutoff) return(i)
    }
    return(FALSE)
  } else {
    return(sum(is.nan(list)) >= cutoff)
  }
}

CheckNDays <- function(df){
  return(nrow(df)/60/24)
}