#! /usr/bin/env Rscript

## File description -------------------------------------------------------------
## Visualizations of my toggl data, because I'm curious how I live my life
##
## author: kriss1@stanford.edu

## ---- libraries ----
library("data.table")
library("plyr")
library("dplyr")
library("ggplot2")
library("lubridate")

## ---- read-data ----
entries <- fread("Toggl_time_entries_2016-12-23_to_2017-02-06.csv")

## ---- cleaning ----
new_names <- make.names(colnames(entries)) %>%
  tolower()
new_names <- gsub("\\.", "_", new_names)
setnames(entries, new_names)

convert_times <- function(dates, times) {
  datetimes <- paste(dates, times)
  ymd_hms(datetimes)
}

entries$start<- convert_times(
  entries$start_date,
  entries$start_time
)

entries$end<- convert_times(
  entries$end_date,
  entries$end_time
)

entries$duration <- difftime(
  entries$end,
  entries$start,
  units = "mins"
) %>%
  as.numeric()

entries <- entries %>%
  select(project, description, start, end, duration, tags)

## ---- work-intensity ----
## How many hours am I logging each day?
day_sums <- entries %>%
  mutate(date = date(start)) %>%
  group_by(date) %>%
  summarise(sum = sum(duration))

ggplot(day_sums) +
  geom_histogram(aes(x = sum), bins = 20)

## ---- attention-span ----

## ---- frazzledness ----

## ---- tag-intensity ----

## ---- hours-working ----


## ---- tag-hours ----
