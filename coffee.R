library(tidyverse)
library(fs)
library(tictoc)

fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/COLOMBIA_COFFEE_1.0.3_pc/")
fls |> str_subset(".csv")
dat <- read_csv(file = str_subset(fls, ".csv")) |> 
    janitor::clean_names()

dat |> 
    filter(department %in% c("CUNDINAMARCA", "SANTANDER")) |> 
    select(exporter) |> 
    unique()
