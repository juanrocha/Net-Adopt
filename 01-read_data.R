library(tidyverse)
library(fs)


fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/BRAZIL_SOY_2.5.1_pc/")

fls |> str_subset(".csv")

dat <- read_csv(file = str_subset(fls, ".csv")) |>
    janitor::clean_names()

skimr::skim(dat)
dat

dat |>
    pull(municipality) |>
    unique() |> length()


dat |> pull(trase_geocode) |> unique()

dat |>
    select(year, exporter, importer, country)

dat |> pull(exporter) |> unique() |> length()
dat |> pull(importer) |> unique() |> length()
dat |> pull(country) |> unique() |> length()

dat |>
    ggplot(aes(land_use_ha)) +
    geom_density()+
    scale_x_log10()

dat |>
    filter(year == 2018) |>
    filter(zero_deforestation_brazil_soy == "COMPANY COMMITMENT") |>
    group_by(exporter,importer) |>
    summarize(n = n())

dat |> pull(zero_deforestation_brazil_soy) |> unique()
