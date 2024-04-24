library(tidyverse)
library(fs)


fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/BRAZIL_SOY_2.6.0_pc/")

fls |> str_subset(".csv")
dat <- read_csv(file = str_subset(fls, ".csv")) |>
    janitor::clean_names()
skimr::skim(dat)
dat

## Extracting data files for Angela to work on TERGMs from MPNET.
# Data files needed:
# Network matrix of exporters (A) and municipalities (B) for 2020 
# Network matrix of exporters and municipalities for 2018

# J240404: actors need to be the same across years

m2020 <- dat |> 
    filter(year == 2020) |> 
    select(municipality_of_production, exporter) |> 
    arrange(exporter) |> 
    filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
    unique() |> 
    add_column(n = 1) |> 
    pivot_wider(names_from = exporter, values_from = n, values_fill = 0) |> 
    arrange(municipality_of_production) #

m2018 <- dat |> 
    filter(year == 2018) |> 
    select(municipality_of_production, exporter) |> 
    arrange(exporter) |> 
    filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
    unique() |> 
    add_column(n = 1) |> 
    filter(exporter %in% names(m2020)) |> 
    pivot_wider(names_from = exporter, values_from = n, values_fill = 0) |> 
    arrange(municipality_of_production) |> 
    filter(municipality_of_production %in% m2020$municipality_of_production)  

#now run again m2020 filtering for the same:

m2020 <- dat |> 
    filter(year == 2020) |> 
    select(municipality_of_production, exporter) |> 
    arrange(exporter) |> 
    filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
    unique() |> 
    add_column(n = 1) |> 
    filter(exporter %in% names(m2018)) |> 
    pivot_wider(names_from = exporter, values_from = n, values_fill = 0) |> 
    arrange(municipality_of_production) |> 
    filter(municipality_of_production %in% m2018$municipality_of_production)

## run once more m2018 to make sure they have the same dims
m2018 |>
    select(-municipality_of_production) |>
    write_tsv(file = "data/bipartie-municipality-exporter-2018.txt", col_names = FALSE)
m2020 |>
    select(-municipality_of_production) |>
    write_tsv(file = "data/bipartie-municipality-exporter-2020.txt", col_names = FALSE)

# Attribute file for binary attributes for A (see example attached)
# 
# Not all companies are present in all years.
dat |> 
    select(year, exporter) |> filter(year >= 2017) |> 
    unique() |> filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
    group_by(exporter) |>  add_tally() |> arrange(( n))


# Attribute file for continuous attributes for A
 
exporter_attributes <- dat |> filter(year == 2020) |> arrange(exporter) |> 
    filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
    filter(exporter %in% names(m2020)) |> 
    select(exporter, zd = zero_deforestation_brazil_soy) |> 
    unique() |> #pull(zero_deforestation_brazil_soy) |> table()
    mutate(commitment_2020 = case_when(
        is.na(zd) ~ 0, zd == "NONE" ~ 0,
        zd == "SOY MORATORIUM" ~ 1, zd == "COMPANY COMMITMENT" ~ 1
    )) |> select(-zd) |>
    # the following part removes duplicate exporters that have commitments with some but not (or NA) with others
    # the assumption is that if a company has commitment with at least one, it shows as endorsing commitments.
    group_by(exporter) |> 
    summarize(commitment_2020 = sum(commitment_2020))

exporter_attributes <- exporter_attributes |> 
    left_join(
        dat |> filter(year == 2019) |> arrange(exporter) |> 
            filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
            filter(exporter %in% names(m2020)) |> 
            select(exporter, zd = zero_deforestation_brazil_soy) |> 
            unique() |> #pull(zero_deforestation_brazil_soy) |> table()
            mutate(commitment_2019 = case_when(
                is.na(zd) ~ 0, zd == "NONE" ~ 0,
                zd == "SOY MORATORIUM" ~ 1, zd == "COMPANY COMMITMENT" ~ 1
            )) |> select(-zd) |> unique() |> 
            group_by(exporter) |> 
            summarize(commitment_2019 = sum(commitment_2019))
    ) |> 
    left_join(
        dat |> filter(year == 2018) |> arrange(exporter) |> 
            filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
            filter(exporter %in% names(m2020)) |> 
            select(exporter, zd = zero_deforestation_brazil_soy) |> 
            unique() |> #pull(zero_deforestation_brazil_soy) |> table()
            mutate(commitment_2018 = case_when(
                is.na(zd) ~ 0, zd == "NONE" ~ 0,
                zd == "SOY MORATORIUM" ~ 1, zd == "COMPANY COMMITMENT" ~ 1
            )) |> select(-zd) |> unique() |> 
            group_by(exporter) |> 
            summarize(commitment_2018 = sum(commitment_2018))
    ) |> 
    left_join(
        dat |> filter(year == 2017) |> arrange(exporter) |> 
            filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
            filter(exporter %in% names(m2020)) |> 
            select(exporter, zd = zero_deforestation_brazil_soy) |> 
            unique() |> #pull(zero_deforestation_brazil_soy) |> table()
            mutate(commitment_2017 = case_when(
                is.na(zd) ~ 0, zd == "NONE" ~ 0,
                zd == "SOY MORATORIUM" ~ 1, zd == "COMPANY COMMITMENT" ~ 1
            )) |> select(-zd) |> unique() |> 
            group_by(exporter) |> 
            summarize(commitment_2017 = sum(commitment_2017))
    )
    
exporter_attributes <- exporter_attributes |> 
    left_join(
        dat |> filter(year == 2020) |> arrange(exporter) |> 
            filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
            filter(exporter %in% names(m2020)) |> 
            group_by(exporter) |> 
            summarize(
                soy_traded = sum(soy_equivalent_tonnes, na.rm = TRUE),
                soy_dollars = sum(fob_usd, na.rm = TRUE)
            )
    ) |> 
    left_join(
        dat |> filter(year == 2020) |> arrange(exporter) |> 
            filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
            filter(exporter %in% names(m2020)) |> 
            select(exporter, importer) |> unique() |> 
            group_by(exporter) |> summarize(buyers=n())
    ) |> 
    left_join(
        dat |> filter(year == 2020) |> arrange(exporter) |> 
            filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
            filter(exporter %in% names(m2020)) |> 
            select(exporter, country_of_first_import) |> unique() |> 
            group_by(exporter) |> summarize(countries=n())
    )

write_tsv(exporter_attributes, file = "data/attributes-exporter-2020.txt", col_names = TRUE)



dat |> filter(year == 2020) |> arrange(municipality_of_production) |> 
    filter( exporter != "DOMESTIC CONSUMPTION", exporter != "UNKNOWN") |> 
    filter(municipality_of_production %in% m2020$municipality_of_production) |> 
    group_by(municipality_of_production) |> 
    summarize(
        soy_deforestation = sum(soy_deforestation_exposure, na.rm = TRUE),
        soy_produced = sum(soy_equivalent_tonnes, na.rm = TRUE),
        soy_area = sum(land_use_ha, na.rm = TRUE),
        soy_yield = soy_produced / soy_area
    ) |> 
    write_tsv(file = "data/attributes-municipalities.txt", col_names = TRUE)

# Attribute file for continuous attributes for B





### J240116: It doesn't work, unclear if attributes are node or edge attributes.
### Either case the attributes are not unique, so we need to decide how to handled them.

# tibble(
#     exporter = dat |> arrange(exporter) |> pull(exporter) |> unique(),
#     commitment2020 = dat |> arrange(exporter) |> filter(year == 2020) |> 
#         mutate(comm = case_when(
#             year == 2020 & zero_deforestation_brazil_soy == "SOY MORATORIUM" ~ 1,
#             year == 2020 & zero_deforestation_brazil_soy == "COMPANY COMMITMENT" ~ 1,
#             year == 2020 & zero_deforestation_brazil_soy == "NONE" ~ 0,
#             year == 2020 & is.na(zero_deforestation_brazil_soy) ~ 0,
#         )) |> pull(comm) |> unique()
# )

# dat |> 
#     mutate(commitment2020 = case_when(
#         year == 2020 & zero_deforestation_brazil_soy == "SOY MORATORIUM" ~ 1,
#         year == 2020 & zero_deforestation_brazil_soy == "COMPANY COMMITMENT" ~ 1,
#         year == 2020 & zero_deforestation_brazil_soy == "NONE" ~ 0,
#         year == 2020 & is.na(zero_deforestation_brazil_soy) ~ 0,
#         ),
#            commitment2019 = case_when(
#                year == 2019 & zero_deforestation_brazil_soy == "SOY MORATORIUM" ~ 1,
#                year == 2019 & zero_deforestation_brazil_soy == "COMPANY COMMITMENT" ~ 1,
#                year == 2019 & zero_deforestation_brazil_soy == "NONE" ~ 0,
#                year == 2019 & is.na(zero_deforestation_brazil_soy) ~ 0,
#            ),
#            commitment2018 = case_when(
#                year == 2018 & zero_deforestation_brazil_soy == "SOY MORATORIUM" ~ 1,
#                year == 2018 & zero_deforestation_brazil_soy == "COMPANY COMMITMENT" ~ 1,
#                year == 2018 & zero_deforestation_brazil_soy == "NONE" ~ 0,
#                year == 2018 & is.na(zero_deforestation_brazil_soy) ~ 0,
#            )) |> 
#     select(exporter, starts_with("commitment")) |> 
#     unique() |> 
#     arrange(exporter)


# Binary attributes (for A):
# Company commitment or moratorium in 2020 (1 – yes, 0 – no)
# New company commitment or moratorium in previous year (1 – yes, 0 – no)
# New company commitment or moratorium in previous 2 years (1 – yes, 0 – no)
# New company commitment or moratorium in previous 3 years (1 – yes, 0 – no)
# 
# Continuous attributes (for B):
#     Soy Deforestation (ha)*
#     Territorial deforestation (ha; area of native vegetation that was removed that year; this data can be found via the link below)
# Soy produced (tonnes; this data can be found via the link below)
# Soy area (ha; total area of soy planted in 2020; this data can be found via the link below).
# Soy yield (tonnes/ha; also available via the link below)
# 
# Continuous attributes (for A):
#     Amount of soy traded in 2020
# Dollar value of soy traded in 2020
# Number of buyers in 2020 (importers)
# Number of markets in 2020 (countries)
