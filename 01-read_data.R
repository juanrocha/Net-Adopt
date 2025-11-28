## Read raw data and clean up datasets to create networks later
## Updated 20241211 to simplify steps from 03-bipartite.Rmd
## Old commented code is from veresion 2.5 of Trase
## Current settings for the full network, needs manual adjustment for cerrado vs non-cerrado datasets
library(tidyverse)
library(fs)
library(tictoc)
library(network)

fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/BRAZIL_SOY_2.6.0_pc/")
fls |> str_subset(".csv")
dat <- read_csv(file = str_subset(fls, ".csv")) |> 
    janitor::clean_names()

## We normalized by municipality area in the next steps but we decided not to use
## it on the  modeling to ease interpretation of coefficients. The code is here
## in any case:
## Since we aren't using it, comment out to speed up computations
# library(sf)
# bzl <- sf::read_sf(
#     "~/Documents/Projects/DATA/GADM_maps/brazil/gadm41_BRA_shp/gadm41_BRA_2.shp")
# tic()
# bzl <- bzl |> 
#     mutate(area_m2 = st_area(geometry)) |> 
#     mutate(area_ha = area_m2 / 10000)
# toc() #31s
# 
# bzl <- bzl |> 
#     as_tibble() |> 
#     select(name = NAME_2, area_ha) |> 
#     mutate(name = stringi::stri_trans_general(name, id = "latin-ascii")) |> 
#     mutate(name = str_to_upper(name)) 
# 
# sum((dat$municipality_of_production |> unique()) %in% bzl$name)
# # 2412 municipalities matched out of 2431
# unique(dat$municipality_of_production)[!unique(dat$municipality_of_production) %in% bzl$name] |> sort()
# # Only 19 municipalities missing
# bzl$name[!bzl$name %in% unique(dat$municipality_of_production) ]
# 
# # modify the names in bzl so they match the ones on trase data
# # or better, modify trase so it follows official names, most of it are mispellings in portuguese
# # bzl |> 
# #     arrange(name) |> 
# #     filter(str_detect(name, "MOGI"))
# 
# dat <- dat |> 
#     mutate(municipality_of_production = 
#                case_when(
#                    municipality_of_production == "CORONEL VIVID" ~ "CORONEL VIVIDA",
#                    municipality_of_production == "COUTO DE MAGALHAES" ~ "COUTO DE MAGALHAES DE MINAS",
#                    municipality_of_production == "ELDORADO DO S" ~ "ELDORADO DO SUL",
#                    municipality_of_production == "FAXINAL DOS G" ~ "FAXINAL DOS GUEDES",
#                    municipality_of_production == "FLORINIA" ~ "FLORANIA",
#                    municipality_of_production =="FORTALEZA DO TABOCAO" ~ "TABOCAO",
#                    municipality_of_production == "LIMEIRA DO OE"~ "LIMEIRA DO OESTE",
#                    municipality_of_production =="MOGI-MIRIM"~ "MOGI MIRIM",
#                    municipality_of_production =="MUQUEM DE SAO FRANCISCO"~ "MUQUEM DO SAO FRANCISCO",
#                    municipality_of_production =="PATOS DE MINA"~ "PATOS DE MINAS",
#                    municipality_of_production =="PORTO NACIONA"~ "PORTO NACIONAL",
#                    municipality_of_production =="POXOREO"~ "POXOREU",
#                    municipality_of_production =="SANTANA DO LIVRAMENTO" ~ "SANT'ANA DO LIVRAMENTO",
#                    municipality_of_production =="SAO LUIS DO PARAITINGA" ~ "SAO LUIZ DO PARAITINGA",
#                    municipality_of_production =="SAO MIGUEL D OESTE"~ "SAO MIGUEL DO OESTE",
#                    municipality_of_production == "SAO THOME DAS LETRAS"~ "SAO TOME DAS LETRAS",
#                    municipality_of_production =="SAO VALERIO DA NATIVIDADE" ~ "SAO VALERIO", #?
#                    municipality_of_production == "VILA BELA SANTISSIMA TRINDADE"~ "VILA BELA DA SANTISSIMA TRINDADE",
#                    .default = municipality_of_production
#                )
#     )
# 
# # join the ones that matches
# towns <- dat |> select(municipality_of_production) |> unique()
# 
# 
# 
# towns <- towns |> 
#     left_join(bzl, by = c("municipality_of_production" = "name"))
# 
# towns |> 
#     filter(is.na(area_ha)) |> 
#     arrange(municipality_of_production)
# 
# # it works, all towns have area in hectares
# 
# towns

#### Create networks ####
## Angela suggested to do the ERGMs with full dataset, only for Cerrado cases, and non-Cerrado.
## It means the next section needs to be repeated activating or deactivating the filter for Cerrado
## Then save the relevant objects for ERGMs in separate Rdat files.


net <- dat |> 
    filter(year >= 2013) |> 
    select(municipality = municipality_of_production, exporter, 
           soy_equivalent_tonnes, soy_deforestation_risk, year, biome) |> 
    ## exclude undesirable obs before any calculation
    filter(municipality != "UNKNOWN" , exporter != "UNKNOWN") |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    mutate(cerrado = case_when(biome == "CERRADO" ~ TRUE, .default = FALSE)) |> 
    ## This is the line to filter out Cerrado from non-Cerrado. Turn off completely for full network
    #filter(cerrado != TRUE) |> 
    group_by(municipality, exporter, year) |> 
    # ungroup() |> skimr::skim()
    summarize(
        soy_tonnes = sum(soy_equivalent_tonnes, na.rm = TRUE), 
        mean_risk = weighted.mean(soy_deforestation_risk, w = soy_equivalent_tonnes,
                                  na.rm = TRUE),
        any_cerrado = sum(cerrado)) |> 
    ungroup() |> #select(-year) |> #slice(517)
    split(~year) |> 
    map(.f = network, matrix.type = "edgelist", ignore.eval = TRUE, bipartite = TRUE,
        loops = FALSE)
# 
#     network( matrix.type = "edgelist", ignore.eval = TRUE, bipartite = TRUE,
#             loops = FALSE)
net

# if using homegeneous networks:
load("data/homogeneous_nets.Rda")
# get the attributes table on the same order as nodes in the network
df_attr <- list()
df_attr <- map(net, function(x) {y <- tibble(node = network.vertex.names(x)); return(y)})

## municipalities attributes
tic()
df_mun <- dat |> filter(year >= 2013) |> 
    select(municipality = municipality_of_production, exporter,
           soy_equivalent_tonnes, soy_deforestation_risk, year, biome) |> 
    filter(municipality != "UNKNOWN" , exporter != "UNKNOWN") |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    mutate(cerrado = case_when(biome == "CERRADO" ~ TRUE, .default = FALSE)) |> 
    ## This is the line to filter out Cerrado from non-Cerrado. Turn off completely for full network
    #filter(cerrado != TRUE) |>
    group_by(municipality, year) |> 
    summarize(
        soy_tonnes = sum(soy_equivalent_tonnes, na.rm = TRUE),
        soy_tonnes_log = log10(soy_tonnes), 
        mean_risk = weighted.mean(soy_deforestation_risk, w = soy_equivalent_tonnes,
                                  na.rm = TRUE),
        sum_risk = sum(soy_deforestation_risk, na.rm = TRUE),
        # now as sum of links to cerrado, it can be soy in tonnes from cerrado as well,
        # or the proportion
        cerrado = sum(cerrado, na.rm = TRUE)
    ) |> 
    # move this below, do the risk / area at the end to avoid duplication of rows
    # left_join(
    #     # there are several municipalities with the same name but different areas
    #     towns |> group_by(municipality_of_production) |>
    #         summarize(area_ha = max(area_ha)) |>
    #         ungroup() |> 
    #         rename(municipality = municipality_of_production)
    # ) |>
    # mutate(norm_risk = sum_risk / area_ha) |> 
    split(~year)
toc() #14.5


## companies attributes
df_comp <- dat |> 
    filter(year >= 2013) |> 
    #filter(biome != "CERRADO") |> 
    select(exporter, municipality = municipality_of_production,
           soy_equivalent_tonnes, soy_deforestation_risk, year, biome) |> 
    filter(municipality != "UNKNOWN" , exporter != "UNKNOWN") |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    mutate(cerrado = case_when(biome == "CERRADO" ~ TRUE, .default = FALSE)) |> 
    ## This is the line to filter out Cerrado from non-Cerrado. Turn off completely for full network
    #filter(cerrado != TRUE) |>
    group_by(exporter, year) |> 
    summarize(
        soy_tonnes_log = sum(soy_equivalent_tonnes, na.rm = TRUE) |> log10(), 
        mean_risk = weighted.mean(soy_deforestation_risk, w = soy_equivalent_tonnes,
                                  na.rm = TRUE) ,
        sum_risk = sum(soy_deforestation_risk, na.rm = TRUE),
        cerrado = sum(cerrado, na.rm = TRUE)
    ) 
## add the summaries of number of countries, and number of exporters (buyers)
df_comp_countries <- dat |> 
    filter(year >= 2013) |> 
    #filter(biome != "CERRADO") |> 
    select(exporter, country = country_of_first_import, year) |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    group_by(exporter, year) |> 
    unique() |> 
    summarize(countries = n())

df_comp_buyers <- dat |> 
    filter(year >= 2013) |> 
    #filter(biome != "CERRADO") |> 
    select(exporter, importer, year) |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    group_by(exporter, year) |> 
    unique() |> 
    summarize(buyers = n())

df_comp <- df_comp |> 
    left_join(df_comp_countries) |> 
    left_join(df_comp_buyers) |>
    ungroup() |> 
    split(~year)

## commitments dataset with both companies and municipalities
df_commit <- dat |> 
    filter(year >= 2013) |> 
    #filter(biome != "CERRADO") |> 
    select(exporter, municipality =  municipality_of_production,
           commitment = zero_deforestation_brazil_soy,
           soy_equivalent_tonnes, year) |> 
    filter(municipality != "UNKNOWN" , exporter != "UNKNOWN") |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    mutate(commitment = case_when(
        is.na(commitment) ~ "missing",
        TRUE ~ commitment
    )) 

commit_towns <- df_commit |> 
    group_by(exporter, municipality, year, commitment) |> 
    summarise(soy_tonnes = sum(soy_equivalent_tonnes)) |> # this is the total soy per dyad
    select(-exporter) |> 
    group_by(municipality, commitment, year) |> 
    summarize(total_soy = sum(soy_tonnes)) |> # this is total soy per municipality
    ungroup() |> group_by(municipality, year) |> 
    mutate(prop_soy = total_soy / sum(total_soy) ) |> #pull(prop_soy) |> range()
    ungroup() |> 
    pivot_wider(id_cols = c(municipality, year), 
                names_from = commitment, values_from = prop_soy) |> 
    rowwise() |> 
    ## line for full dataset
    mutate(prop_commit = sum(`COMPANY COMMITMENT`, `SOY MORATORIUM`, na.rm = TRUE)) |> 
    ## line for Cerrado, there is no soy moratorium
    # mutate(prop_commit = sum(`COMPANY COMMITMENT`,  na.rm = TRUE)) |>
    ungroup() |> 
    split(~year)

commit_companies <- df_commit |> 
    group_by(exporter, municipality, year, commitment) |> 
    summarise(soy_tonnes = sum(soy_equivalent_tonnes)) |> # this is the total soy per dyad
    ungroup() |> 
    select(-municipality) |> 
    group_by(exporter, commitment, year) |> 
    summarize(total_soy = sum(soy_tonnes)) |> 
    ungroup() |> group_by(exporter, year) |> 
    mutate(prop_soy = total_soy / sum(total_soy) ) |> 
    ungroup() |> 
    pivot_wider(id_cols = c(exporter, year),
                names_from = commitment, values_from = prop_soy)|>
    rowwise() |> 
    ## line for full dataset
    mutate(prop_commit = sum(`COMPANY COMMITMENT`, `SOY MORATORIUM`, na.rm = TRUE)) |> 
    ## line for Cerrado, there is no soy moratorium
    # mutate(prop_commit = sum(`COMPANY COMMITMENT`,  na.rm = TRUE)) |>
    ungroup() |> 
    split(~year)

df_comp <- map2(
    .x = df_comp,
    .y = commit_companies,
    .f = function(x,y) left_join(x,y) |> 
        mutate(prop_commit = case_when(
            is.na(prop_commit) ~ 0, TRUE ~ prop_commit))
)

df_mun <- map2(
    .x = df_mun,
    .y = commit_towns,
    .f = function(x,y) left_join(x,y) |> 
        rowwise() |> 
        ## line for full dataset
        mutate(prop_commit = sum(`COMPANY COMMITMENT`, `SOY MORATORIUM`, na.rm = TRUE)) |> 
        ## line for Cerrado, there is no soy moratorium
        # mutate(prop_commit = sum(`COMPANY COMMITMENT`,  na.rm = TRUE)) |>
        mutate(prop_commit = case_when(is.na(prop_commit) ~ 0, TRUE ~ prop_commit))
)

names(df_mun[[1]]) %in% names(df_comp[[1]])

df_attr <- pmap(
    .l = list(df_attr, df_mun, df_comp),
    .f = function(x,y,z){
        x |> left_join(
            bind_rows(rename(y, node = municipality),
                      rename(z, node = exporter))
        )
    }
)

### fill up with zeroes for isolate attributes:
df_attr <- map(
    df_attr,
    .f = function(x) {
        x |> mutate(across(soy_tonnes:last_col(), function(z) replace_na(z, 0)))
    }
)

## normalizing / rescaling risk
df_attr <- map(
    df_attr, function(x) mutate(
        x, risk_scaled = mean_risk / (median(mean_risk)),
        risk_log = log1p(sum_risk),
        risk_stand = risk_log - mean(risk_log) / sd(risk_log)))

## make prop-commitment categorical for modelling purposes
#df_attr[[1]] |> select(node, prop_commit) |> filter(is.na(prop_commit))
df_attr <- map(
    df_attr, function(x) mutate(
        x, cat_commit = case_when(
            prop_commit < 0.5 ~ "low", 
            prop_commit >= 0.5 ~ "high",
            .default = "none"
        )
    )
)

## set cat_commit for municipalities to NA for modelling: there are 2278 municipalites on mode 1 (first on the list)
## set cat_commit for municipalities to zero
df_attr[[1]] |> select(node, cat_commit) |> slice(2275:2280)
df_attr <- map(
    df_attr, function(x) {
        x$cat_commit[1:2278] <- 0
        return(x)
    }
)
# Doesn't work due to Error: In term ‘b2starmix’ in package ‘ergm’: Attribute ‘"cat_commit"’ has missing data, which is not currently supported by ergm

net <- map2(.x = net, .y = df_attr,
            .f = function(x,y) {x %v% "soy" <- y$soy_tonnes_log; return(x)})
net <- map2(.x = net, .y = df_attr,
            .f = function(x,y) {x %v% "risk" <- y$risk_stand; return(x)})
net <- map2(.x = net, .y = df_attr,
            .f = function(x,y) {x %v% "prop_commit" <- y$prop_commit; return(x)})
net <- map2(.x = net, .y = df_attr,
            .f = function(x,y) {x %v% "cat_commit" <- y$cat_commit; return(x)})
# net <- map2(.x = net, .y = df_attr,
#             .f = function(x,y) {x %v% "risk_norm" <- as.numeric(y$norm_risk); return(x)})
net <- map2(.x = net, .y = df_attr,
            .f = function(x,y) {x %v% "countries" <- y$countries; return(x)})
net <- map2(.x = net, .y = df_attr,
            .f = function(x,y) {x %v% "buyers" <- y$buyers; return(x)})


## J250225: It is very difficult to set the matrix in year -1 as a edge attribute on year 0
## the reason is the difference in actors from a network to another.
# m1 <- net[[8]] |> as.sociomatrix()
# m2 <- net[[7]] |> as.sociomatrix()
# 
# dim(m1) # future
# dim(m2) # past
# sum(!colnames(m1) %in% colnames(m2))
# # complement of 2, the smaller network: first compute columns
# c2 <- matrix(0, nrow = nrow(m2), ncol = sum(!colnames(m1) %in% colnames(m2)),
#              dimnames = list(rownames(m2), colnames(m1)[!colnames(m1) %in% colnames(m2)]))
# # merge
# m2 <- cbind(m2,c2)


save(net, file = "data/cleaned_networks_full.Rda")





#### Left overs version 2.5 ####
# library(tidyverse)
# library(fs)
# 
# 
# fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/BRAZIL_SOY_2.5.1_pc/")
# 
# fls |> str_subset(".csv")
# 
# dat <- read_csv(file = str_subset(fls, ".csv")) |>
#     janitor::clean_names()
# 
# skimr::skim(dat)
# dat
# 
# dat |>
#     pull(municipality) |>
#     unique() |> length()
# 
# 
# dat |> pull(trase_geocode) |> unique()
# 
# dat |>
#     select(year, exporter, importer, country)
# 
# dat |> pull(exporter) |> unique() |> length()
# dat |> pull(importer) |> unique() |> length()
# dat |> pull(country) |> unique() |> length()
# 
# dat |>
#     ggplot(aes(land_use_ha)) +
#     geom_density()+
#     scale_x_log10()
# 
# dat |>
#     filter(year == 2018) |>
#     filter(zero_deforestation_brazil_soy == "COMPANY COMMITMENT") |>
#     group_by(exporter,importer) |>
#     summarize(n = n())
# 
# dat |> pull(zero_deforestation_brazil_soy) |> unique()
