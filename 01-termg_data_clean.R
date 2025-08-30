## The idea is to reimport data but keeping matrix row and cols constant over years
## so creating zeroes / isolates in a way that all years have the same actors on the 
## same order

library(tidyverse)
library(fs)
library(tictoc)
library(network)

fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/BRAZIL_SOY_2.6.0_pc/")
fls |> str_subset(".csv")
dat <- read_csv(file = str_subset(fls, ".csv")) |> 
    janitor::clean_names()

complete_set <- dat |> 
    filter(year >= 2013) |> 
    select(municipality = municipality_of_production, exporter, 
           soy_equivalent_tonnes, soy_deforestation_risk, year, biome) |> 
    ## exclude undesirable obs before any calculation
    filter(municipality != "UNKNOWN" , exporter != "UNKNOWN") |> 
    filter(exporter != "DOMESTIC CONSUMPTION") |> 
    expand(municipality, exporter) |> 
    unique()

# as matrix it has 2278 Municipalites and 623 unique exporters
complete_set |> 
    mutate(value = 1) |> 
    pivot_wider(values_from = value, names_from = exporter) 

tic()
dat <- dat |> 
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
    map(.f = right_join, complete_set) |> 
    map(.f = arrange, municipality, exporter) |> 
    map(.f = function(x){
        x |> mutate(
            year = case_when(is.na(year) ~ mean(year, na.rm = TRUE), .default = year),
            soy_tonnes = case_when(is.na(soy_tonnes) ~ 0, .default = soy_tonnes),
            mean_risk = case_when(is.na(mean_risk) ~ 0, .default = mean_risk),
            any_cerrado = case_when(is.na(any_cerrado) ~ 0, .default = any_cerrado)
        )
    })
toc() # 1.2s

net <- dat |> 
    map(.f = function(x){
        x |> select(municipality, exporter, soy_tonnes) |> 
            pivot_wider(names_from = exporter, values_from = soy_tonnes)
    })

## test names are the same
net |> map(.f = dim) ## all same dims
row_nms <- net[[1]]$municipality

tic()
net <- net |> map(.f = function(x){
    m <- x |> select(-municipality) |> 
        as.matrix() 
    rownames(m) <- row_nms
    ntw <- network(m, bipartite = TRUE, loops = FALSE)
    return(ntw)
})
toc()

all((net[[1]] |> network.vertex.names()) %in% (net[[2]] |> network.vertex.names()))

## with this routine networks are created but there is no edge attributes because
## the network was not created as edge list. `dat` contain the edge attributes
tic()
net <- map2(net, dat, .f = function(x,y){
    m <- y |> select(municipality, exporter, soy_tonnes) |> 
        pivot_wider(names_from = exporter, values_from = soy_tonnes) |> 
        select(-municipality) |> 
        as.matrix()
    m <- m |> 
        # complete dimensions of the m to make it one mode:
        rbind(matrix(0, nrow = (2900- nrow(m)), ncol = ncol(m))) |> 
        cbind(matrix(0, nrow = 2900, ncol = (2900 - ncol(m)))) 
    
    x %e% "soy" <- m
    return(x)
})
toc()
# repeat for risk
tic()
net <- map2(net, dat, .f = function(x,y){
    m <- y |> select(municipality, exporter, mean_risk) |> 
        pivot_wider(names_from = exporter, values_from = mean_risk) |> 
        select(-municipality) |> 
        as.matrix()
    m <- m |> 
        # complete dimensions of the m to make it one mode:
        rbind(matrix(0, nrow = (2900- nrow(m)), ncol = ncol(m))) |> 
        cbind(matrix(0, nrow = 2900, ncol = (2900 - ncol(m)))) 
    
    x %e% "mean_risk" <- m
    return(x)
})
toc()
## repeat for Cerrado
tic()
net <- map2(net, dat, .f = function(x,y){
    m <- y |> select(municipality, exporter, any_cerrado) |> 
        pivot_wider(names_from = exporter, values_from = any_cerrado) |> 
        select(-municipality) |> 
        as.matrix()
    m <- m |> 
        # complete dimensions of the m to make it one mode:
        rbind(matrix(0, nrow = (2900- nrow(m)), ncol = ncol(m))) |> 
        cbind(matrix(0, nrow = 2900, ncol = (2900 - ncol(m)))) 
    
    x %e% "any_cerrado" <- m
    return(x)
})
toc() #0.6s


## Homogeneous networks: all are based on the same matrix including isolates
save(net, file = "data/homogeneous_nets.Rda")


#### Left overs ####
## Prob: Solved
## The network is represented as one mode
# net[[1]] |> network.size() # 2900: it's represented as one mode
# nn <- net[[1]] |> network.vertex.names()
# (nn %in% (dat[[1]] |> pull(municipality) |> unique()))[2201:2279] # first names are municipalites
# (nn %in% (dat[[1]] |> pull(exporter) |> unique()))[2277:2300] # after 2278 are exporters
# 
# m <- dat[[1]] |> select(municipality, exporter, soy_tonnes) |> 
#     pivot_wider(names_from = exporter, values_from = soy_tonnes) |> 
#     select(-municipality) |> 
#     as.matrix()
# m |> 
#     rbind(matrix(0, nrow = (2900- nrow(m)), ncol = ncol(m))) |> 
#     cbind(matrix(0, nrow = 2900, ncol = (2900 - ncol(m)))) |> 
#     dim()
