library(tidyverse)
library(fs)
library(tictoc)
library(patchwork)

# load data
fls <- fs::dir_ls(path = "~/Documents/Projects/DATA/Trase/BRAZIL_SOY_2.6.0_pc/")
fls |> str_subset(".csv")
dat <- read_csv(file = str_subset(fls, ".csv")) |> 
    janitor::clean_names()

skimr::skim(dat)

## create matrices per year, ensure all share same actors on the same order
dat |> pull(municipality_of_production) |> unique() |> length() # 2431 municipalities
dat |> pull(exporter) |> unique() |> length() # 1945 exporters

## there are more than one value per exporter, municipality year; for example:
dat |> 
    filter(year == 2020, exporter == "COFCO INTERNATIONAL GRAINS LTDA.") |> 
    select(importer:last_col())
# this is because the same exporter can send soy from the same municipality to 
# different destinies, and through different importers on the receiving country.
# Solution for the bip network, sum over.

## extracts the bipartite matrix for one year where Bij is the amount of soy exported
tic()
dat_complete <- dat |> 
    select(municipality_of_production, exporter, soy_equivalent_tonnes, year) |> 
    group_by(municipality_of_production, exporter, year) |> 
    summarize(total_soy = sum(soy_equivalent_tonnes)) |> ungroup() |> 
    arrange(municipality_of_production) |> 
    mutate(municipality_of_production = as_factor(municipality_of_production)) |> 
    arrange(exporter) |> 
    mutate(exporter = as_factor(exporter)) |> 
    # this steps ensure that all non-observed links are present in the matrix, it 
    # also makes the data huge
    complete(municipality_of_production, exporter, year, fill = list(total_soy = 0)) 
toc() # 15sec, 1.8Gb

dat_complete |> 
    filter(year == 2013, .preserve = TRUE) |> select(-year) |> 
    pivot_wider(names_from = exporter, values_from = total_soy, values_fill = 0 ) |> 
    select(-municipality_of_production) |> 
    as.matrix() |> dim()
## now all matrices have the same dims

mats <- list()
range(dat$year)

for (i in seq_along(2004:2020)){
    mats[[i]] <- dat_complete |> 
        filter(year == c(2004:2020)[i]) |> select(-year) |> 
        pivot_wider(names_from = exporter, values_from = total_soy, values_fill = 0 ) |> 
        select(-municipality_of_production) |> 
        as.matrix()
}

## test they are comparable:
map(mats, dim) #indeed
map(mats, range)

## calculate correlation between two matrices
x <- cor(c(mats[[17]]), c(mats[[16]]))

df_cor <- tibble(yr1 = c(1:17), yr2 = c(17:1))
df_cor <- df_cor |> 
    complete(yr1, yr2) |> 
    filter(yr1 != yr2)
df_cor <- df_cor |> 
    rowwise() |> 
    mutate(correlation = cor(c(mats[[yr1]]), c(mats[[yr2]])))

a <- df_cor |> 
    mutate(yr1 = yr1 + 2003, yr2 = yr2 + 2003) |> 
    ggplot(aes(yr1, yr2)) +
    geom_tile(aes(fill = correlation)) +
    labs(title = "Pearson correlation coefficient", 
         subtitle = "Bipartite networks of companies and municipalities weighted by soy exported",
         caption = "Data source: trase database, Brazil soy", x = "", y = "", tag = "A") +
    scale_fill_viridis_c(expression(rho), guide = guide_colorbar(barwidth = unit(2,"mm"), barheight = unit(2,"cm")))+
    theme_light(base_size = 6)

## Now do it for just links, not weighted by soy tonnage
df_cor <- df_cor |> # rowwise is already active
    mutate(cor_Bij = cor(c(mats[[yr1]]) > 0, c(mats[[yr2]]) > 0))

b <- df_cor |> 
    mutate(yr1 = yr1 + 2003, yr2 = yr2 + 2003) |> 
    ggplot(aes(yr1, yr2)) +
    geom_tile(aes(fill = cor_Bij)) +
    labs(title = "Pearson correlation coefficient", 
         subtitle = "Bipartite networks of companies and municipalities (Bij)",
         caption = "Data source: trase database, Brazil soy", x = "", y = "", tag = "B") +
    scale_fill_viridis_c(expression(rho), guide = guide_colorbar(barwidth = unit(2,"mm"), barheight = unit(2,"cm")))+
    theme_light(base_size = 6)

ggsave(filename = "correlations_bipartite.png", path = "figures/",
       plot = (a+b), device = "png", width = 7, height = 3.5)
