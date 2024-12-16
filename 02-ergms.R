## simplified script based on 03-bipartite for the ERGMs
## Angela suggested to run them with full dataset, Cerrado only and non-cerrado
## the dataset splits and networks were already created with 01-read_data.R

library(tidyverse)
library(ergm)
library(tictoc)

load("data/cleaned_networks_full.Rda")
load("data/cleaned_networks_cerrado.Rda")
load("data/cleaned_networks_non-cerrado.Rda")

## tests
tic()
fit0 <- ergm(
    net[[8]] ~ edges + b1cov("prop_commit") * b1cov("risk") +
        b1cov("soy")  + b2cov("prop_commit") * b2cov("risk") + b2cov("soy") +
        diff("prop_commit") + diff("risk") +
        gwdsp(fixed=FALSE, cutoff=30)
)
toc()


tic()
fits <- map(
    net,
    function(x) ergm(
        x ~ edges + b1cov("prop_commit") * b1cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b2cov("risk") + b2cov("soy") +
            gwnsp(decay, fixed=FALSE, cutoff=30)
    ),
    .progress = TRUE)
toc() # 580s

# save(fits, file = "data/ergms_cerrado_network.Rda")


out <- map(fits, broom::tidy) 
out <- map2(out, names(fits), function(x,y) {x$year <- y; return(x)})
out <- bind_rows(out)
out |> 
    mutate(p_value = case_when(
        p.value < 0.01 ~ "< 0.01",
        p.value >=0.01 & p.value < 0.05 ~ "< 0.05",
        p.value >= 0.05 ~ "> 0.05"
    )) |> 
    filter(term != "edges") |> 
    ggplot(aes(estimate, term)) +
    geom_point(aes(color = p_value, shape = p_value)) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error, 
                       color = p_value), height = 0.1) +
    scale_color_brewer(palette = "Set1") +
    facet_wrap(~year) 

ggsave(
    filename = "ergms_bipartite_interaction.png", path = "figures/", device = "png",
    plot = last_plot() + theme_light(base_size = 8), width = 6, height = 4, dpi = 400
)
