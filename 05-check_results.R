library(tidyverse)
library(sna)
library(network)
library(networkDynamic)
library(tergm)
library(tsna)
library(tictoc)


load("data/termg_260504.Rda")

summary(mod)
mcmc.diagnostics(mod)

broom::tidy(mod) |> 
    mutate(p_value = case_when(
        p.value < 0.01 ~ "< 0.01",
        p.value >=0.01 & p.value < 0.05 ~ "< 0.05",
        p.value >= 0.05 ~ "> 0.05"
    ),
    term = as_factor(term) |> fct_rev()) |> 
    ggplot(aes(estimate, term)) +
    geom_point(aes(color = p_value, shape = p_value)) +
    geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error, 
                       color = p_value), height = 0.1) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = c(0.9,0.1), legend.position.inside = TRUE)

ggsave(
    filename = "termg_260504.png", path = "figures/", device = "png",
    plot = last_plot() + theme_light(base_size = 16), width = 6, height = 4, dpi = 400
)
