## simplified script based on 03-bipartite for the ERGMs
## Angela suggested to run them with full dataset, Cerrado only and non-cerrado
## the dataset splits and networks were already created with 01-read_data.R

library(tidyverse)
library(ergm)
library(tictoc)

load("data/cleaned_networks_full.Rda") # working on this one with isolates
load("data/cleaned_networks_cerrado.Rda")
load("data/cleaned_networks_non-cerrado.Rda")

## set up tergm:
# df_idx <- tibble(id = seq_along(net)) |> 
#     mutate(id_1 = lag(id))
## Not needed by the way that edgecov works, I can set it up on the map(ergm) section
# net[2:length(net)] <- map2(
#     .x = net[2:length(net)],
#     .y = net[1:length(net)-1],
#     .f = function(x,y){
#         m <- as.sociomatrix(y)
#         # complete the matrix with zeroes to make it one mode
#         m <- m |> 
#             # complete dimensions of the m to make it one mode:
#             rbind(matrix(0, nrow = (2900- nrow(m)), ncol = ncol(m))) |> 
#             cbind(matrix(0, nrow = 2900, ncol = (2900 - ncol(m)))) 
#         
#         x %e% "past_net" <- m
#         return(x)
#     }
# )

## tests: b1 municipalities, b2 companies
tic()
fit0 <- ergm(
    net[[8]] ~ edges + 
        #b1starmix(k = 2, attr = "cat_commit") + 
        b2starmix(k = 2, attr = "cat_commit") +
        b2cov("countries") + b2cov("buyers") +
        b1cov("prop_commit") * b2cov("risk") +
        b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy") +
        ##diff("prop_commit") + diff("risk") #+
        #gwb2degree(fixed = TRUE, decay = 2.86) + 
        gwb1degree(fixed = TRUE, decay = 0.6) +
        edgecov(as.sociomatrix(net[[1]]), attrname = "past_net") +
        ##gwb1dsp(fixed=TRUE, decay = 0.5) +
        gwb2dsp(fixed=TRUE, decay = 0.75),
    # short ergm, change  MCMLE.maxit = 60
    control = control.ergm(parallel = 10, parallel.type = "PSOCK") 
)
toc() 
# 20s simple model: b1-2covs, lines 38:40
# 306.311 sec elapsed with gwb2degree fixed
# 357s using both gwdegrees
# 731s with edgecov, using as.sociomatrix(previous network) is working.
# With gwb2dsp: Warning: ‘glpk’ selected as the solver, but package ‘Rglpk’ is not available; falling back to ‘lpSolveAPI’. This should be fine unless the sample size and/or the number of parameters is very big\
# after installing package: In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
# Unable to reach target effective size in iterations alotted.
# gwb2dsp alone takes 7.3s
# 468.478 sec elapsed with gwd2dsp but without gwdegrees
# bstarmix: ‘b1starmix.2.prop_commit.0.993925116986996.0’, and ‘b1starmix.2.prop_commit.0.996493085308564.0’ are not varying. This may indicate that the observed data occupies an extreme point in the sample space or that the estimation has reached a dead-end configuration.
# MCMLE estimation stuck. There may be excessive correlation between model terms, suggesting a poor model for the observed data. If target.stats are specified, try increasing SAN parameters.
# In addition: Warning message:
#     In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
#                                                                          Unable to reach target effective size in iterations alotted.
#                                                                          
tic()
fit0 <- ergm(
    net[[2]] ~ edges + 
        b1starmix(k=2, attr="cat_commit", base = c(2,3)), 
        #gwb2degree(fixed = TRUE, decay=2.85) + gwb1degree(fixed = FALSE, cutoff = 500) ,
        ##gwb1dsp(fixed=TRUE, decay = 0.5) +
        #gwb2dsp(fixed=FALSE, cutoff = 100),
    control = control.ergm(parallel = 10, parallel.type = "PSOCK", MCMLE.maxit = 10)
)
toc() # 518s, decay gwb2degree = 2.85, significant.
# 10s, decay gwb2dsp =  2.3e-16, non-significant
# degrees in b1 and b2 are linear combinations so cannot be used together
summary(fit0)

tic() ## run after meeting
fits <- map2(
    .x = net[2:length(net)],
    .y = net[1:length(net)-1],
    function(x,y) ergm(
        x ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) +
            b2star(k = 2, attr = "cat_commit", base = c(2,3)) +
            b2cov("countries") + b2cov("buyers") +
            b1cov("prop_commit") * b2cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy") + 
            edgecov(as.sociomatrix(y), attrname = "past_net") ,
            #gwb2dsp(fixed=TRUE, decay = 0.01), 
        control = control.ergm(parallel = 10, parallel.type = "PSOCK")
    ),
    .progress = TRUE)
toc() # 580s | 920s with the new terms for buyers and countries
# 1326.672 sec elapsed with gwb2dsp fixed 0.75, parallel 10 cores
# 2300.104 sec elapsed with gwb3dsp and edgecov, parallel 10 cores, 38min
lobstr::obj_size(fits) # 115MB
#save(fits, file = "data/tergms_full_network_geometric.Rda")


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
    facet_wrap(~year, ncol = 4) +
    theme(legend.position = c(0.9,0.1), legend.position.inside = TRUE)

ggsave(
    filename = "tergms_bipartite_geometric_250828.png", path = "figures/", device = "png",
    plot = last_plot() + theme_light(base_size = 8), width = 6, height = 4, dpi = 400
)


## all delays:
## 
tic()
fit2020 <- ergm(
    net[[7]] ~ edges + 
        b2cov("countries") + b2cov("buyers") +
        b1cov("prop_commit") * b2cov("risk") +
        b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy") +
        edgecov(as.sociomatrix(net[[1]]), attrname = "2014") +
        edgecov(as.sociomatrix(net[[2]]), attrname = "2015") +
        edgecov(as.sociomatrix(net[[3]]), attrname = "2016") +
        edgecov(as.sociomatrix(net[[4]]), attrname = "2017") +
        edgecov(as.sociomatrix(net[[5]]), attrname = "2018") +
        edgecov(as.sociomatrix(net[[6]]), attrname = "2019") +
        gwb2dsp(fixed=TRUE, decay = 0.75),
    control = control.ergm(parallel = 10, parallel.type = "PSOCK")
)
toc() #728.496 sec elapsed, 12mins

summary(fit2020)
#save(fits, fit2020, file = "data/tergms_full_network_geometric.Rda")



#### temporal ####
library(terg)
library(networkDynamic)