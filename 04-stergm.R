library(tidyverse)
library(sna)
library(network)
library(networkDynamic)
library(tergm)
library(tsna)
library(tictoc)

tic()
dnet <- networkDynamic(network.list = net[6:8])
toc() # takes several mins, 252s | 4.2m


tic()
mod4 <- tergm(
    dnet ~ Form(
        ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) + isolates() +
            #b2nodematch(attr= "cat_commit", diff = TRUE) + #Error
            b1starmix(k=2, attr="cat_commit", diff=FALSE) + #alternative
            b2star(k = 2, attr = "cat_commit") +
            b2cov("countries") + b2cov("buyers") +
            b1cov("prop_commit") * b2cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy")
    ) + Persist(
        ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) + isolates() +
            #b2nodematch(attr= "cat_commit", diff = TRUE) +
            b1starmix(k=2, attr="cat_commit", diff = FALSE) +
            b2star(k = 2, attr = "cat_commit") +
            b2cov("countries") + b2cov("buyers") +
            b1cov("prop_commit") * b2cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy")
    ),
    estimate = "CMLE", times = 1:3
)
toc() 
#50s without geometric terms
# 214975.933 sec elapsed | 186Mb object | 2.5 days | mod1
# 76107.105 sec elapsed | 186Mb | 21hrs | mod2
summary(mod2)
save(mod3, file = "data/termg3.Rda", compress = TRUE)
lobstr::obj_size(mod2)
