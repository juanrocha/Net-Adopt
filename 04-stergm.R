library(tidyverse)
library(sna)
library(network)
library(networkDynamic)
library(tergm)
library(tsna)
library(tictoc)

tic()
dnet <- networkDynamic(network.list = net[1:3])
toc() # takes several mins, 252s | 4.2m


tic()
mod5 <- tergm(
    dnet ~ Form(
        ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) + isolates() +
            gwb2dsp(fixed=TRUE, decay = 0.75) +
            #b2nodematch(attr= "cat_commit", diff = TRUE) + #Error
            #b1starmix(k=2, attr="cat_commit", diff=FALSE) + #alternative: failing
            b2star(k = 2, attr = "cat_commit") +
            b2cov("countries") + b2cov("buyers") +
            b1cov("prop_commit") * b2cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy")
    ) + Persist(
        ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) + isolates() +
            gwb2dsp(fixed=TRUE, decay = 0.75) +
            #b2nodematch(attr= "cat_commit", diff = TRUE) + # error
            #b1starmix(k=2, attr="cat_commit", diff = FALSE) + # alternative: fails
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
# mod 3 66051.742 sec elapsed | 18.3hrs | 352Mb
# mod 4 87110.513 sec elapsed } 24.5hrs | 352Mb

summary(mod4)
save(mod4, file = "data/termg4.Rda", compress = TRUE)
lobstr::obj_size(mod4)
mcmc.diagnostics(mod4)
