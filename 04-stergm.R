library(tidyverse)
library(sna)
library(network)
library(networkDynamic)
library(tergm)
library(tsna)
library(tictoc)

tic()
dnet <- networkDynamic(network.list = net)
toc() # takes several mins


tic()
mod1 <- tergm(
    dnet ~ Form(
        ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) +
            b2star(k = 2, attr = "cat_commit") +
            b2cov("countries") + b2cov("buyers") +
            b1cov("prop_commit") * b2cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy")
    ) + Persist(
        ~ edges + 
            gwb2degree(fixed = TRUE, decay=2.85) +
            b2star(k = 2, attr = "cat_commit") +
            b2cov("countries") + b2cov("buyers") +
            b1cov("prop_commit") * b2cov("risk") +
            b1cov("soy")  + b2cov("prop_commit") * b1cov("risk") + b2cov("soy")
    ),
    estimate = "CMLE", times = 0:8
)
toc()
