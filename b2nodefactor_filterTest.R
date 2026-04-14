library(ergm)
temp<-matrix(sample(c(0,1),200,prob=c(.8,.2),replace=T),nrow=10,ncol=20)

net<-network(temp,bipartite=T)
net%v%"prop_commit"<-c(rep("NA",10),rep("Low",10),rep("High",10))

##summary of filter term should match the summary of the separated networks without filter

##extract out the right sub-networks - here, this would be just the nodes with 'low' (netA) and 'high' (netB)
netA<-network(temp[,1:10],bipartite=T)
netB<-network(temp[,11:20],bipartite=T)

summary(netA~gwb2degree)
summary(netB~gwb2degree)

##this should match the responses for netA
summary(net~ F( ~gwb2degree, ~ nodefactor("prop_commit", levels = "Low")))

##this should match the responses for netB
summary(net~ F( ~gwb2degree, ~ nodefactor("prop_commit", levels = "High")))


