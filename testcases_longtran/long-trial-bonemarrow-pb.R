# Long's attempt at replicating the bone marrow example (page 9), found in 
# list.files(system.file("doc", package = "distcomp"))

graphics.off()
rm(list=ls(all=TRUE))

setwd("C:/Users/longt/Study/Stanford/RA")
library(distcomp)
distcompSetup(workspace = "C:/Users/longt/Study/Stanford/RA",
              ssl_verifyhost = 0L, ssl_verifypeer = 0L, 
              serialize_type = "pb")

opencpu <- list(url = function() "http://localhost:5656/ocpu")

# load the package
if (!require("KMsurv")) {
  stop("Please install the KMsurv package before proceeding")
}

library(KMsurv)
data(bmt)

# rename variables
bmt$tnodis <- bmt$t2 ## time to disease relapse/death
bmt$inodis <- bmt$d3 ## disease relapse/death indicator
bmt$tplate <- bmt$tp ## time to platelet recovery
bmt$iplate <- bmt$dp ## platelet recovery
bmt$agep <- bmt$z1 ## age of patient in years
bmt$aged <- bmt$z2 ## age of donor in years
bmt$fab <- bmt$z8 ## fab grade 4 or 5 + AML
bmt$imtx <- bmt$z10 ## MTX used
bmt <- bmt[order(bmt$tnodis), ] ## order by time to disease relapse/death
bmt <- cbind(1:nrow(bmt)[1], bmt)
names(bmt)[1] <- "id"

bmt$agep.c <- bmt$agep - 28
bmt$aged.c <- bmt$aged - 28

bmt$imtx <- factor(bmt$imtx)

# check the aggregated fit
bmt.cph <- coxph(formula = Surv(tnodis, inodis) ~ fab + agep.c * aged.c + 
                   factor(group) + strata(imtx), data = bmt, ties = "breslow")

print(bmt.cph)

# split data into sites
bmtDef <- data.frame(compType = names(availableComputations())[1],
                     formula = paste("Surv(tnodis, inodis) ~ fab +",
                                     "agep.c * aged.c + factor(group)"),
                     id = "bmt", stringsAsFactors = FALSE)
siteData <- with(bmt, split(x = bmt, f = imtx))
nSites <- length(siteData)
sites <- lapply(seq.int(nSites), function(i) list(name = paste0("site", i),
                                                  url = opencpu$url()))

ok <- Map(uploadNewComputation, sites, lapply(seq.int(nSites), function(i) bmtDef), 
          siteData)
stopifnot(all(as.logical(ok)))

master <- CoxMaster$new(defnId = bmtDef$id, formula = bmtDef$formula)

for (site in sites){
  master$addSite(site$name, site$url)
}

# run the computation
result <- master$run()

# print the results
kable(master$summary())