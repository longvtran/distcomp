# Long's attempt at replicating the larger example (page 8), found in 
# list.files(system.file("doc", package = "distcomp"))

graphics.off()
rm(list=ls(all=TRUE))

setwd("C:/Users/longt/Study/Stanford/RA")
library(distcomp)
distcompSetup(workspace = "C:/Users/longt/Study/Stanford/RA",
              ssl_verifyhost = 0L, ssl_verifypeer = 0L, 
              serialize_type = "pb")


# define opencpu (this is not an elegant way?)
# first run library(opencpu) and ocpu_start_server()
# on another RStudio window
opencpu <- list(url = function() "http://localhost:5656/ocpu")

# run the aggregated fit
data(pbc)
pbcCox <- coxph(Surv(time, status==2) ~ age + edema + log(bili) +
                  log(protime) + log(albumin) + strata(ascites), data = pbc,
                ties = "breslow")
print(pbcCox)

# split the data
pbcDef <- data.frame(compType = names(availableComputations())[1],
                     formula = paste("Surv(time, status == 2) ~ age + edema +",
                                     "log(bili) + log(protime) + log(albumin)"),
                     id = "pbc", stringsAsFactors = FALSE)

siteData <- with(pbc, split(x = pbc, f = ascites))
nSites <- length(siteData)

sites <- lapply(seq.int(nSites), function(i) list(name = paste0("site", i),
                                                  url = opencpu$url()))

ok <- Map(uploadNewComputation, sites, lapply(seq.int(nSites), function(i) 
  pbcDef), siteData)
stopifnot(all(as.logical(ok)))

master <- CoxMaster$new(defnId = pbcDef$id, formula = pbcDef$formula)

for (site in sites){
  master$addSite(site$name, site$url)
}

# run the computation
result <- master$run()

# printout the results
kable(master$summary())

# SUCCESS