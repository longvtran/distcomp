# USING RPROTOBUF
# Long's attempt at replicating the simple example, found in 
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

# begin the simple example
library(knitr)
library(survival)
data(ovarian)
str(ovarian)

kable(ovarian)

# a Cox fit on the aggregated data
cp <- coxph(Surv(futime, fustat) ~ age + strata(rx), data = ovarian, ties = "breslow")
print(cp)

# model definition
print(availableComputations())

ovarianDef <- data.frame(compType = names(availableComputations())[1],
                         formula = "Surv(futime, fustat) ~ age",
                         id = "Ovarian", stringsAsFactors=FALSE)

# split the ovarian data into two sites
# here we pretend that data set is from two sites
# one having placebo group (rx = 1) and the other
# having the drug group (rx = 2)
siteData <- with(ovarian, split(x = ovarian, f = rx))

# set up the sites
nSites <- length(siteData)
sites <- lapply(seq.int(nSites), function(i) list(name = paste0("site", i),
                                                  url = opencpu$url()))

# Map the upload function to each site
ok <- Map(uploadNewComputation, sites,
          lapply(seq.int(nSites), function(i) ovarianDef),
          siteData)

stopifnot(all(as.logical(ok)))

# reproduce original analysis in distributed fashion
master <- CoxMaster$new(defnId = ovarianDef$id, formula = ovarianDef$formula)

# add worker sites:
for (i in seq.int(nSites)) {
  master$addSite(name = sites[[i]]$name, url = sites[[i]]$url)
}

# maximize the partial likelihood
result <- master$run()

# print the summary
master$summary()

## SUCCESS
