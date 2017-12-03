# Long's attempt at replicating the SVD rank-k example (page 10), 
# found inthe paper https://www.jstatsoft.org/article/view/v077i13

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

# construct the compdef for the computation:
svdDef <- data.frame(compType = names(availableComputations())[2], 
                     rank = 2L, ncol = 5L, id = "SVD", 
                     stringsAsFactors = FALSE)

set.seed(12345)
nSites <- 3

siteData <- lapply(seq.int(nSites), function(i) matrix(rnorm(100),
                                                        nrow = 20))
sites <- lapply(seq.int(nSites), function(x) list(name = 
                      paste0("site", x), url = opencpu$url()))

# upload the data to the three sites
ok <- Map(uploadNewComputation, sites, lapply(seq.int(nSites),
                      function(i) svdDef), siteData)

# instantiate master
master <- SVDMaster$new(defnId = svdDef$id, k = svdDef$rank)

# add the three participating sites
for (site in sites) {
  master$addSite(name = site$name, url = site$url)
}

result <- master$run()
result
# SUCCESS

result <- master$run(k = 5)
x <- do.call(rbind, siteData)
result$d
# SUCCESS

# compare with SVD on aggregate data
svd(x)$d

result$v
svd(x)$v
