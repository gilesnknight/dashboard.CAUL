# library(data.table)
# set.seed(1)
# value <- c(rnorm(50, mean = 1), rnorm(50, mean = 3))
# site  <- c(rep("site1", 50), rep("site2", 50))
# dt    <- data.table(site,value)
# #  generate kdf
# gg <- dt[,list(x=density(value)$x, y=density(value)$y),by="site"]
# 
# 
# 
# PER_SSC_DATA[,list(x=density(PER_SSC_DATA$PerAnyTree)$x, y=density(PER_SSC_DATA$PerAnyTree)$y)]
# 
# 
# PER_SSC_DATA[,list(x=density(PerAnyTree)$x, y=density(PerAnyTree)$y)]
#      
# list(x=density(PER_SSC_DATA$PerAnyTree)$x, y=density(PER_SSC_DATA$PerAnyTree)$y)
# 
# require(sm)
# library(sm)
# foo <- data.frame(Return=rpois(100,5))
# foo$density <- sm.density(foo$Return,eval.points=foo$Return)$estimate
# # the plot
# id <- order(foo$Return)
# hist(foo$Return,freq=F)
# lines(foo$Return[id],foo$density[id],col="red")