library(metafor)
data<- read.csv("data/preliminary_dataset.csv")

r<- 0.5
dat <- escalc(measure="SMCR", m1i=m_RAVLT_t2, m2i=m_RAVLT_t1, 
              sd1i=sd_RAVLT_t1, ni=n, data=data, ri=r,
              slab=Autori)

#random effects model
res <- rma(yi, vi, data=dat, method="REML")
res


par(mfrow=c(1,2))
forest(res)
funnel(res)

rs <- c(.3, .5, .7)

fits <- lapply(rs, function(r){
  dat_r <- escalc(measure="SMCR",
                  m1i=m_RAVLT_t2, m2i=m_RAVLT_t1,
                  sd1i=sd_RAVLT_t1,
                  ni=n, ri=r,
                  data=data, slab=Autori)
  rma(yi, vi, data=dat_r, method="REML")
})

names(fits) <- paste0("r=", rs)
fits


predict(fits[["r=0.5"]])
predict(fits[["r=0.3"]])
predict(fits[["r=0.7"]])

####Egger's test for asymmetry####
regtest(res, model="rma")


####Descriptives####
#total n
sum(dat$n)
