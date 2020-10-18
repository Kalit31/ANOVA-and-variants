
# library(HSAUR)
# data("skulls", package = "HSAUR") 
mydata<-read.csv("skulls.csv")

summary(skulls)

attach(mydata)
y = cbind(mb, bh, bl, nh)
str(mydata)

x<-epoch

manova1<-manova(
  y ~ x, data=mydata
)


## also like this
manova1<-manova(
  cbind(mb, bh, bl, nh) ~ epoch, data=mydata
)



summary(manova1)


# skull shapes (skull shape measurements) are 
# significantly different across time

summary.aov(manova1) ## details...


