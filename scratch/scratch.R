s <- sFFLHD$new(D=2,L=3)
plot(s$get.batch(),xlim=0:1,ylim=0:1,pch=19);abline(h=(setdiff(0:9,c(0,3,6,9)))/9,v=(setdiff(0:9,c(0,3,6,9)))/9,col=4)
abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batch(),pch=19)
for(i in 1:27){abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batch(),pch=19)}
abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batches.to.golden(),pch=19)


s <- sFFLHD$new(D=4,L=4,a=2)
s$get.batch()
for(i in 1:160) {s$get.batch()}


s <- sFFLHDmm$new(D=2,L=3)
plot(s$get.batch(),xlim=0:1,ylim=0:1,pch=19);abline(h=(0:(s$L)^2)/s$L^2,v=(0:(s$L^2))/s$L^2,col=4);abline(h=(0:(s$L))/s$L,v=(0:(s$L))/s$L,col=3, lwd=3);
abline(h=(0:(s$s$Lb))/s$s$Lb,v=(0:(s$s$Lb))/s$s$Lb,col=3);points(s$get.batch(),pch=19)
for(i in 1:27){abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batch(),pch=19)}
abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batches.to.golden(),pch=19)

# For prospectus slides
oparmar <- par()$mar
par(mar=c(2.3, 2.3, .7, .7)) #BLTR
set.seed(0)
s <- sFFLHDmm$new(D=2,L=3)
plot(s$get.batch(),xlim=0:1,ylim=0:1,pch=19, xlab="", ylab="");abline(h=(0:(s$L)^2)/s$L^2,v=(0:(s$L^2))/s$L^2,col=4, lwd=2);abline(h=(0:(s$L))/s$L,v=(0:(s$L))/s$L,col=3, lwd=4);points(s$s$Xb, pch=19, cex=3)
points(s$get.batch(),pch=19, cex=3)
points(s$get.batch(),pch=19, cex=3)
s$get.batch();abline(h=(0:(s$L)^4)/s$L^4,v=(0:(s$L^4))/s$L^4,col=2,lwd=1);abline(h=(0:(s$L)^2)/s$L^2,v=(0:(s$L^2))/s$L^2,col=4, lwd=2);abline(h=(0:(s$L))/s$L,v=(0:(s$L))/s$L,col=3, lwd=4);points(s$X, pch=19, cex=3)
s$get.batches(23);abline(h=(0:(s$L)^4)/s$L^4,v=(0:(s$L^4))/s$L^4,col=2,lwd=1);abline(h=(0:(s$L)^2)/s$L^2,v=(0:(s$L^2))/s$L^2,col=4, lwd=2);abline(h=(0:(s$L))/s$L,v=(0:(s$L))/s$L,col=3, lwd=4);points(s$X, pch=19, cex=3)
