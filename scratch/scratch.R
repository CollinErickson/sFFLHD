s <- sFFLHD$new(D=2,L=3)
plot(s$get.batch(),xlim=0:1,ylim=0:1,pch=19)
abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batch(),pch=19)
for(i in 1:27){abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batch(),pch=19)}
abline(h=(0:(s$Lb))/s$Lb,v=(0:(s$Lb))/s$Lb,col=3);points(s$get.batches.to.golden(),pch=19)


s <- sFFLHD$new(D=4,L=4,a=2)
s$get.batch()
for(i in 1:160) {s$get.batch()}
