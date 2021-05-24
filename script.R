library(rgl)
library(mclust)

data <- read.table('C:\\Users\\Razor\\Desktop\\дистанційне навчання\\статистичний аналіз багатовимірних даних\\lab5\\v9.txt',
                   header = T)
pairs(data)

mod <- Mclust(data)
summary(mod)

plot(mod, what = "BIC")

datamod <- MclustDR(mod)
plot(datamod, what = 'evalues')

plot(datamod, what = 'scatterplot')

plot(datamod, what = 'pairs')

plot3d((as.matrix(data) %*% datamod$basis)[,1:3],
       col = c('red', 'green', 'blue')[mod$classification])

plot3d((as.matrix(data) %*% datamod$basis)[,3:5],
       col = c('red', 'green', 'blue')[mod$classification])

plot(princomp(covmat = mod$parameters$variance$sigma[,,1]))
plot(princomp(covmat = mod$parameters$variance$sigma[,,2]))
plot(princomp(covmat = mod$parameters$variance$sigma[,,3]))

plot(princomp(covmat = mod$parameters$mean))
