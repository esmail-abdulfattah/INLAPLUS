

D = matrix(0,12,6)

D[1,1] = 1#sqrt(6)
D[2,1] = -1#sqrt(6)
D[3,2] = 1#sqrt(6)
D[4,2] = -1#sqrt(6)
D[5,3] = 1#sqrt(6)
D[6,3] = -1#sqrt(6)
D[7,4] = 1#sqrt(6)
D[8,4] = -1#sqrt(6)
D[9,5] = 1#sqrt(6)
D[10,5] = -1#sqrt(6)
D[11,6] = 1#sqrt(6)
D[12,6] = -1#sqrt(6)
D = D*sqrt(6)
v1 = c(rep(-1,10), rep(1,10))
v2 =rep(c(rep(-1,5),rep(1,5)),2)
v3 =rep(c(rep(-1,2),rep(1,2)),5)
v4 =rep(c(rep(-1,1),rep(1,1)),10)
v5 = c(1,1,1,-1,1,1,1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,-1)
v6 = rep(c(1,1,-1,1,1,-1,-1,1,-1,1,-1,-1,1,1,-1,1,1,-1,-1,1),1)

BB = matrix(0,20,6)
BB[,1] = v1
BB[,2] = v2
BB[,3] = v3
BB[,4] = v4
BB[,5] = v5
BB[,6] = v6


MM = rbind(rep(0,6),D,BB)
MM = round(MM/sqrt(6),3)
##size 33


setwd("~/Documents/inlaDense/cppfiles/Data/CCDdesign")
write.table(MM, file = "dim6.txt",quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")


