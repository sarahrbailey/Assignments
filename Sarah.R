# I.1

set.seed(1)
n=100
x=runif(n)
y=2*x+rnorm(n) #generates response vector y
x2=x+100
X=cbind(1,x,x2)
beta.hat<-solve(t(X)%*%X,t(X)%*%y)

#Error comes up: Error in solve.default(t(X) %*% X, t(X) %*% y) : 
# system is computationally singular: reciprocal condition number = 1.82611e-20

r<-2
n<-nrow(X)
#I.2
svd_decomp<-svd(X,nu=nrow(X),nv=ncol(X))

singular_vals <- svd_decomp$d


sigma_mat <- diag(singular_vals)

d<-diag(svd_decomp$d)
u <- svd_decomp$u
v <- svd_decomp$v
d[3] <- 0

dr<-d[1:r,1:r]
ur<-u[1:n,1:r]
vr<-v[1:r,1:r]

# I.3
MPinv<-vr%*%solve(dr)%*%t(ur) #From lecture nOtes
lin_mod <- lm(y~x+x2)$coefficients
MP.beta<-MPinv%*%y

vec_len <- function(vec){
  len <- sqrt(sum(vec^2, na.rm=TRUE))
  return(len)
}

MP.length <- vec_len(MP.beta)
lin.length <- vec_len(lin_mod)
#MP Smaller

#II.1
set.seed(1)
CC<-t(c(0,1,-1))
CC<-as.matrix(CC)

#II.2
CC.svd<-svd(CC, nu=1,nv=3)


u<-CC.svd$u
d<-CC.svd$d
v<-CC.svd$v
d2<-c(d,0,0)
u%*%d2%*%t(v)

#II.3  #Fixed
ur<-u[1:1,1:1]
dr<-d[1:1,1:1] 
vr<-v[,2:3] 

lm<-lm.fit(X%*%vr, y)
vr%*%lm$coefficients #Right answer

#III.1
set.seed(1)

n_mat <- 1000
p_mat <- 100

m1 <- c(1,rep(0,9),rep(0,90))
m2 <- c(0,1,rep(0,8),rep(0,90))
m3 <- c(rep(0,8),1,0,rep(0,90))
m4 <- c(rep(0,9),1,rep(0,90))

popms <- rbind(m1,m2,m3,m4)

sigma <- 0.05
cov <- sigma^2*diag(p_mat)

p<-matrix(NA, nrow=1000, ncol=100)

for(i in 1:1000){
  p[i,]<-sample(1:4,100,replace=T)
}

# Does this work?
set.seed(1)
X<-matrix(NA,nrow=1000,ncol=100)
for(i in 1:nrow(p)){
for(j in 1:ncol(p)){
ifelse(p[i,j]==1,X[i,j]<-rnorm(1,popms[1,j],sigma),
ifelse(p[i,j]==2, X[i,j]<-rnorm(1,popms[2,j],sigma),
ifelse(p[i,j]==3, X[i,j]<-rnorm(1,popms[3,j],sigma),
ifelse(p[i,j]==4,X[i,j]<-rnorm(1,popms[4,j],sigma),0))))
}}

# III.2
d<-dist(X)
cmd<-cmdscale(d,k=2)
plot(cmd)

#III.3, III.4
par(mfrow=c(1,3))
pc<-princomp(X, cor=F)
plot(-1*pc$scores)
pc2<-prcomp(X, scale=F)
pc2$x[,1]<--pc2$x[,1]
plot(pc2$x)

### III.6
plot(-1*pc$scores)
pc2$x[,1]<--pc2$x[,1]
plot(pc2$x)

#III.7

f1=function(X){
  X=scale(X,center=TRUE, scale=FALSE)
  ss=eigen(t(X)%*%X)
  return(X%*%ss$vectors[,1:2])
}

f2=function(X){
  X=scale(X,center=TRUE, scale=FALSE)
  ss=eigen(X%*%t(X))
  return(ss$vectors[,1:2]%*%diag(sqrt(ss$values[1:2])))
}
plot(-1*f1(X))
plot(f2(X))
#f1 is closes to princomp because it simply required a -1* PC's

system.time(f1(X))
system.time(f2(X))
system.time(prcomp(X)$x[,1:2])
system.time(f1(t(X)))
system.time(f2(t(X)))
system.time(prcomp(t(X))$x[,1:2])


