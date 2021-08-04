####################
#Play-the-winner rule

p_A <- 0.3
p_B <- 0.6
P <- c(p_A,p_B)
n <- 10

trt = NULL
j = NULL

j = sample(P,size = 1)
trt[[1]] = c(rbinom(1,1,prob = j),j)

trt ; j

for(i in 1:n)
{
  if(trt[[i]][1] == 1){
    j = P[which(P == j)]
    trt[[i+1]] = c(rbinom(1,1,j),j)
  } else {
    j = P[which(P != j)]
    trt[[i+1]] = c(rbinom(1,1,j),j)
  }
}

trt

#Using Data matrix

p_A <- 0.3
p_B <- 0.6
P <- c(p_A,p_B)
n <- 999

trt = matrix(,nrow = n+1,ncol = 3)
j = NULL

j = sample(P,size = 1)
trt[1,1] = 1 ; trt[1,2] = rbinom(1,1,prob = j) ; trt[1,3] = j

trt[1:10,] ; j

for(i in 1:n)
{
  trt[i+1,1] = i+1
  if(trt[i,2] == 1){
    j = P[which(P == j)]
    trt[i+1,2] = rbinom(1,1,j)
    trt[i+1,3] = j
  } else {
    j = P[which(P != j)]
    trt[i+1,2] = rbinom(1,1,j)
    trt[i+1,3] = j
  }
}

summary(trt)
sum(trt[,3] == 0.3)/(n+1)
sum(trt[,3] == 0.6)/(n+1)
(1-p_A)/(1-p_B)
0.627/0.373
head(trt)
trt[,3] <- as.factor(trt[,3])

#Sequential Allocation Proportions for both the treatments
q_A = 1-p_A ; q_B = 1-p_B
k <- 10
nA <- cumsum(trt[,3] == 1)
nB <- cumsum(trt[,3] == 2)
plot(1:(n+1),(nA)/(1:(n+1)),type = "o",pch = 20,main = "Allocation proportions for Both Treatments",xlab = "Sample Size n",ylab = "Prop",ylim = c(0,1))
abline(h = q_B/(q_A+q_B))
par(new = TRUE)
plot(1:(n+1),(nB)[1:(n+1)]/(1:(n+1)),type = "o",pch = 20,main = NULL,ylab = "",xlab = "",xaxt = "n",yaxt = "n",ylim = c(0,1),col = "red")
abline(h = q_A/(q_A+q_B),col = "red")
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))

#success proportion
n_A <- sum(trt[,3] == 1)
plot(which(trt[,3] == 1),cumsum(trt[,2][which(trt[,3] == 1)])/1:n_A,type = "o",pch = 20,main = "Success proportion for both treatment",ylab = "Prop",ylim = c(0,1),xlab = "sample size n")
abline(h = p_A)
par(new = TRUE)
n_B <- sum(trt[,3] == 2)
plot(which(trt[,3] == 2),cumsum(trt[,2][which(trt[,3] == 2)])/1:n_B,,type = "o",pch = 20,main = NULL,ylab = "",xlab = "",xaxt = "n",yaxt = "n",ylim = c(0,1),col = "red")
abline(h = p_B)
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))

#allocation proportion
plot(which(trt[,3] == 1),cumsum(trt[,2][which(trt[,3] == 1)])/which(trt[,3] == 1),type = "o",pch = 20,main = "Allocation proportion for Treatment A",ylab = "Prop",xlab = "sample size n")
abline(h = q_B/(q_A+q_B))
par(new = TRUE)
n_B <- sum(trt[,3] == 2)
plot(which(trt[,3] == 2),cumsum(trt[,2][which(trt[,3] == 2)])/which(trt[,3] == 2),type = "o",pch = 20,main = NULL,ylab = "",xlab = "",xaxt = "n",yaxt = "n",ylim = c(0,1),col = "red")
abline(h = q_B/(q_A+q_B))
abline(h = q_A/(q_A+q_B))
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))


n_A <- sum(trt[,3] == 1)
n_B <- sum(trt[,3] == 2)
q_A = 1- p_A ; q_B = 1- p_B
plot(2:n_A,(2:n_A)/which(trt[,3] == 1)[-1],type = "o",pch = 20,main = "Allocation proportion for Treatment A",xlab = "Sample Size n",ylab = "Prop")
abline(h = q_B/(q_A+q_B))
legend("topright",expression(atop(q[B]/q[A]+q[B])),col = "black")


##########################
#Randomized play-the-winner rule

p_A <- 0.2
p_B <- 0.4
Prob <- P <- c(p_A,p_B)
n <- 10^3 - 1
trt = matrix(,nrow = n+1,ncol = 3)
j = sample(P,1,replace = T)
trt[1,1] = 1 ; trt[1,2] = rbinom(1,1,prob = j) ; trt[1,3] = j

for(i in 1:n)
{
  if(trt[i,2] == 1){
    P = c(P,P[which(Prob == j)])
    j = sample(P,1,replace = T)
    trt[i+1,2] = rbinom(1,1,j)
    trt[i+1,3] = j
  } else {
    P = c(P,P[which(Prob != j)])
    j = sample(P,1,replace = T)
    trt[i+1,2] = rbinom(1,1,j)
    trt[i+1,3] = j
  }
  trt[i,1] <- i
}

trt[,3] <- as.factor(trt[,3])

#success proportion

n_A <- sum(trt[,3] == 1)
plot(which(trt[,3] == 1),cumsum(trt[,2][which(trt[,3] == 1)])/1:n_A,type = "o",pch = 20,main = "Success proportion for both treatment",ylab = "Prop",ylim = c(0,1),xlab = "sample size n")
abline(h = p_A)
par(new = TRUE)
n_B <- sum(trt[,3] == 2)
plot(which(trt[,3] == 2),cumsum(trt[,2][which(trt[,3] == 2)])/1:n_B,,type = "o",pch = 20,main = NULL,ylab = "",xlab = "",xaxt = "n",yaxt = "n",ylim = c(0,1),col = "red")
abline(h = p_B)
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))

#allocation proportion

n_A <- sum(trt[,3] == 1)
n_B <- sum(trt[,3] == 2)
q_A = 1- p_A ; q_B = 1- p_B
data.frame(1:n_A,which(trt[,3] == 1))[1:10,]
plot(2:n_A,(2:n_A)/which(trt[,3] == 1)[-1],type = "o",pch = 20,main = "Allocation proportion for Treatment A",xlab = "Sample Size n",ylab = "Prop",ylim = c(0.2,0.8))
abline(h = q_B/(q_A+q_B))
legend("topright",expression(atop(q[B]/q[A]+q[B])),col = "black")
plot(2:n_B,(2:n_B)/which(trt[,3] == 2)[-1],type = "o",pch = 20,main = "Allocation proportion for Treatment A",xlab = "Sample Size n",ylab = "Prop",ylim = c(0.2,0.8))
abline(h = q_A/(q_A+q_B))
legend("topright",expression(atop(q[B]/q[A]+q[B])),col = "black")

#Sequential Allocation Proportions for both the treatments
k <- 10
nA <- cumsum(trt[,3] == 1)
nB <- cumsum(trt[,3] == 2)
plot(k:(n+1),(nA)[k:(n+1)]/(k:(n+1)),type = "o",pch = 20,main = "Allocation proportions for Both Treatments",xlab = "Sample Size n",ylab = "Prop",ylim = c(0.2,0.8))
abline(h = q_B/(q_A+q_B))
par(new = TRUE)
plot(k:(n+1),(nB)[k:(n+1)]/(k:(n+1)),type = "o",pch = 20,main = NULL,ylab = "",xlab = "",xaxt = "n",yaxt = "n",ylim = c(0.2,0.8),col = "red")
abline(h = q_A/(q_A+q_B),col = "red")
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))

#######################
# Sequential maximum likelihood procedure

p_A <- 0.3
p_B <- 0.7
P <- c(p_A,p_B)
n <- 10^3
trt = matrix(,nrow = n,ncol = 3)
j = NULL
n0 <- 10

A <- rbinom(n0,1,p_A)
B <- rbinom(n0,1,p_B)

trt[1:n0,2] <- A ; trt[1:n0,3] <- rep(p_A,n0)
trt[(n0+1):(2*n0),2] <- B ; trt[(n0+1):(2*n0),3] <- rep(p_B,n0)
trt[1:(2*n0),1] <- 1:(2*n0)

trt[1:20,]

pA_est <- sum(trt[,2][which(trt[,3] == p_A)])/length(which(trt[,3] == p_A))
pB_est <- sum(trt[,2][which(trt[,3] == p_B)])/length(which(trt[,3] == p_B))

for(i in (2*n0+1):n)
{
  pA_alloc <- sqrt(pA_est)/(sqrt(pA_est) + sqrt(pB_est))
  pB_alloc <- 1 - pA_alloc
  trt[i,1] <- i
  j <- rbinom(1,1,prob = pB_alloc)
  trt[i,2] <- rbinom(1,1,prob = rbinom(1,1,P[j+1]))
  trt[i,3] <- P[j+1]
  pA_est <- sum(trt[,2][which(trt[,3] == p_A)])/length(which(trt[,3] == p_A))
  pB_est <- sum(trt[,2][which(trt[,3] == p_B)])/length(which(trt[,3] == p_B))
}
trt[1:40,]
dim(trt)
sum(trt[,3] == 0.3)/nrow(trt)
sqrt(p_A)/(sqrt(p_A) + sqrt(p_B))
plot((2*n0):n,cumsum(trt[,3] == p_A)[(2*n0):n]/((2*n0):nrow(trt)),pch = 20,type = "o",main = "Allocation Proportion of Trt A 
     in Sequential Maximum Likelihood Procedure",ylab = "Prop",xlab = "Sample Size n")
abline(h = sqrt(p_A)/(sqrt(p_A) + sqrt(p_B)),lty = 2)
legend("topright",expression(atop(sqrt(p[A])/sqrt(p[A])+sqrt(p[B]))),col = "black",lty = 2)


#Sequential Allocation Proportions for both the treatments
q_A = 1- p_A ; q_B = 1- p_B
k <- 10
nA <- cumsum(trt[,3] == p_A)
nB <- cumsum(trt[,3] == p_B)
plot(k:(n+1),(nA)[k:(n+1)]/(k:(n+1)),type = "o",pch = 20,main = "Allocation proportions for Both Treatments",xlab = "Sample Size n",ylab = "Prop",ylim = c(0.2,0.8),cex = 0.2)
abline(h = sqrt(p_A)/(sqrt(p_A) + sqrt(p_B)))
par(new = TRUE)
plot(k:(n+1),(nB)[k:(n+1)]/(k:(n+1)),type = "o",pch = 20,main = NULL,ylab = "",xlab = "",xaxt = "n",yaxt = "n",ylim = c(0.2,0.8),col = "red",cex = 0.2)
abline(h = sqrt(p_B)/(sqrt(p_A) + sqrt(p_B)),col = "red")
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))


#Doubly adaptive biased coin design

g <- function(x,y,gm)
{
  t = (y*(y/x)^gm)/((y*(y/x)^gm) + ((1-y)*((1-y)/(1-x))^gm))
  return(t)
}

set.seed(1000)
p_A <- 0.4
p_B <- 0.6
P <- c(p_A,p_B)
n <- 500
trt = matrix(,nrow = n,ncol = 3)
j = NULL
n0 <- 10

A <- rbinom(n0,1,p_A)
B <- rbinom(n0,1,p_B)

trt[1:n0,2] <- A ; trt[1:n0,3] <- rep(p_A,n0)
trt[(n0+1):(2*n0),2] <- B ; trt[(n0+1):(2*n0),3] <- rep(p_B,n0)
trt[1:(2*n0),1] <- 1:(2*n0)

trt[1:20,]

pA_est <- sum(trt[,2][which(trt[,3] == p_A)])/length(which(trt[,3] == p_A))
pB_est <- sum(trt[,2][which(trt[,3] == p_B)])/length(which(trt[,3] == p_B))

for(i in (2*n0+1):n)
{
  rho <- sqrt(pA_est)/(sqrt(pA_est) + sqrt(pB_est))
  prop_A <- sum(trt[,3][1:(i-1)] == p_A)/(i-1)
  pA_alloc <- g(x = prop_A,y = rho,gm = 0)
  pB_alloc <- 1 - pA_alloc
  trt[i,1] <- i
  j <- rbinom(1,1,prob = pB_alloc)
  trt[i,2] <- rbinom(1,1,prob = rbinom(1,1,P[j+1]))
  trt[i,3] <- P[j+1]
  pA_est <- sum(trt[,2][which(trt[,3] == p_A)])/length(which(trt[,3] == p_A))
  pB_est <- sum(trt[,2][which(trt[,3] == p_B)])/length(which(trt[,3] == p_B))
}
trt[1:40,]
dim(trt)
sum(trt[,3] == max(p_A,p_B))/nrow(trt)
sum(trt[,3] == min(p_A,p_B))/nrow(trt)
sqrt(p_A)/(sqrt(p_A) + sqrt(p_B))
plot((2*n0+1):n,cumsum(trt[,3] == p_A)[(2*n0+1):n]/((2*n0+1):nrow(trt)),pch = 20,type = "o",main = "Allocation Proportion of Both Treatments 
     in Doubly adaptive biased coin design",ylab = "Prop",xlab = "Sample Size n",cex = 0.2,ylim = c(0,1))
par(new = TRUE)
plot((2*n0+1):n,cumsum(trt[,3] == p_B)[(2*n0+1):n]/((2*n0+1):nrow(trt)),pch = 20,type = "o",main = "",ylab = "",xlab = "",xaxt = "n",yaxt = "n",cex = 0.2,ylim = c(0,1),col = "red")
abline(h = sqrt(p_A)/(sqrt(p_A) + sqrt(p_B)))
abline(h = sqrt(p_B)/(sqrt(p_A) + sqrt(p_B)),col = "red")
legend("topleft",expression(paste(gamma,"= 20")),col = "black")
legend("topright",legend = c("Trt A","Trt B"),fill = c("black","red"))

legend("topright",expression(atop(sqrt(q[A])/sqrt(q[A])+sqrt(q[B]))),col = "black",lty = 2)

# When the difference between p_A and p_B is small, 
# it's quite hard to determine the better treatment 
# as the power of the test decreases so we require 
# more samples here
# for the doubly adaptive biased coin design if we increase gamma value, 
# the allocatoin converges to the better treatment quickly 
# but for smaller values of gamma, the observed proportion 
# converges to the optimal allocation proportion (whatever it will be) 