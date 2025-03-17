# Exemplo de cálculo de matriz de Covariâncias e de Correlações amostrais, além da distância de Mahalanobis.

# Vetores e Matriz
y1 <- c(72,60,56,41,32,30,39,42,37,33,32,63,54,47,91,56,79,81,78,46,39,32,60,35,39,50,43,48)
y2 <- c(66,53,57,29,32,35,39,43,40,29,30,45,46,51,79,68,65,80,55,38,35,30,50,37,36,34,37,54)
y3 <- c(76,66,64,36,35,34,31,31,31,27,34,74,60,52,100,47,70,68,67,37,34,30,67,48,39,37,39,57)
y4 <- c(77,63,58,38,36,26,27,25,25,36,28,63,52,43,75,50,61,58,60,38,37,32,54,39,31,40,50,43)

Y <- cbind(y1, y2, y3, y4)
colnames(Y) <- c("North", "East", "South", "West"); Y

class(Y) 

# Matriz de Variâncias e Covariâncias
n <- nrow(Y); n
p <- ncol(Y); p

In <- diag(n); In  
jn <- matrix(data = 1, nrow = n, ncol = 1); jn
Jnn <- matrix(data = 1, nrow = n, ncol = n); Jnn
Jnn <- jn %*% t(jn); Jnn
Sigma <- (1 / (n - 1)) * t(Y) %*% (In - (1/n) * Jnn) %*% Y; Sigma
D <- sqrt(diag(Sigma)); D
corr <- solve(diag(D)) %*% Sigma %*% solve(diag(D)); corr
cor(Y)
Verifica <- diag(D) %*% corr %*% diag(D); Verifica

# Distância de Mahalanobis (Distância padronizada)
mi <- (1/n) * t(jn) %*% Y; mi

DM2 <- rep(0, n)
for (i in 1:n) {
  yi <- Y[i,]
  DM <- as.numeric((yi - mi) %*% solve(Sigma) %*% t(yi - mi))
  DM2[i] <- DM
}; DM2

mahalanobis(x = Y, center = mi, cov = Sigma)

rank <- rank(DM2)
data.frame(Y, DM2, rank)









