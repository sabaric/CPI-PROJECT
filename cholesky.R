#Cholesky decomposition
A <- matrix(runif(25),5,5); A <- A%*%t(A) ## Example +ve def matrix
 
U <- chol(E)   ## start from factor given by chol
D <- diag(U)   ## extract sqrt(D) 
L <- t(U/D)     ## get unit lower triangular factor
D <- diag(D^2)  ## and diagonal
## So now A = LDL' 
modelrange(A-L%*%D%*%t(L)) 

A1 <- matrix(c(2, -1.6, -1.6, 5.28),2,2)
chol(A1)