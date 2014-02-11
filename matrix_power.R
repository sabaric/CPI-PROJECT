# Function to raise a matrix to a power.

matrix.power <- function(mat, n)
{
  # test if mat is a square matrix
  # treat n < 0 and n = 0 -- this is left as an exercise
  # trap non-integer n and return an error
  if (n == 1) return(mat)
  result <- diag(1, ncol(mat))
  while (n > 0) {
    if (n %% 2 != 0) {
      result <- result %*% mat
    
      n <- n - 1
    }
    mat <- mat %*% mat
    
    n <- n / 2
  }
  return(result)
}

  
  

A <- matrix(c( 0.8, 0.6, -0.5  ,1, 0, 0, 0, 1 ,0), nrow =3, ncol = 3, byrow = T)
A
matrix.power(A,2)