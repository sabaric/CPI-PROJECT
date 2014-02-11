mp <- function(mat,n.out)
  {
  IRF <- array( dim= length(n.out) )  
  ans <- mat
  IRF[1] <-ans[1,1]
    for ( i in 2:(n.out))
    {
    ans <- t(mat)%*%ans
    IRF[i] <- ans[1,1]
    }
  return(IRF)
  }
  a
  A <- matrix(c( -0.1728,1.1504,0.0513,-0.4785,0.3853,-0.1724,-0.1189,1.0159
                 ,1,0,0,0,0,0,0,0, 0,1,0,0,0,0,0,0, 0,0,1,0,0,0,0,0
                 ,0,0,0,1,0,0,0,0, 0,0,0,0,1,0,0,0, 0,0,0,0,0,1,0,0
                 ,0,0,0,0,0,0,1,0), nrow =8, ncol = 8, byrow = T)

  A
  mp(A,40)
 plot(ts(mp(A,30)))