K = matrix(c(1/2, 1/4, 1/4,
             1/4, 1/2, 1/4,
             1/4, 1/4, 1/2), nrow=3, byrow=TRUE)
rowSums(K)
colSums(K)
#note that rows & columns sum to 1
matrixpower = function(mat, pow){
  #this function multiplies a matrix to a specified power
  stopifnot(pow > 0) #pow must be positive
  stopifnot(pow %% 1 == 0) #pow must be an integer
  mat1 = mat
  if(pow>1){
    for(i in 2:pow)
      mat1 = mat1 %*% mat
  }
  mat1
} #does this work for pow==1?

matrixpower(K,2)
matrixpower(K,3)
matrixpower(K,5)
matrixpower(K,10)
#as n->infinity, the rows of K^n approach the "invariant probability"
#this means that, as the number of iterations get higher, it matters less 
#where the initial probability distribution is.
c(1,0,0) %*% matrixpower(K,20)
c(0,1,0) %*% matrixpower(K,20)
c(0,0,1) %*% matrixpower(K,20)

#the rows of K MUST sum to 1. If the columns also do, the invariant prob is uniform (1/3, 1/3, 1/3)
#what happens if we change it?

K = matrix(c(1/2, 1/4, 1/4,
             1/4, 1/2, 1/4,
             1/12, 5/12, 1/2), nrow=3, byrow=TRUE)
rowSums(K)
colSums(K)
#note that rows sum to 1 but columns don't
matrixpower(K,2)
matrixpower(K,3)
matrixpower(K,5)
matrixpower(K,10)
c(1,0,0) %*% matrixpower(K,20)
c(0,1,0) %*% matrixpower(K,20)
c(0,0,1) %*% matrixpower(K,20)
c(1/3, 1/3, 1/3) %*% matrixpower(K,20)

#what sort of matrix will this not work for? A non-ergodic markov chain
K = matrix(c(1, 0, 0,
             0, 1, 0,
             0, 0, 1), nrow=3, byrow=TRUE)
rowSums(K)
colSums(K)
matrixpower(K,2)
matrixpower(K,10)
matrixpower(K,100)#no invariant probability. The starting point still matters. 
#This is not an irreducible MC

K = matrix(c(1/2, 1/4, 1/4,
             1/4, 1/2, 1/4,
             0, 0, 1), nrow=3, byrow=TRUE)
rowSums(K)
colSums(K)
matrixpower(K,2)
matrixpower(K,10)
matrixpower(K,100)#there's an invariant prob... but it's c(0, 0, 1)
#again, not an irreducible MC. You can't get from state C to states A or B

K = matrix(c(0, 1, 0,
             0, 0, 1,
             1, 0, 0), nrow=3, byrow=TRUE)
rowSums(K)
colSums(K)
matrixpower(K,2)
matrixpower(K,10)
matrixpower(K,100)
matrixpower(K,101)#it's changing with every iteration
matrixpower(K,102)
#there's no invariant prob, this Markov Chain is periodic.
