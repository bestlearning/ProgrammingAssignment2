## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Creates a new variable in the environment of makeCacheMatrix
  
  set <- function(y) {
    x <<- y  #assign the value y to the variable x, where x is the input variable in the makeCacheMatrix  environment
    m <<- NULL
  }
  get <- function() x
  
  #setinverse is a function which allows inverse to be set by hand.
  #It is this function that is used by cacheSolve to actually store the inverse in m. 
  
  setinverse <- function(inverse) m <<- inverse 
  getinverse <- function() m
  
  # Allows for access these functions outside of makeVector environment
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

#############

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # call data from x
  m <- solve(data, ...) #calculate inverse matrix by solve()
  x$setinverse(m)       #Store the inverse matrix in m
  m
}



#########
# example 1 on how it works

amatrix = makeCacheMatrix(matrix(c(0,2,3,4,5,6,7,8,9), nrow=3, ncol=3))


amatrix$get()         # Returns original matrix
amatrix$getinverse()

cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse

amatrix$getinverse()  # Returns matrix inverse

cacheSolve(amatrix) 

# example 2 on how it works

amatrix$set(matrix(c(5,11,22,33,44,55,66,77,88), nrow=3, ncol=3)) # Modify existing matrix


cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse

amatrix$get()         # Returns matrix

amatrix$getinverse()  # Returns matrix inverse

## varidataion of inverse matrix: Multiply matrix by its inverse and show that the result is indeed the Identity matrix.
amatrix$get() %*% cacheSolve(amatrix)












