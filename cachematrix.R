
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix".
## The second function, cacheSolve" computes the inverse of the special "matrix" returned by  makeCacheMatrix above. 
## If the inverse has already been calculated then  cacheSolve  should retrieve the inverse from the cache.


## The first function, makeCacheMatrix is a list containing a function to
## 1. set the value of the inverse
## 2. get the value of the inverse
## 3. set the value of the inverse
## 4. get the value of the inverse
 
 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # Creates a new variable m and assign it to null in the environment of makeCacheMatrix
  
  ## Define the "set" functions defined within the containing makeCacheMatrix() function. 
  ## Variable x in the containing environment is updated to be y. 
  ## Note that it requires special assignment operator (<<-) to update x.
  
  set <- function(y) {
    x <<- y     
    m <<- NULL
  }
  get <- function() x # Retrieve the values of x from makeVector
  
  
  setinverse <- function(inverse) m <<- inverse #setinverse is a function which allows inverse to be set mannually.
  getinverse <- function() m
  
  # 'List' will allow you to access these functions outside of makeVector environment.
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it  get s the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache via the  setinverse  function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ##  Codes below will checkhecks to see if the inverse has already been calculated. 
  ## If so, it  gets the inverse from the cache and skips the computation.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  
   
   ## If the inverse has not been calculated, then do as below. 
  data <- x$get()  # retrieve data from x once you call the function makeCacheMatrix.
  m <- solve(data, ...) #Calculate inverse matrix usingsolve()
  x$setinverse(m)       #Sets the value of the inverse in the cache via the  setinverse function
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












