## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
#This function is used to return a list where we store the matrix and inverse of the matrix. It creates a special matrix object that can cache its inverse.
  {
  m <- NULL   # Initializing our Matrix inverse as Null
  set <- function(y) {
    x <<- y   # Y parameter matrix is cached and stored as x and Inverse matrix is initialized as NULL
    m <<- NULL
  }
  get <- function() x  # Returning the value of x matrix which is the input matrix.
  setInverse <- function(inverse) m <<- inverse  # Caching and Storing the inverse of the matrix
  getInverse <- function() m   # returing the Mat_Inv after caching from setInverse
  
  # Creating list to return the calculations from each functions
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  # Return a matrix that is the inverse of 'x'
  {
  m <- x$getInverse()   # Gets list as parameter from function makeCacheMatrix
  if(!is.null(m))   # Return a matrix from cache to check if the m has inverse matrix stored in cache or not
    {
    message("getting cached data")  # If the value of m already has cached ,then it is returned directly.
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)   # Calculates inverse of the matrix
  x$setInverse(m)  # Passing the inverse matrix as paramter to setInverse function
  m ## Return a matrix that is the inverse of 'x'
}



result <- matrix(c(10,12,3,44),2,2)  # Creating a 2x2 matrix 
cachedData <- makeCacheMatrix(result) # Calling makeCacheMatrix
cacheSolve(cachedData)   # Calling cacheSolve using the returned list from makeCacheMatrix
