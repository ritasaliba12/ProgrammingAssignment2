
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {    #set the value of the vector
    x <<- y
    m <<- NULL
  }
  get <- function() x     #get the value of the vector
  setinv <- function(solve) m <<- solve   #set the value of the inverse
  getinv <- function() m   #get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

x <- matrix(c(2, 4, 3, 1, 5, 7,5,6,7), nrow=3, ncol=3)
makeCacheMatrix(x)

#The following function calculates the inverse of the matrix created 
#with the fucntion above. However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
#via the setinv function.

cacheSolve<- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

cacheSolve(makeCacheMatrix(x))
