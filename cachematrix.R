#this function will give a list, which contains the original matrix and the possible inverse matirx
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  
  #get is a function that will get the value of "x"
  get <- function() x
  #setinverse is a function that is possible to give "m" a value
  
  setinverse <- function(inverse) m <<- inverse
  #getinverse is a funciotn that will get the value of "m"
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#this function will take the result that the last function return as its arguments,
#and first it will check if the inverse has been given,
#if ture, it will print "getting cached data"
#if not, it will calculate the inverse matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #to check if the inverse matrix has been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not, calculate the inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
