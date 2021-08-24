makeCacheMatrix <- function(x=matrix()){
  #assume that matrix is invertible
  i <- NULL
  #setting the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
    #used the double arrow operator to maintain its state
  }
  #getting value of the matrix
  get <- function(){
    x 
  }
  #setting value of the inverse
  setInverse <- function(inverse){
    i <<- inverse
  }
  #getting value of the inverse
  getInverse <- function(){
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  #To return a matrix that is an inverse of x
  i <- x$getInverse()
  #If already set, the code shall just return the inverse
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <-x$get()
  #Determine inverse through multiplication of matrix
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
