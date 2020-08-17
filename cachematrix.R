## create a function that inverts an invertible matrix 
## and caches the result to avoid recomput

## caching function

makeCacheMatrix <- function(x = matrix()) {
  ## the inverted mat will go here
  n<-NULL
  
  ## makes a new spot for us with the matrix and a null spot for its inverse
  set<-function(y){
    x<<-y
    n<<-NULL
  }
  ## get returns the x we have
  get<-function(){x}
  
  ## setinverse takes in the function 'solve', and returns n 
  ## as equal to that solved matrix
  setinverse<-function(solve) n<<-solve
  
  ##getinverse returns the inverse we have on hand
  getinverse<-function(){n}
    
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## inversion.  If inversion already cached return instead.

cacheSolve <- function(x, ...) {
    ## gets the inverse if it exists
    q<-x$getinverse()
    if (!is.null(q)){
      message("pre-cached")
      return(q)
    }
    
    # if not, get, then solve x, and set its inverse in the cache
    d<-x$get()
    S<-solve(d,...)
    x$setinverse(S)
    S
}
