## This function just puts in x as an input if it is a matrix
## and if not, as a NULL 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }

  get<-function() x
  setInv<-function(inverse) inv<<-inverse
  getInv<-function() inv
  list(set=set,
       get=get,
       setInv=setInv,
       getInv=getInv)
}

## This gives you the inversed matrix and caches it so you can retrieve
## it again later

cacheSolve <- function(x, ...) {
  inv<-x$getInv()
  if(!is.null(inv)) {
    message("retrieving matrix")
    return(inv)
  }

  ans<-x$get()
  inv<-solve(ans,...)
  x$setInv(inv)
  inv

}
