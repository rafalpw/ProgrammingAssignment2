
## The purpose of these funcitons is to inverse a given matrix
## Function have a cache feature which will prevent from  
## inversing already inversed matrix

## Write a short comment describing this function
## below function will create a vector of functions to cache 
## results of inversing a matrix
## parent environment of this function will store original matrix
## and inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        m<<-NULL
        x<<-y
    }
    get<-function() x
    setinv<- function(inv) m<<- inv
    getinv<-function() m
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
## below function will check if inverse matrix is already computed
## if so it will get a result from cache
## otherwise it will compute inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinv()
    if (!is.null(m)) {
        message('geting results from cache')
        return(m)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setinv(inv)
    inv
}
