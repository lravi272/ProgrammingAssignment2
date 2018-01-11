


##To create a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function()x
	setInverse<-function(inverse)i<<-inverse
	getInverse<-function()i
	list(set=set,get=get,
	setInverse=setInverse,
	getInverse=getInverse)

}



##Computes the inverse of the special matrix and stores it in cache using makeCacheMatrix
cacheSolve <- function(x, ...) {
	existingInverse <- x$getInverse()
	if(!is.null(existingInverse)){
		message("Retrieving cached data")
		return(existingInverse)
	}
	
	data<-x$get()
	inverse<-solve(data)
	x$setInverse(inverse)
}
