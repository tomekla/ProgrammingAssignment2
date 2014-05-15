## test of cache matrix 

test <- function(i) {

    m<-matrix(rnorm(i*i),nrow=i,ncol=i)

    print(paste("Matrix is ",i," by ",i))

    ptm1<-proc.time()
    sfull<-solve(m)
    ptm2<-proc.time()

    print(paste("Full Solve time is: ",ptm2-ptm1))

    t<-makeCacheMatrix(m)
    s1<-cacheSolve(t)
    ptm3<-proc.time()

    print(paste("First Cache time is: ",ptm3-ptm2))        

    s2<-cacheSolve(t)
    ptm4<-proc.time()

    print(paste("Second Cache time is:  ",ptm4-ptm3))

    if(!isTRUE(all.equal(sfull,s1,s2)))
        print(paste("Equality test failed for i=",i))


}
