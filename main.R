rm(list=ls())

# Extended Euclidean Algorithm
# Computes d=gcd(u,v) and a,b such that 
# a*u+b*v=d
gcd_E = function(u,v){
    m = matrix(c(1,0,0,1),nrow=2)
    n = 0
    
    while(v != 0){
        q = floor(u/v)
        m =  m %*% matrix(c(q,1,1,0),nrow=2,byrow=T)
        temp = v
        v = u - q*v
        u = temp
        n = n+1
    }
    
    return( list(d=u, a=(-1)^n*m[2,2], b=(-1)^(n+1)*m[1,2]) )
}
