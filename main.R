rm(list=ls()) # Remove pre-existing objects

gcd_E = function(u,v){
   
    # Extended Euclidean Algorithm
    # Computes d=gcd(u,v) and a,b that satisfy 
    # a*u+b*v=d 
    #
    # Args:
    #   u,v: Two integers, with u>v
    # Returns:
    #   A list with a,b,d, such that au+bv=d
   
    m = matrix(c(1,0,0,1),nrow=2)                       # m = |1 0|
    n = 0                                               #     |0 1|
    
    while(v != 0){
        q = floor(u/v) # Get u/v, less the remainder
        m =  m %*% matrix(c(q,1,1,0),nrow=2,byrow=T)    # m = m * |q 1|
        temp = v                                        #         |1 0|
        v = u - q*v # (u,v)=(v,u-q*v)
        u = temp
        n = n+1
    }
    
    return( list(d=u, a=(-1)^n*m[2,2], b=(-1)^(n+1)*m[1,2]) )
}
