
fmincg = function(f,X, Maxiter=10, ...) {
    length = Maxiter
    RHO = 0.01 
    SIG = 0.5  
    INT = 0.1  
    EXT = 3.0  
    MAX = 20   
    RATIO = 100
    
    red = 1
    i = 0
    ls_failed = 0
    fX = numeric(0)
    
    FUN = match.fun(f)
    JandG = FUN(X, ...)
    f1 = JandG$J
    df1 = JandG$grad
    
    i = i + (length<0)
    s = -df1
    d1 = t(-s)%*%s
    z1 = red/(1-d1)
    
    while (i < abs(length)) {
        i = i + (length>0)
        
        X0 = X; f0 = f1; df0 = df1
        X = X + z1[1]*s
        
        JandG = FUN(X, ...)
#        eval = f(X)
        f2 = JandG$J
        df2 = JandG$grad
        i = i + (length<0)
        d2 = t(df2)%*%s;
        f3 = f1; d3 = d1; z3 = -z1;
        if(length>0){M=MAX}else{M=min(MAX,-length-i)}
        success = 0; limit = -1; 
        while(1){
            while(((f2 > f1+z1*RHO*d1) | (d2 > -SIG*d1)) & (M > 0)) {
                limit = z1
                if (f2 > f1){
                    z2 = z3 - (0.5*d3*z3*z3)/(d3*z3+f2-f3)
                } else {
                    A = 6*(f2-f3)/z3+3*(d2+d3)                                
                    B = 3*(f3-f2)-z3*(d3+2*d2)
                    z2 = (sqrt(B*B-A*d2*z3*z3)-B)/A}
                if (is.na(z2)|is.infinite(z2)){z2 = z3/2}
                z2 = max(min(z2, INT*z3),(1-INT)*z3)
                z1 = z1 + z2
                X = X + z2[1]*s
            
                JandG = FUN(X, ...)
#                eval = f(X)
                f2 = JandG$J
                df2 = JandG$grad
                M = M - 1; i = i + (length<0)
                d2 = t(df2)%*%s
                z3 = z3 - z2
            }
            if (f2 > (f1+z1*RHO*d1) | d2 > -SIG*d1) {
                break
            } else if (d2 > SIG*d1) {
                success = 1; break
            } else if (M ==0) {
                break
            }
            A = 6*(f2-f3)/z3+3*(d2+d3)
            B = 3*(f3-f2)-z3*(d3+2*d2)
            z2 = -d2*z3*z3/(B+sqrt(B*B-A*d2*z3*z3))
            if (is.na(z2)|is.infinite(z2)|z2<0) {
                if (limit <-0.5) {
                    z2 = z1*(EXT-1)
                } else {
                    z2 = (limit-z1)/2
                }
            } else if ((limit > -0.5) & ((z2+z1) > limit)){
                z2 = (limit-z1)/2
            } else if ((limit < -0.5) & ((z2+z1) > z1*EXT)) {
                z2 = z1*(EXT-1)
            } else if (z2 < (-z3*INT)) {
                z2 = -z3*INT
            } else if ((limit > -0.5) & (z2 < (limit-z1)*(1.0-INT))) {
                z2 = (limit - z1)*(1 - INT)
            }
            f3 = f2; d3 = d2; z3 = -z2; 
            z1 = z1 + z2; X = X + z2[1]*s;
#            eval = f(X)
            JandG = FUN(X, ...)
            f2 = JandG$J
            df2 = JandG$grad
            M = M - 1; i = i + (length<0)
            d2 = t(df2)%*%s
        }
        
        if (success) {
            f1 = f2; fX = rbind(fX, f1)
            message(sprintf('Iteration %4i | Cost: %4.6e ',i,f1))
            temp = (t(df2)%*%df2-t(df1)%*%df2)/(t(df1)%*%df1)
            s = temp[1]*s - df2
            tmp = df1; df1 = df2; df2 = tmp;
            d2 = t(df1)%*%s
            if (d2>0) {
                s = -df1
                d2 = -t(s)%*%s
            }
            z1 = z1 * min(RATIO, d1/(d2-2.2251e-308))
            d1 = d2
            ls_failed = 0
        } else {
            X = X0; f1 = f0; df1 = df0;
            if (ls_failed|i > abs(length)) {
                break
            }
            tmp = df1; df1 = df2; df2 = tmp;
            s = -df1
            d1 = -t(s)%*%s
            z1 = 1/(1-d1)
            ls_failed = 1
        }
    }
    return(list(par=X,cost=fX))
}
