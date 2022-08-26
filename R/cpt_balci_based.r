exp_IET = function(Data,crit){
  
  if(Data %% 1 != 0){
    stop("Data is not integer valued.")
  }
  
  FstData = Data
  FstCum = cumsum(Data)
  CP = matrix(data = 0,nrow = length(Data),ncol = 4)
  i = 1
  R = 0
  Rec = matrix(data = 0,nrow = length(Data),ncol = 9)
  LtsTrunc = 0
  
  while(length(Data) > 2){
    CumExp = cumsum(Data)
    N = length(Data)
    n = 1
    while(n < N){
      for (r in seq(0,n,1)) {
        Tr = CumExp[r]
        E1 = Tr/(r + 1)
        
        if(r < n){
          E2 = (CumExp[n] - Tr)/(n - r)
          LL[r] = sum(log( dexp(Data[1:(r+1)],E1) ))
        }
      }
    }
  }
}