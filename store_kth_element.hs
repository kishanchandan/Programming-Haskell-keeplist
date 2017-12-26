keepl k (x:xs) 
  |k==0 = error "Cannot slice on 0" 
  |k==1 = (x:xs) -- Every element will be returned as step is 1
  |k>1 = let m=k in keeps k m (x:xs) 
  |otherwise = error "Number cannot be negative" 


len [] = 0 
len (x:xs) = 1 + len xs

keeps k m (x:xs) 
  |k==0 = []
  |k==1 = if (len(xs)/m)>=1 then x:[]++ keepl m xs else x:[]
  |otherwise = keeps (k-1) m xs
