a<-as.numeric("7")
test<-is.numeric(a)
print(test)

vec <- matrix(0,nrow = 10, ncol = 1)
vec[1,1] = 10
vec[2,1] = 14
vec[3,1] = 10
vec[4,1] = 999999
vec[5,1] = 999999
vec[6,1] = 999999
vec[7,1] = 625
vec[8,1] = 999999
vec[9,1] = 012
vec[10,1] = 10

print(vec)
tmp_before <-0
tmp_after <- 0
for (i in 1:length(vec))
{
  if (vec[i,1] != 999999)
  {
    tmp_before <- as.numeric(vec[i,1])
  }
  else
  {
    incr <- i
    while(vec[incr,1]==999999)
    {
          
          incr <- incr + 1
    }
    print(i)
    print(incr)
     tmp_after <- as.numeric(vec[incr,1])

     
     for(j in i:(incr-1))
     {
       vec[j,1] <- (tmp_before+tmp_after)/2
       
     }
  }
}

print(vec)

