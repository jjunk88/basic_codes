
# datasets for one million(m개의 wildtype에서 n개의 mutant type fragment(n/m % tumor contents)가 섞인 dataset 제작)
dataset_adv2 <- function(n,m){ 
  a <- c(rep(1, times=n),rep(0, times=(m-n)))
  b <- a[sample(length(a))]
  return(b)
}

# data에서 n번 sampling 후 벡터로 제작 후 sum of them으로 1000번 안에서 probablity계산(m개의 windows)
resample_adv <- function(data,n,m) {
  counter <- 0
  for (i in 1:1000){
    vec=NULL
    for (j in 1:n) {
      c = sample(data,m,replace=F)
      vec <- c(vec, c)
    }
    if (sum(vec) > 1){
      counter <- counter + 1
    }
  }
  prob <- counter/1000
  return(prob)
}

# Let's do simulation!!!(n개의 mutation을 검출할때, tumor fraction 0.01%-10% 구간에서의 the probablity of detection(1000x100))
binomial_test_adv <- function(n){
  vec=NULL
  for (i in seq(1.5,1515,by=0.1)){
    a <- dataset_adv2(i,15150)
    b <- resample_adv(a,1000,n)
    print(b)
    vec = rbind(vec, b)
    if (b == 1){
      break
    }
  }
  return(b)
}
