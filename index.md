The Curse of Dimension
================

#### KNN

#### The Curse of Dimension

-   KNN(K Nearest Neighbor)은 분류나 회귀에 사용되는 unsupervised learning이다.
-   Nearest neighbor averaging은 큰 N값과 작은 차원(p)에 대하여는 상당히 좋은 능력을 가진다.
-   하지만 차원이 커질수록 대부분의 sample points는 해당 샘플의 edge에 가까워진다는 문제점이 있다.
-   N개의 p차원 데이터에서 원점(origin)에서 가장 가까운 점까지의 거리의 중간 값 공식은 d(p, N) = ( 1 – ( 1/2 )<sup>(1/p))</sup>(1/N) 이다.
-   실제 p차원의 unit sphere에서 random points를 생성하는 함수를 구현하여 총 1000번의 simulation을 하여 공식에서 구한 값에 수렴하는지 알아보고자 한다!!

``` r
### KNN
### The Curse of Dimension
```

``` r
generate_point<-function(N,p){
  result<-matrix(0,nrow=N,ncol=p)
  for(i in 1:N){
    x<-rnorm(p,0,1)
    x<-x/sqrt(sum(x^2)) # p차원 unit sphere surface에 있는 점

    u<-runif(1,0,1)
    d<-u^(1/p)
    x<-d*x

    result[i,]<-x
  }
  return(result)
}

min_dist<-function(data){
  dist<-apply(data, 1, function(x){sqrt(sum(x^2))}) # row
  dist[which.min(dist)]  
}


result<-function(num,N,p){
  dist<-c()
  for( i in 1:num){ # num simulation
    data<-generate_point(N,p)
    dist<-c(dist,min_dist(data))
  }
  return(c(median(dist), (1-(1/2)^(1/N))^(1/p)))
}
```

### Compare the result

``` r
result(1000, 200, 10)
```

    ## [1] 0.5649147 0.5674196

``` r
result(1000, 200, 15)
```

    ## [1] 0.6838880 0.6853875

``` r
result(1000, 200, 20)
```

    ## [1] 0.7550189 0.7532726

``` r
result(1000, 500, 10) 
```

    ## [1] 0.5214138 0.5177921

``` r
result(1000, 500, 15)
```

    ## [1] 0.6453802 0.6448177

``` r
result(1000, 500, 20)
```

    ## [1] 0.7201162 0.7195777

``` r
result(1000, 1000, 20)
```

    ## [1] 0.6935325 0.6950783

``` r
result(1000, 1000, 30)
```

    ## [1] 0.7860818 0.7846738

``` r
result(1000, 1000, 50)
```

    ## [1] 0.8647690 0.8645966

``` r
# very similar!!
```
