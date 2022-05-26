
#-------------------Initialization------------------------

dist <- list(continuous = c("Normal","t","F","exp","gamma","chi-square","uniform"),discrete = c("geom","binomial","pascal","poi","hypergeo"),any = "any")
dist1 <- c("Normal","t","F","exp","gamma","chi-square","uniform","geom","binomial","pascal","poi","hypergeo","any")
names(dist1)<-dist1
# Generating samples from specific distribution
generate<-function(n=1,pop="Normal",params=list(...)){
  a<-params$par1
  b<-params$par2
  c<-params$par3
  if (pop!="any")
    stopifnot(logical(pop,a,b,c))
  switch (pop,
          "Normal" = rnorm(n,mean=a,sd=sqrt(b)),
          #正态,均值为a,方差为b
          
          "t" = rt(n,df = a, ncp = b),
          #t分布,自由度为a,非中心参数为b
          
          "F" = rf(n,df1 = a,df2 = b,ncp = c),
          #F分布,分子自由度为a,分母自由度为b,非中心参数为c
          
          "exp" = rgamma(n,shape = 1, rate = a),
          #指数分布,参数为b
          
          "gamma" = rgamma(n,shape = a, rate = b),
          #伽马分布,形状参数为a,位置参数为b
          
          "chi-square" = rchisq(n,a,ncp = b),
          #卡方分布,自由度为a,非中心参数为b
          
          "uniform" = runif(n,min=a,max=b),
          #均匀分布,下界为a,上界为b
          
          "binomial" = replicate(n,sum(sample(c(0,1),a,replace=TRUE,prob = c(1-b,b)))),
          #二项分布
          
          "geom" = rgeom(n,a)+1,
          
          "pascal" = replicate(n,sum(rgeom(a,b)+1)),
          #几何分布与帕斯卡(负二项)分布
          
          "poi" = rpois(n,lambda = a),
          # 泊松分布
          
          "hypergeo" = rhyper(n,b,a-b,k=c),
          #超几何分布
          
          "any" = replicate(n,A_R(params))
          
  )
}#生成样本
A_R<- function(params){
  repeat{
    u = runif(1,0,params$m)
    x = runif(1,params$supp[1],params$supp[2])
    if (u < rlang::eval_tidy(str2lang(params$f),data = params))
      return(x)
  }
}
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  (abs(x - round(x)) < tol)&(x>0)#判断参数范围是否合理
logical <- function(pop="Normal",a=0,b=0,c=0){
  switch (pop,
          "Normal" = ifelse((is.null(a))|(is.null(b))|(b<=0), FALSE, TRUE),
          "t" = ifelse((is.null(a))|(is.null(b))|(!is.wholenumber(a-2)), FALSE, TRUE),
          "F" = ifelse((is.null(a))|(!is.wholenumber(a)) | (!is.wholenumber(b)), FALSE, TRUE),
          "chi-square" = ifelse(is.null(a)|(!is.wholenumber(a)), FALSE, TRUE),
          "exp" = ifelse((is.null(a))|(a <= 0), FALSE,TRUE),
          "gamma" = ifelse( is.null(b) | is.null(a) | (!is.wholenumber(a)) | (b <= 0), FALSE, TRUE),
          "uniform" = ifelse( is.null(a) | is.null(b) | (a >= b), FALSE, TRUE),
          "geom" = ifelse(is.null(a) | (a <= 0) | (a >= 1), FALSE, TRUE),
          "binomial" = ifelse(is.null(a) | (!is.wholenumber(a)) | is.na(b) | (b <= 0) | (b >= 1), FALSE, TRUE),
          "pascal" = ifelse(is.null(a)|(!is.wholenumber(a)) | is.na(b) | (b <= 0) | (b >= 1), FALSE, TRUE),
          "poi" = ifelse(is.null(a) | (a <= 0), FALSE, TRUE),
          "hypergeo" = ifelse(is.null(a)|is.null(b)|is.null(c)|(!is.wholenumber(a) ) | (!is.wholenumber(b)) | (!is.wholenumber(c)) | (a<b) | (a<c), FALSE, TRUE)
  )
}
standardize <- function(n,pop,params=list(...),data){
  a<-params$par1
  b<-params$par2
  c<-params$par3
  switch (pop,
          "Normal" = (data-a)*sqrt(n)/sqrt(b),
          "t" = (data)*sqrt(n)/sqrt(a/(a-2)),
          "chi-square" = (data-a)*sqrt(n)/sqrt(2*a),
          "F" = sqrt(n)*(data-b/(b-2))/sqrt(2*b*b*(a+b-2)/(a*(b-2)*(b-2)*(b-4))),
          "exp" = a*(data-1/a)*sqrt(n),
          "gamma" = sqrt(n)*(data*b-a)/sqrt(a),
          "uniform" = sqrt(12*n)*(data-(b-a)/2)/(b-a),
          "geom" = sqrt(n)*(data-1/a)/sqrt(1/a),
          "binomial" = sqrt(n)*(data-a*b)/sqrt(a*b*(1-b)),
          "pascal" = sqrt(n)*(data-a/b)/sqrt(a/b),
          "poi" = sqrt(n)*(data-a)/sqrt(a),
          "hypergeo" = sqrt(n)*(data-c*b/a)/sqrt(b*c*(a-c)*(a-b)/(a*a*(a-1))),
          "any" = data
  )
}#标准化
parainput <- function(pop="Normal",id){
  switch (pop,
          'Normal' = tagList(numericInput(NS(id,paste0(pop,"par1")),"Mean", value = 0),
                             numericInput(NS(id,paste0(pop,"par2")), "Variance", value = 1)),
          'chi-square' = tagList(sliderInput(NS(id,paste0(pop,"par1")),"degree of freedom:", min=1, step=1, max = 100, value=1),
                                 numericInput(NS(id,paste0(pop,"par2")), "Non-central Parameter:", value = 0)),
          't' = tagList(sliderInput(NS(id,paste0(pop,"par1")),"degree of freedom:", min=1, step=1, max = 100, value=5),
                        numericInput(NS(id,paste0(pop,"par2")), "Non-central Parameter:", value = 0)),
          'F' = tagList(sliderInput(NS(id,paste0(pop,"par1")),"degree of freedom 1:", min = 1, max = 100, step = 1, value= 5),
                        sliderInput(NS(id,paste0(pop,"par2")),"degree of freedom 2:", min = 1, max = 100, step = 1, value= 10),
                        numericInput(NS(id,paste0(pop,"par3")), "Non-central Parameter:", value = 0)),
          'exp' = numericInput(NS(id,paste0(pop,"par1")),"rate:", min=0,value=1),
          'gamma' = tagList(sliderInput(NS(id,paste0(pop,"par1")),"Shape:", value = 1, min = 0, max = 100),
                            numericInput(NS(id,paste0(pop,"par2")), "Rate", value = 1, min = 0)),
          'uniform' = tagList(numericInput(NS(id,paste0(pop,"par1")),"Lower bound:", value = 0),
                              numericInput(NS(id,paste0(pop,"par2")), "Upper bounnd:", value = 1)),
          'geom' = numericInput(NS(id,paste0(pop,"par1")),"prob:", value=0.5,min = 0,max = 1),
          'binomial' = tagList(numericInput(NS(id,paste0(pop,"par1")),"number of trials:", value = 1,min = 1,step = 1),
                               numericInput(NS(id,paste0(pop,"par2")), "prob:", value = 0.5,min = 0,max = 1)),
          'pascal' = tagList(numericInput(NS(id,paste0(pop,"par1")),"stopping time:", value = 1,min = 1,step = 1),
                             numericInput(NS(id,paste0(pop,"par2")), "prob:", value = 0.5,min = 0,max = 1)),
          'poi' = numericInput(NS(id,paste0(pop,"par1")),"lambda:", value = 1, min = 0),
          'hypergeo' = tagList(numericInput(NS(id,paste0(pop,"par1")),"total balls:", min = 1, step = 1, value = 10),
                               numericInput(NS(id,paste0(pop,"par2")),"white balls:", min = 1, step = 1, value = 5),
                               numericInput(NS(id,paste0(pop,"par3")), "number of draws:", min = 1,step = 1,max = 10, value = 5)),
          "any" = tagList(sliderInput(NS(id,"nparam"), "Number of params", value = 1, min = 0, max = 5),
                          textInput(NS(id,"pdf"),"p.d.f :", value="1"),
                          sliderInput(NS(id,"supp"),"supp of p.d.f",value=c(0,1),min=-100,max=100),
                          numericInput(NS(id,"bound"),"supreme bound of p.d.f", value =1 ),
                          htmlOutput(NS(id,"paramsmenu"))) 
  )
}#Produce a parameter selection menu

jax <- function(...){
  withMathJax(paste(sep = " ","\\(",paste0(list(...),collapse = " "),"\\)"))
} #simplify math formula input
params_list <- function(id){
  default <- list(
    "es-Z" = list(h0="\\mu_0",h="\\mu",test0="test0",test1="test1",par1="\\sigma"),
    "Z" = list(h0="\\mu_0",h="\\mu",test0="test0",test1="test1",par1="\\sigma"),
    "es-Chi" = list(h0="\\sigma_0^2",h="\\sigma^2",test0="test0",test1="test1",par1="known"),
    "Chi" = list(h0="\\sigma_0^2",h="\\sigma^2",test0="test0",test1="test1",par1="known"),
    "ds-F" = list(h0="k_0",h="\\frac{\\sigma_1^2}{\\sigma_2^2}",test0="test0",test1="test1",par1="\\sigma_1^2",par2="known1",par3="known2"),
    "F" = list(h0="k_0",h="\\frac{\\sigma_1^2}{\\sigma_2^2}",test0="test0",test1="test1",par1="\\sigma_1^2",par2="known1",par3="known2"),
    "es-t" = list(h0="\\mu_0",h="\\mu",test0="test0",test1="test1",par1="\\sigma"),
    "t" = list(h0="\\mu_0",h="\\mu",test0="test0",test1="test1",par1="\\sigma"),
    "ds-Z2" = list(h0="\\mu_0",h="\\mu_1 - \\mu_2",test0="test0",test1="test1",par1="\\sigma_1",par2="\\sigma_2"),
    "Z2" = list(h0="\\mu_0",h="\\mu_1 - \\mu_2",test0="test0",test1="test1",par1="\\sigma_1",par2="\\sigma_2"),
    "ds-t2" = list(h0="\\mu_0",h="\\mu_1 - \\mu_2",test0="test0",test1="test1",par1="\\sigma"),
    "t2" = list(h0="\\mu_0",h="\\mu_1 - \\mu_2",test0="test0",test1="test1",par1="\\sigma"))
  return(default[[id]])
} #params list of default test
paramsInput <- function(id,x){
  if (x %in% c("test0","test1"))
    sliderInput(NS(id,x),x,min = 0,max=0,value=0)
  else
    if (x %in% c("known"))
      radioButtons(NS(id,x),NULL,choiceNames = list(jax("\\mu \\ unknown"),jax("\\mu \\ known")),choiceValues=c(0,1),inline=TRUE )
  else
    if (x %in% c("known1"))
      radioButtons(NS(id,x),NULL,choiceNames = list(jax("\\mu_1 \\ unknown"),jax("\\mu_1 \\ known")),choiceValues=c(0,1),inline=TRUE )
  else
    if (x %in% c("known2"))
      radioButtons(NS(id,x),NULL,choiceNames = list(jax("\\mu_2 \\ unknown"),jax("\\mu_2 \\ known")),choiceValues=c(0,1),inline=TRUE )
  else
    numericInput(NS(id,x),jax(x),value=1)
} #produce a parameter selection menu
generate2 <- function(id,func,params=list(...),size,h0){
  iter <- 500
  switch (id,
          "Z" = {
            x <- runif(iter,min = params$test - 6*params$par1 / sqrt(size$n), max = params$test + 6*params$par1 / sqrt(size$n))
            y <- func(x, mean = params$test,sd = params$par1 / sqrt(size$n) )
            list(
              x=x,
              y=y
            )
          },
          "Chi" = {
            known<-as.numeric(params$par1)
            x <- runif(iter, min = 0, 
                       max = qgamma(0.001, shape = (size$n-1+known)/2, rate = h0 / (2 * params$test), lower.tail = FALSE))
            y <- func(x, shape = (size$n-1+known)/2, rate = h0 / (2 * params$test) )
            list(
              x=x,
              y=y
            )
          },
          "F" = {
            known1<-as.numeric(params$par2)
            known2<-as.numeric(params$par3)
            x <- runif(iter, min = 0, 
                       max = h0*qf(0.001, df1 = size$n-1 + known1, df2 = size$m-1 + known2, lower.tail = FALSE))
            y <- func(x * 1/params$test, df1 = size$n-1 + known1, df2 = size$m-1 + known2)/params$test
            list(
              x=x,
              y=y
            )
          },
          "t" = {
            x <- runif(iter, min = qt(1-0.001, df = size$n-1, ncp = sqrt(size$n)*(params$test - h0)/params$par1, lower.tail = FALSE),
                       max = qt(0.001, df = size$n-1, ncp = sqrt(size$n)*(params$test - h0)/params$par1, lower.tail = FALSE))
            y <- func(x, df = size$n-1, ncp = sqrt(size$n)*(params$test - h0)/params$par1)
            list(
              x=x,
              y=y
            )
          },
          "Z2" = {
            x <- runif(iter,min = params$test - 6*sqrt(params$par1^2/size$n+params$par2^2/size$m), 
                       max = params$test + 6*sqrt(params$par1^2/size$n+params$par2^2/size$m))
            y <- func(x, mean = params$test,sd = sqrt(params$par1^2/size$n+params$par2^2/size$m))
            list(
              x=x,
              y=y
            )
          },
          "t2" = {
            x <- runif(iter, min = qt(1-0.001, df = size$n+size$m-2, 
                                      ncp = sqrt((size$n*size$m)/(size$n+size$m))*(params$test - h0)/params$par1, lower.tail = FALSE),
                       max = qt(0.001, df = size$n+size$m-2,
                                ncp = sqrt((size$n*size$m)/(size$n+size$m))*(params$test - h0)/params$par1, lower.tail = FALSE))
            y <- func(x, df = size$n+size$m-2, ncp =  sqrt((size$n*size$m)/(size$n+size$m))*(params$test - h0)/params$par1)
            list(
              x=x,
              y=y
            )
          },
  )
} #generate the graph of testing statistic according to p.d.f
param_func <- function(id){
  switch (id,
          "Z" = ,
          "Z2" = dnorm,
          "t" = ,
          "t2" = dt,
          "F" = df,
          "Chi" = dgamma,
  )
}
capture <- function(x){
  enexpr(x)
}