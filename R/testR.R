

ztest <- function(size,level=0.05,mu=0,sd=1,type="=",real_val=0){
  switch (type,
          "=" = list( lower = mu+qnorm(1-level/2,lower.tail = FALSE)*sd/sqrt(size$n),
                      upper = mu+qnorm(level/2,lower.tail = FALSE)*sd/sqrt(size$n),
                      p = 2*pnorm(real_val, mu, sd/sqrt(size$n), lower.tail = (real_val < mu) )
          ),
          "<" = list( lower = -Inf,
                      upper = mu+qnorm(level,lower.tail = FALSE)*sd/sqrt(size$n),
                      p = pnorm(real_val, mu, sd/sqrt(size$n), lower.tail = FALSE )
          ),
          ">" = list( lower = mu+qnorm(1-level,lower.tail = FALSE)*sd/sqrt(size$n), 
                      upper = Inf,
                      p = pnorm(real_val, mu, sd/sqrt(size$n), lower.tail = TRUE )
          )
  )
} #operate one sample z-test

z2test <- function(size,level=0.05,mu=0,sd1=1,sd2=1,type="=",real_val=0){
  switch (type,
          "=" = list( lower = mu+qnorm(1-level/2,lower.tail = FALSE)*sqrt(sd1^2/size$n+sd2^2/size$m),
                      upper = mu+qnorm(level/2,lower.tail = FALSE)*sqrt(sd1^2/size$n+sd2^2/size$m),
                      p = 2*pnorm(real_val, mu, sqrt(sd1^2/size$n+sd2^2/size$m), lower.tail = (real_val < mu))    
          ),
          "<" = list( lower = -Inf,
                      upper = mu+qnorm(level,lower.tail = FALSE)*sqrt(sd1^2/size$n+sd2^2/size$m),
                      p = pnorm(real_val, mu, sqrt(sd1^2/size$n+sd2^2/size$m), lower.tail = FALSE)
          ),
          ">" = list( lower = mu+qnorm(1-level,lower.tail = FALSE)*sqrt(sd1^2/size$n+sd2^2/size$m),
                      upper = Inf,
                      p = pnorm(real_val, mu, sqrt(sd1^2/size$n+sd2^2/size$m), lower.tail = TRUE)
          )
  )
} #operate two samples z-test
chitest <- function(size,level=0.05,sigma=1,known_ch,type="=",real_val=0){
  known <- as.numeric(known_ch)
  switch (type,
          "=" = list( lower = qchisq(1-level/2, df = size$n-1+known, lower.tail = FALSE),
                      upper = qchisq(level/2, df = size$n-1+known, lower.tail = FALSE),
                      p = 2*pchisq(real_val, df = size$n-1+known, lower.tail = (real_val < qchisq(0.5, df = size$n-1+known)))
          ),
          "<" = list( lower = -Inf,
                      upper = qchisq(level, df = size$n-1+known, lower.tail = FALSE),
                      p = pchisq(real_val, df = size$n-1+known, lower.tail = FALSE)
          ),
          ">" = list( lower = qchisq(1-level, df = size$n-1+known, lower.tail = FALSE),
                      upper = Inf,
                      p = pchisq(real_val, df = size$n-1+known, lower.tail = FALSE)
          )
  )
} #operate chi-square test
ftest <- function(size,level=0.05,k,known1_ch,known2_ch,type="=",real_val=0){
  known1 <- as.numeric(known1_ch)
  known2 <- as.numeric(known2_ch)
  switch (type,
          "=" = list(lower = k*qf(1-level/2, df1 = size$n+known1-1, df2 = size$m+known2-1, lower.tail = FALSE),
                     upper = k*qf(level/2, df1 = size$n+known1-1, df2 = size$m+known2-1, lower.tail = FALSE),
                     p = 2*pf(real_val/k, df1 = size$n + known1-1, df2 = size$m+known2-1, 
                              lower.tail = (real_val/k < qf(0.5, df1 = size$n+known1-1, df2 = size$m+known2-1)) )
                     ),
          "<" = list( lower = -Inf,
                      upper = k*qf(level/2, df1 = size$n+known1-1, df2 = size$m+known2-1, lower.tail = FALSE),
                      p = pf(real_val/k, df1 = size$n + known1-1, df2 = size$m+known2-1, lower.tail = FALSE)
                      ),
          ">" = list( lower = k*qf(1-level, df1 = size$n+known1-1, df2 = size$m+known2-1, lower.tail = FALSE), 
                      upper = Inf,
                      p = pf(real_val/k, df1 = size$n + known1-1, df2 = size$m+known2-1, lower.tail = TRUE)
                      )
  )
} #operate F-test
ttest <- function(size,level=0.05,type="=",real_val=0){
  switch (type,
          "=" = list( lower = qt(1-level/2,df = size$n-1, lower.tail = FALSE),
                      upper = qt(level/2,df = size$n-1, lower.tail = FALSE),
                      p = 2*pt(real_val,df = size$n-1, lower.tail = (real_val<0))
          ),
          "<" = list( lower = -Inf,
                      upper = qt(level,df = size$n-1, lower.tail = FALSE),
                      p = pt(real_val,df = size$n-1, lower.tail = FALSE)
          ),
          ">" = list( lower = qt(1-level,df = size$n-1, lower.tail = FALSE), 
                      upper = Inf,
                      p = pt(real_Val,df = size$n-1, lower.tail = TRUE)
          )
  )
} #operate one sample t-test
t2test <- function(size,level=0.05,type="=",real_val=0){
  switch (type,
          "=" = list( lower = qt(1-level/2,df = size$n+size$m-2, lower.tail = FALSE),
                      upper = qt(level/2,df = size$n+size$m-2, lower.tail = FALSE),
                      p = 2*pt(real_val,df = size$n+size$m-2, lower.tail = (real_val<0))
          ),
          "<" = list( lower = -Inf,
                      upper = qt(level,df = size$n+size$m-2, lower.tail = FALSE),
                      p = pt(real_val,df = size$n-1, lower.tail = FALSE)
          ),
          ">" = list( lower = qt(1-level,df = size$n+size$m-2, lower.tail = FALSE),
                      upper = Inf,
                      p = pt(real_val,df = size$n-1, lower.tail = TRUE)
                      
          )
  )
} #operate two samples t-test
#保存检验统计量的公式
testStat <- function(id){
  switch (id,
          "Z" = expr(mean(x)),
          "Chi" = expr((n-1)*sd(x)^2/h0),
          "t" = expr(sqrt(n)*(mean(x)-h0)/sd(x)),
          "Z2" = expr(mean(x)-mean(y)),
          "t2" = expr((mean(x)-mean(y)-h0)*sqrt(m*n/(m+n))/sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(n+m-2))),
          "F" = expr(sd(x)^2/sd(y)^2)
  )
}
#计算检验统计量
Call_testStat <- function(id,data,size){
  req(data())
  moduleServer(id,function(input,output,session){
    test_expr <- testStat(id)
    rlang::eval_tidy(test_expr,data(),env(h0 = input$h0, n = size()$n, m = size()$m))
  })
}