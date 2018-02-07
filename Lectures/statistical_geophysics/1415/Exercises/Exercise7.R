
> load("hubble.rda")
> View(hubble)
> b=lm(velocity~-1+distance,data=hubble)
> summary(b)

Call:
  lm(formula = velocity ~ -1 + distance, data = hubble)

Residuals:
  Min     1Q Median     3Q    Max 
-736.5 -132.5  -19.0  172.2  558.0 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
distance   76.581      3.965   19.32 1.03e-15 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 258.9 on 23 degrees of freedom
Multiple R-squared:  0.9419,  Adjusted R-squared:  0.9394 
F-statistic: 373.1 on 1 and 23 DF,  p-value: 1.032e-15
> coef(b)
distance 
76.58117 
> beta1=coef(b)
> (1/beta1*3.09*10^(19))/(3.16*10^7)
distance 
12768779523 
> e=residuals(b)
> hist(e,freq=FALSE)
> lines(density(e))
> ks.test(e,"pnorm")
One-sample Kolmogorov-Smirnov test
data:  e
D = 0.5833, p-value = 2.734e-08
alternative hypothesis: two-sided
> plot(b)
Hit <Return> to see next plot: 
  Hit <Return> to see next plot: 
  Hit <Return> to see next plot: 
  Hit <Return> to see next plot: 
> b2=lm(velocity~-1+distance+I(distance^2),data=hubble)
> summary(b)

Call:
  lm(formula = velocity ~ -1 + distance, data = hubble)

Residuals:
  Min     1Q Median     3Q    Max 
-736.5 -132.5  -19.0  172.2  558.0 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
distance   76.581      3.965   19.32 1.03e-15 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 258.9 on 23 degrees of freedom
Multiple R-squared:  0.9419,  Adjusted R-squared:  0.9394 
F-statistic: 373.1 on 1 and 23 DF,  p-value: 1.032e-15
#model selection
> AIC(b,b2)
df      AIC
b   2 337.8030
b2  3 338.9533
> BIC(b,b2)
df      BIC
b   2 340.1591
b2  3 342.4875

#model selection with cross validation
> n=5000
> x1=runif(n)
> x2=runif(n)
> x3=runif(n)
> x4=runif(n)
> y=1.2+0.3*x1-0.5*x2+0*x3+0*x4+rnorm(n,sd=0.3)
> plot (y)
> > b=step(lm(y~x1+x2+x3+x4))
Start:  AIC=-12043.89
y ~ x1 + x2 + x3 + x4

Df Sum of Sq    RSS    AIC
- x4    1     0.124 448.85 -12044
<none>              448.73 -12044
- x3    1     0.330 449.06 -12042
- x1    1    36.702 485.43 -11653
- x2    1    93.928 542.66 -11096

Step:  AIC=-12044.51
y ~ x1 + x2 + x3

Df Sum of Sq    RSS    AIC
<none>              448.85 -12044
- x3    1     0.345 449.20 -12043
- x1    1    36.719 485.57 -11653
- x2    1    93.929 542.78 -11096

