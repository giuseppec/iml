set.seed(42)
n = 100
x1 = rnorm(n)
x2 = 2 * x1 + rnorm(n)
x3 = rnorm(n)

y = 2 +  0.5 * x1 -4 * x2 + rnorm(n)


df= data.frame(x1,x2, x3, y)
mod = lm(y ~ x1 + x2 + x3, data = df)

summary(mod)

i = 4
df[i,]
res = shapley(instance.x = df[i,c('x1', 'x2', 'x3')], 
        f = function(x){predict(mod, x)}, 
        dat = df[c('x1', 'x2', 'x3')], n.runs = 1000)
res[,list(shapley.value = mean(pred.diff)),by=feature]



df[i,c('x1', 'x2', 'x3')] * coef(mod)[2:4]

# Shapley value is superior to LM betas in the sense that the 
# effect = beta * x_value only sums up the contribution of each feature 
# compared to the instance where each feature is zero or at reference level 
# for categorical variable. 
# Effect = shapley value is only true if variables are mean-centered. 