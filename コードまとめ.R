# ゼロから作るDeep Learning 5

#---------- パッケージ ----------
library(tidyverse)



#---------- 第一章 ----------

#####norm_dist####
normal = function(x, mu=0, sigma = 1)
{
  y = 1/sqrt(2*pi*sigma)*exp(-0.5*(x-mu)^2/sigma)
  return(y)
}

x = seq(-5,5,0.01)
y = normal(x)

ggplot() + geom_line(aes(x=x,y=y))

#####norm_params####

#平均を変更した場合
x = seq(-10,10,0.01)
y0 = normal(x, mu=-3)
y1 = normal(x, mu=0)
y2 = normal(x, mu=5)

plot_dat = bind_cols(x,y0,y1,y2)
names(plot_dat) = c("x", "mu=-3", "mu=0", "mu=5")

ggplot(plot_dat) + aes(x=x) + 
  geom_line(aes(y = !!as.name('mu=-3'), colour = 'mu=-3')) +
  geom_line(aes(y = !!as.name('mu=0'), colour = 'mu=0')) +
  geom_line(aes(y = !!as.name('mu=5'), colour = 'mu=5')) +
  labs(x = "x", y = "y")

#分散を変更した場合
x = seq(-10,10,0.01)
y0 = normal(x, sigma=0.5^2)
y1 = normal(x, sigma=1^2)
y2 = normal(x, sigma=2^2)

plot_dat = bind_cols(x,y0,y1,y2)
names(plot_dat) = c("x", "sigma=0.5", "sigma=1", "sigma=2")

ggplot(plot_dat) + aes(x=x) + 
  geom_line(aes(y = !!as.name('sigma=0.5'), colour = 'sigma=0.5')) +
  geom_line(aes(y = !!as.name('sigma=1'), colour = 'sigma=1')) +
  geom_line(aes(y = !!as.name('sigma=2'), colour = 'sigma=2')) +
  labs(x = "x", y = "y")

#####sample_avg####

#準備
N = 3
xs = c()
for(n in 1:N) 
{
  x = runif(1)
  xs = c(xs,x)
}

x.mean = mean(xs)
print(x.mean)


# sample_avg
x_mean = c()
N = 10 #任意の数字に設定してください。

for(iter1 in 1:10000)
{
  xs = c()
  for(iter2 in 1:N)
  {
    x = runif(1)
    xs = c(xs,x)
  }
  x.mean = mean(xs)
  x_mean = c(x_mean,x.mean)
}

ggplot() + geom_histogram(aes(x = x_mean, y = ..density..),bins = 100) + 
  labs(title = paste0("N=",N),x = "x", y = "probability Density") +
  theme(plot.title = element_text(hjust = 0.5 ))

# sample_sum
x_sum = c()
N = 10 #任意の数字に設定してください。

for(iter1 in 1:10000)
{
  xs = c()
  for(iter2 in 1:N)
  {
    x = runif(1)
    xs = c(xs,x)
  }
  x.sum = sum(xs)
  x_sum = c(x_sum,x.sum)
}

x_norm = seq(0,10,0.01)
x_norm_density = normal(x_norm,mu = N/2, sigma = N/12)

ggplot() + 
  geom_histogram(aes(x = x_sum, y = ..density..),bins = 100) + 
  geom_line(aes(x = x_norm, y = x_norm_density), colour = "red", linewidth = 2) +
  labs(title = paste0("N=",N),x = "x", y = "probability Density") +
  theme(plot.title = element_text(hjust = 0.5 )) +
  xlim(c(0,10)) + ylim(c(0,0.5))



#---------- 第二章 ----------

