library(xlsx)
library(ggplot2)
library(formattable)
library(extrafont)
library(scales)
windowsFonts(Times=windowsFont("TT Times New Roman"))

#datasim parameters
set.seed(1) #Set seed so result is reproducable
n = 1000    #Sample size
M = 50      #Variable Means
SD = 10     #Variable SD
deci = 0    #Decimals to round data
r = .5

num.col = 5 #Number of columns desired for quantile plot
x.axis.title = "Predicted Criterion Quantiles"

y.axis.limit.low = 40
y.axis.limit.high = 60  
y.axis.limits = c(y.axis.limit.low,y.axis.limit.high)
y.axis.title = "Mean Actual Criterion Score"


# Correlated Data Sim -----------------------------------------------------

names = c("actu","pred")
nvars = length(names)

R = matrix(cbind(  1,r,  
                 r,  1  
                 ),nrow=nvars)

U = t(chol(R))
nvars = dim(U)[1]
random.normal = matrix(rnorm(nvars*n,0,1), nrow=nvars, ncol=n);
data = as.data.frame(t(U %*% random.normal))
data = round(data*SD+M, digits = deci)
names(data) = names
rm(U,R,random.normal,names,M,n,nvars,SD,deci)

# Correlated Data Sim -----------------------------------------------------

data$quant <- as.numeric(cut(data$pred, quantile(data$pred, probs = seq(0,1,1/num.col)), include.lowest=TRUE))

plot1 = ggplot(data) +
  #labs(title = "Mean Actual Criterion Score by Predicted Criterion Quartiles") +
  scale_y_continuous(name=y.axis.title, limits = y.axis.limits, oob = rescale_none) + 
  scale_x_continuous(name=x.axis.title, oob = rescale_none) +

  geom_bar(aes(x = quant, y = actu), position = "dodge", stat = "summary", fun.y = "mean",
           color = 'black',
           fill = '#006747',
           width = .5) +
  theme(text = element_text(size = 14, family = "Times"),
        panel.background = element_rect(fill = "white", color = "black"),
        axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black')
  )
plot1

#write.xlsx(data, "C:\\Users\\AJ Thurston\\Desktop\\data2.xlsx")



#means = aggregate(data, list(data$quant), mean)
  


