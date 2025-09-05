# DESCRIPTION -------------------------------------------------------------
# This script is designed to create quantile plots for communicating a validity
# to non-technical audiences using bar plots.  The script imports your data
# from Excel, creates quantiles (default = 5) on the predicted score, and plots 
# the mean actual criterion score for each quantile.  It will then save the quantiles
# in a new XLSX file.
#
# The simulated data in this example have a correlation of r = .5 (N = 1000, 
# M = 50, SD = 10)
# -------------------------------------------------------------------------
library(xlsx)
library(ggplot2)
library(formattable)
library(scales)
windowsFonts(Times=windowsFont("TT Times New Roman"))

data = read.xlsx("data.xlsx", sheetName = "Sheet1")
setwd("C:\\Users\\AJThurston\\Desktop")


quants  = 5                               #Number of quantiles
x.title = "Predicted Criterion Quantiles" #X-axis title
y.ll    = 40                              #Y-axis lower limit          
y.ul    = 60                              #Y-axis upper limit
y.title = "Mean Actual Criterion Score"   #Y-axis title
text.size = 12                            #Size for all text in the plot   

# Calculates quantiles and plot results -----------------------------------
data$quant <- as.numeric(cut(data$pred, quantile(data$pred, probs = seq(0,1,1/quants)), include.lowest=TRUE))

help(cut)

plot1 = ggplot(data) +
  scale_y_continuous(name=y.title, limits = c(y.ll,y.ul), oob = rescale_none) + 
  scale_x_continuous(name=x.title, oob = rescale_none) +
  geom_bar(aes(x = quant, y = actu), position = "dodge", stat = "summary", 
           fun.y = "mean",
           color = 'black',
           fill = '#006747',
           width = .5) +
  theme(text = element_text(size = text.size, family = "Times"),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = 'black'),
        axis.text.x = element_text(color = 'black')
       )

# Write plot and quantile data to working directory -----------------------
ggsave("quantiles.png", 
       plot = plot1, 
       scale = 1, 
       width = 6.5, 
       height = 4, 
       units = "in",
       dpi = 300)

write.xlsx(data, "data.quantiles.xlsx")

#ANCILLARY FOR BLOG POST
plot(data$actu,data$pred, xlab = "Predicted", ylab = "Acutal") 
help(plot)

cor(data$actu,data$pred)
cor(data$actu,data$pred)^2

cor(data$actu,data$quant)
cor(data$actu,data$quant)^2

means = aggregate(data, by = list(data$quant), FUN = "mean")
help(aggregate)

cor(means$actu,means$pred)
cor(means$actu,means$pred)^2
