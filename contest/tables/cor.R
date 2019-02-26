## SKG
## Feb. 26, 2019
## Correlation matrix for randome effects

library(ggplot2)
library(reshape2)



mat <- matrix(c(1, 0, .94, .93,
         0, 1, -.05, -.36,
         .94, -.05, 1, .88,
         .93, -.36, .88, 1), byrow = TRUE, nrow = 4)
colnames(mat) <- c("Australian Open", "French Open", "US Open", "Wimbledon")
rownames(mat) <- c("Australian Open", "French Open", "US Open", "Wimbledon")
#
df <- melt(mat)
#
ggplot(df, aes(x = Var1, y = Var2, fill = value)) + geom_tile() +
     geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   midpoint = 0, limit = c(-1,1), space = "Lab", 
   name="Correlation") + labs(x = "", y = "",
                              title = "Correlation matrix for random effects")
