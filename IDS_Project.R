install.packages("dplyr")
install.packages("gridExtra")

library(gridExtra)
library(ggplot2)
library(Pareto)

set.seed(100)


#task 1
Data <- data.frame(x.n = rnorm(50000), 
                   x.p = rPareto(50000, t = 1, alpha = 2))


histogram1 <- ggplot(data = Data, aes(x = x.n)) +
              geom_histogram(bins = 50, fill = "blue", color="black") +
              labs(x = "x.n", y = "Frequency", title = "Histogram of x.n")
boxplot1 <- ggplot(data=Data, aes(x = x.n)) +
            geom_boxplot(fill = "blue", outlier.colour = 'red') +
            labs(x = "x.n", title = "Boxplot of x.n")
grid.arrange(histogram1, boxplot1)


mean(Data$x.n)
sd(Data$x.n)


histogram2 <- ggplot(data = Data, aes(x = x.p)) +
              geom_histogram(bins = 50, fill = "blue", color = "black") +
              labs(x = "x.p", y = "Frequency", title = "Histogram of x.p") +
              geom_vline(xintercept = 3, color = "red")
boxplot2 <- ggplot(data = Data, aes(x = x.p)) +
            geom_boxplot(fill = "blue", outlier.color = "red") +
            labs(x = "x.p", title = "Boxplot of x.p")
grid.arrange(histogram2, boxplot2)


mean_xp <- mean(Data$x.p)
sd_xp <- sd(Data$x.p)

cutoff_up <- mean_xp + 3*sd_xp
cutoff_down <- mean_xp - 3*sd_xp
subset <- dplyr::filter(Data, (x.p >= cutoff_down) & (x.p <= cutoff_up))

print(paste("Mean of x.p full:", mean_xp))
print(paste("Standard Deviation of x.p full:", sd_xp))
print(paste("Mean of x.p subset:", mean(subset$x.p)))
print(paste("Standard Deviation of x.p subset:", sd(subset$x.p)))





#task 2
carData <- read.csv("Data/Car_data.csv", na.strings = c("?") )
carDataCleaned <- carData[!is.na(carData$price), ]

ggplot(data = carDataCleaned , aes(x = price)) +
geom_histogram(bins = 25, fill = "blue", color = "black") +
labs(x = "Price", y = "Frequency", title = "Histogram of Price")

scatter_plots <- list()
for (var in c("curb.weight", "engine.size", "horsepower", "highway.mpg")) {
  scatter_plots[[var]] <- ggplot(subset, aes_string(x = var, y = "price")) +
                          geom_point(color = "blue") +
                          labs(x = var, y = "Price")
}
grid.arrange(scatter_plots$curb.weight, scatter_plots$engine.size, 
             scatter_plots$horsepower, scatter_plots$highway.mpg)

arr1 = c("curb.weight", "engine.size", 
         "horsepower", "highway.mpg", "price")
arr2 = c("curb.weight", "engine.size", "horsepower", "highway.mpg")
pca_data <- na.omit(subset(carDataCleaned, select = arr1))
pca_dataCleaned <- subset(pca_data, select = arr2)
pca_result <- prcomp(pca_dataCleaned, scale. = TRUE)
pca_result$rotation


pc1 = pca_result$x[, 1]
pc2 = pca_result$x[, 2]
pc3 = pca_result$x[, 3]
pc4 = pca_result$x[, 4]
Data_PC <- data.frame(pc1, pc2, pc3, pc4, price= pca_data$price)
scatter_plots2 <- list()
for (var in c("pc1", "pc2", "pc3", "pc4")) {
  scatter_plots2[[var]] <- ggplot(Data_PC, aes_string(x = var, y = "price")) +
                           geom_point(color = "blue") +
                           labs(x = var, y = "Price")
}
grid.arrange(scatter_plots2$pc1, scatter_plots2$pc2, 
             scatter_plots2$pc3, scatter_plots2$pc4)
