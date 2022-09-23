library(readr)
data <- read_delim("data_soils.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE)
summary(data)
data$depth <- data$`depth increment`


plot(data$TOC ~ data$depth) ## falta de datos por debajo de 1 metro


library(dplyr)

metro <- data %>%
  filter(depth <= 100)


profundidades <- metro %>%
  group_by(depth) %>%
  summarise(
    meanTOC = mean(TOC),
    sdTOC = sd(TOC))

library(ggplot2)
ggplot(metro, aes(x=TOC, y=BD_fine)) +
  geom_point() +
  geom_smooth()


modelo <- lm(TOC ~ depth, data=metro)

summary(modelo)

# Call:
#  lm(formula = TOC ~ depth, data = metro)

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.9472 -0.5869 -0.2339  0.2620  7.7680 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.3802925  0.0172151  138.27   <2e-16 ***
# depth       -0.0246133  0.0002891  -85.14   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 1.006 on 13071 degrees of freedom
# Multiple R-squared:  0.3567,	Adjusted R-squared:  0.3567 
# F-statistic:  7248 on 1 and 13071 DF,  p-value: < 2.2e-16


modelo2 <- lm(BD_fine ~ TOC, data = metro)

summary(modelo2)



library("performance")
check_model(modelo2)
