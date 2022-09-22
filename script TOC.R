summary(data)

data$depth <- data$`depth increment` ## simplifico el nombre de la variable depth

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
ggplot(profundidades, aes(x=depth, y=meanTOC)) +
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

# Call:
#  lm(formula = BD_fine ~ TOC, data = metro)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.14136 -0.08070  0.01929  0.10776  2.41528 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.549547   0.002023   766.0   <2e-16 ***
# TOC         -0.123723   0.001203  -102.8   <2e-16 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.1724 on 13070 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.4473,	Adjusted R-squared:  0.4472 
# F-statistic: 1.058e+04 on 1 and 13070 DF,  p-value: < 2.2e-16


