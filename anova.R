library(car)

func <- function(data){
  an <- aov(x ~ g, data = data)
  normality_p <- shapiro.test(an$residuals)[[2]]
  
  variance <- leveneTest(x ~ g, data = data)
  variance_p <- variance$`Pr(>F)`[1]
  
  if (normality_p > 0.05 & variance_p > 0.05){
    anova_p <- summary(an)[[1]][1, 5]
    if (anova_p > 0.05){
      print("Failing to reject the null hypothesis that all means are equal")
    } else {
      print("Rejecting the null hypothesis that all means are equal")
      TukeyHSD(an)
    }
  } else if (normality_p <= 0.05){
    print("Data is not normally distributed")
    kruskal.test(x ~ g, data=data)
  } else if (variance_p <= 0.05){
    print("Variances are not equal")
  } else {
    print("Error")
  }
}
data3 <-data.frame(x=c(1,4,2,6,2,4,7,9,6,9,3,9,5,4,9,1,5,3,5,6), g=c("white","white","white","white","white","Black","Black","Black","Black","Black","Latino","Latino","Latino","Latino","Latino","Not","Not","Not","Not","Not"))


func(data3)

