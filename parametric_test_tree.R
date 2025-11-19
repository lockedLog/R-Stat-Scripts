library(car)

func_2 <- function(g1, g2){
  x = c(g1, g2)
  g = c(rep('group1', length(g1)), rep('group2', length(g2)))  
  data = data.frame(x, g) 
  
  an <- aov(x ~ g, data = data)
  normality_p <- shapiro.test(an$residuals)[[2]]
  
  variance <- leveneTest(x ~ g, data = data)
  variance_p <- variance$`Pr(>F)`[1]
  

  if (normality_p > 0.05 & variance_p > 0.05) {
    dep <- readline(prompt = "Enter 'i' for independent or 'd' for dependent: ")
    dep <- tolower(trimws(dep))
    
    if (dep == "i") {
      print("Running independent variable T-Test:")
      print(t.test(g1, g2, var.equal = TRUE))
      
    } else if (dep == "d") {
      print("Running dependent variable T-Test:")
      print(t.test(g1, g2, paired = TRUE))
      
    } else {
      print("Invalid dependence selection")
    }
    
  } else {
    print("Nonparametric data, running U Test:")
    print(wilcox.test(g1, g2))
  }
}


func_more <- function(data) {
  an <- aov(x ~ g, data = data)
  normality_p <- shapiro.test(an$residuals)[[2]]
  
  variance <- leveneTest(x ~ g, data = data)
  variance_p <- variance$`Pr(>F)`[1]
  
  
  if (normality_p > 0.05 & variance_p > 0.05) {
    print("Data is normal and variances are equal, running anova: ")
    anova_p <- summary(an)[[1]][1, 5]
    if (anova_p > 0.05) {
      print("Failing to reject the null hypothesis that all means are equal (P > 0.05)")
    } else {
      print("Rejecting the null hypothesis that all means are equal")
      print(TukeyHSD(an))
    }
  } else if (normality_p <= 0.05) {
    print("Data is not normally distributed, running Kruskal-Wallis Test")
    print(kruskal.test(x ~ g, data = data))
  } else if (variance_p <= 0.05) {
    print("Variances are not equal, running Kruskal-Wallis Test")
    print(kruskal.test(x ~ g, data = data))
  } else {
    print("Error: Unable to determine suitable test")
  }
}


func_run <- function(){
  choice <- readline(prompt = "For two vars (will use group1 & group2 data), enter 2, for more than 2 vars (will use data3 dataframe), enter 3: ")
  
  if (choice == "2") {
    func_2(group1, group2)
  } else if (choice == "3") {
    func_more(data3)
  } else {
    print("Invalid variable selection")
  }
}


#-----------------------------------
data3 <-data.frame(x=c(1,4,2,6,2,4,7,9,6,9,3,9,5,4,9,1,5,3,5,6), g=c("white","white","white","white","white","Black","Black","Black","Black","Black","Latino","Latino","Latino","Latino","Latino","Not","Not","Not","Not","Not"))

group1 <- c(10, 11, 9, 12, 10, 11, 9, 12)
group2 <- c(15, 14, 16, 15, 14, 16, 15, 14)

#-----------------------------------



func_run()

