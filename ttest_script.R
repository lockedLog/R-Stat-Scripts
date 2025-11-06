group1 <- c(1, 2, 4, 5, 6)
group2 <- c(1, 2, 3, 4, 5)

t <- function() {
  dep <- readline(prompt = "Enter 'i' for independent or 'd' for dependent: ")
  dep <- tolower(trimws(dep))
  
  if (dep == "i") {
    print(t.test(group1, group2, var.equal=TRUE))
  } else if (dep == "d") {
    print(t.test(group1, group2, paired=TRUE))
  } else {
    print("Invalid dependence selection")
  }
}

t()
