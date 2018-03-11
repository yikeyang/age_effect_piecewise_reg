---
title: "#reg_breakpoint_8"
author: "Yike Yang"
date: "27 October 2017"
output: html_document
---
#########
## Graphical data exploration
#########

### Load package ggplot2
```{r}
library(ggplot2)
```

### Load data
```{r}
full <- readRDS("full.rds")
```

#########
## Linear regressions with and without breakpoint at AOA = 8
#########

### Define breakpoint and shift AOA
```{r}
breakpoint = 8
full$ShiftedAoA = full$AoA - breakpoint
```

### Compute breakpoint model (before/after as interaction term)
```{r}
full$PastBreakPoint = as.factor(full$ShiftedAoA > 0)
full.with8 = lm(ba ~ ShiftedAoA:PastBreakPoint, data=full)
```

### Compare with simpler model without breakpoint
```{r}
full.without8 = lm(ba ~ ShiftedAoA, data=full)
anova(full.without8, full.with8)
```

### Inspect summaries
```{r}
summary(full.with8)
summary(full.without8)
```

#########
## Plot regression lines
#########

### Rewrite regression models first for ease of plotting
```{r}
full.with8 = lm(ba ~ ShiftedAoA:as.factor(ShiftedAoA > 0), data=full)
```

### Define domain of predictions
```{r}
newShiftedAoA <- seq(-5, 10)
```

### Predictions with breakpoint
```{r}
predictedba <- predict(full.with8, newdata=data.frame(ShiftedAoA=newShiftedAoA), interval =c("confidence"), level=.95, type="response")
```

### Predictions without breakpoint
```{r}
predictedba2 <- predict(full.without8, newdata=data.frame(ShiftedAoA=newShiftedAoA), interval =c("confidence"), level=.95, type="response")
```

### Plot regressions
solid: with breakpoints + 95% CI (dashed)
dash-dot: without breakpoints (no CI to keep things clear)
```{r}
plot(full$ShiftedAoA, full$ba, main="Regression with breakpoint at age eight", xlim=c(-5,10), ylim=c(0,12), xaxt="n", xlab="Age of acquisition", ylab="Production of ba-sentences")
axis(1,at=seq(-5,10,by=3), labels=c("3","6","9","12","15","18"))
lines(newShiftedAoA, predictedba[,1], lwd=3)
lines(newShiftedAoA, predictedba[,2], lty=2, lwd=.6)
lines(newShiftedAoA, predictedba[,3], lty=2, lwd=.6)
lines(newShiftedAoA, predictedba2[,1], lty=4, lwd=3)
```
