---
title: "#reg_breakpoint_6"
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

### Graph with loess smoother
```{r}
qplot(data=full, AoA, ba, geom=c("point", "smooth"), main="Cantonese learners' production")+
  scale_x_continuous('Age of acquisition', limits=c(3,18)) +
  scale_y_continuous('Production of ba-sentences', limits=c(0,12)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

### Graph without loess smoother
```{r}
ggplot(full, aes(x=AoA, y=ba)) + geom_point(shape=19) + geom_smooth(method = lm) +
  ggtitle("Cantonese learners' production") +
  scale_x_continuous('Age of acquisition', limits=c(3,18)) +
  scale_y_continuous('Production of ba-sentences', limits=c(0,12)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

#########
## Linear regressions with and without breakpoint at AOA = 6
#########

### Define breakpoint and shift AOA
```{r}
breakpoint = 6
full$ShiftedAoA = full$AoA - breakpoint
```

### Compute breakpoint model (before/after as interaction term)
```{r}
full$PastBreakPoint = as.factor(full$ShiftedAoA > 0)
full.with6 = lm(ba ~ ShiftedAoA:PastBreakPoint, data=full)
```

### Compare with simpler model without breakpoint
```{r}
full.without6 = lm(ba ~ ShiftedAoA, data=full)
anova(full.without6, full.with6)
```

### Inspect summaries
```{r}
summary(full.with6)
summary(full.without6)
```

#########
## Plot regression lines
#########

### Rewrite regression models first for ease of plotting
```{r}
full.with6 = lm(ba ~ ShiftedAoA:as.factor(ShiftedAoA > 0), data=full)
```

### Define domain of predictions
```{r}
newShiftedAoA <- seq(-3, 12)
```

### Predictions with breakpoint
```{r}
predictedba <- predict(full.with6, newdata=data.frame(ShiftedAoA=newShiftedAoA), interval =c("confidence"), level=.95, type="response")
```

### Predictions without breakpoint
```{r}
predictedba2 <- predict(full.without6, newdata=data.frame(ShiftedAoA=newShiftedAoA), interval =c("confidence"), level=.95, type="response")
```

### Plot regressions
solid: with breakpoints + 95% CI (dashed)
dash-dot: without breakpoints (no CI to keep things clear)
```{r}
plot(full$ShiftedAoA, full$ba, main="Regression with breakpoint at age six", xlim=c(-3,12), ylim=c(0,12), xaxt="n", xlab="Age of acquisition", ylab="Production of ba-sentences")
axis(1,at=seq(-3,12,by=3), labels=c("3","6","9","12","15","18"))
lines(newShiftedAoA, predictedba[,1], lwd=3)
lines(newShiftedAoA, predictedba[,2], lty=2, lwd=.6)
lines(newShiftedAoA, predictedba[,3], lty=2, lwd=.6)
lines(newShiftedAoA, predictedba2[,1], lty=4, lwd=3)
```

#########
## Loop through AoAs to determine optimal breakpoint
#########
```{r}
deviances = rep(3, 18)

for (i in 2:16) {breakpoint = i+1
  full$ShiftedAoA = full$AoA - breakpoint
  full$PastBreakPoint = as.factor(full$ShiftedAoA > 0)
  mod.lm = lm(ba ~ ShiftedAoA:PastBreakPoint, data=full)
  deviances[i] = deviance(mod.lm)}

which(deviances==min(deviances))
```

