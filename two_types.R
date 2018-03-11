---
title: "#two_types"
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
## Loop through AoAs to determine optimal breakpoint for the disposal type
#########
```{r}
deviances = rep(3, 18)

for (i in 2:16) {breakpoint = i+1
full$ShiftedAoA = full$AoA - breakpoint
full$PastBreakPoint = as.factor(full$ShiftedAoA > 0)
mod.lm = lm(dis ~ ShiftedAoA:PastBreakPoint, data=full)
deviances[i] = deviance(mod.lm)}

list(deviances)
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
full.with8 = lm(dis ~ ShiftedAoA:PastBreakPoint, data=full)
```

### Compare with simpler model without breakpoint
```{r}
full.without8 = lm(dis ~ ShiftedAoA, data=full)
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
full.with8 = lm(dis ~ ShiftedAoA:as.factor(ShiftedAoA > 0), data=full)
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
plot(full$ShiftedAoA, full$dis, main="Models for AoA and production of disposal type", xlim=c(-5,10), ylim=c(0,6), xaxt="n", xlab="Age of acquisition", ylab="Production of ba-sentences")
axis(1,at=seq(-5,10,by=3), labels=c("3","6","9","12","15","18"))
lines(newShiftedAoA, predictedba[,1], lwd=3)
lines(newShiftedAoA, predictedba[,2], lty=2, lwd=.6)
lines(newShiftedAoA, predictedba[,3], lty=2, lwd=.6)
lines(newShiftedAoA, predictedba2[,1], lty=4, lwd=3)
```

#########
## Loop through AoAs to determine optimal breakpoint for the locational displacement type
#########
```{r}
deviances = rep(3, 18)

for (i in 2:16) {breakpoint = i+1
full$ShiftedAoA = full$AoA - breakpoint
full$PastBreakPoint = as.factor(full$ShiftedAoA > 0)
mod.lm = lm(loc ~ ShiftedAoA:PastBreakPoint, data=full)
deviances[i] = deviance(mod.lm)}

list(deviances)
```

#########
## Linear regressions without breakpoint 
#########

### Plot the model for disposal type
```{r}
ggplot(full, aes(x=AoA, y=dis)) + geom_point(shape=19) + geom_smooth(method = lm) +
  ggtitle("AoA and the disposal type") +
  scale_x_continuous('Age of acquisition', limits=c(3,18)) +
  scale_y_continuous('Production of disposal ba-sentences', limits=c(0,6)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

### Plot the model for locational displacement type
```{r}
ggplot(full, aes(x=AoA, y=loc)) + geom_point(shape=19) + geom_smooth(method = lm) +
  ggtitle("AoA and the displacement type") +
  scale_x_continuous('Age of acquisition', limits=c(3,18)) +
  scale_y_continuous('Production of displacement ba-sentences', limits=c(0,6)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

