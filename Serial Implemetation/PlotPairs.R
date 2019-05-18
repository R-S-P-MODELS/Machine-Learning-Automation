
PairPlot=function(df,coresgeradas){

library(ggplot2)
library(cdata)
print(head(df))
meas_vars <- names(df)
df=data.frame(df,as.factor(coresgeradas))
print(meas_vars)
names(df)[length(names(df))]='cores'
controlTable <- data.frame(expand.grid(meas_vars, meas_vars, 
                                       stringsAsFactors = FALSE))
# rename the columns
print(controlTable)
colnames(controlTable) <- c("x", "y")
print("Vai construir")
# add the key column
controlTable <- cbind(
  data.frame(pair_key = paste(controlTable[[1]], controlTable[[2]]),
             stringsAsFactors = FALSE),
  controlTable)

print("Passou pelo primeiro teste")
iris_aug = rowrecs_to_blocks(
  df,
  controlTable,
  columnsToCopy = 'cores')

splt <- strsplit(iris_aug$pair_key, split = " ", fixed = TRUE)
iris_aug$xv <- vapply(splt, function(si) si[[1]], character(1))
iris_aug$yv <- vapply(splt, function(si) si[[2]], character(1))

iris_aug$xv <- factor(as.character(iris_aug$xv),
                           meas_vars)
iris_aug$yv <- factor(as.character(iris_aug$yv),
                           meas_vars)
print("Vai gerar a imagem")
p1<-ggplot(iris_aug, aes(x=x, y=y)) +
  geom_point(aes(color=cores)) + 
  facet_grid(yv~xv, labeller = label_both, scale = "free") +
  ggtitle("All clustering Pairs") +
  scale_color_brewer(palette = "Dark2") +
  ylab(NULL) + 
  xlab(NULL)

p1=p1 + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())

return(p1)

}
