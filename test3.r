library(dplyr,warn.conflicts=FALSE);
library(ggplot2,warn.conflicts=FALSE);
options(width=2000);

cars2<-select(mtcars,cyl,mpg);
print(cars2);

ggplot(cars2,aes(factor(cyl),mpg))+geom_violin();
ggsave("graph3.png");