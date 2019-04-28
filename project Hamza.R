library(haven)
library(dplyr)
library(reshape2)

d <- read_sav("merged_r5_data.sav")

INVALID  <- c(-1, 9, 988, 7) # -1: missing, 9: don't know, 988: refused to answer, 7: No experience with schools...

VA <- c("Expensive", "Tools", "Poor.Teaching", "Absent.Teachers", "Overcrowded.Classrooms", "Poor.Facilities")
VB <- c("Never", "Once or twice", "A few times", "Often")

d_2 <- select(d, URBRUR, Q68A, Q68B, Q68C, Q68D, Q68E, Q68F)
d_2 <- d_2[!(d$URBRUR == 3 | d$Q68A %in% INVALID | d$Q68B %in% INVALID | d$Q68C %in% INVALID | d$Q68D %in% INVALID | d$Q68E %in% INVALID | d$Q68F %in% INVALID), ]
View(d_2)

Expensive <- summary(factor(d_2$Q68A))
Tools <- summary(factor(d_2$Q68B))
Poor.Teaching <- summary(factor(d_2$Q68C))
Absent.Teachers <- summary(factor(d_2$Q68D))
Overcrowded.Classrooms <- summary(factor(d_2$Q68E)) 
Poor.Facilities <- summary(factor(d_2$Q68F))

mat <- cbind(Expensive, Tools, Poor.Teaching, Absent.Teachers, Overcrowded.Classrooms, Poor.Facilities)

data <- as.data.frame(mat)
View(t1)
t1 = as.data.frame(t(prop.table(as.matrix(t(data)), 1) * 100))
t1_reshaped <- data.frame( "",
                           independent.variable = c(t1$Expensive, t1$Tools, t1$Poor.Teaching, t1$Absent.Teachers, t1$Overcrowded.Classrooms, t1$Poor.Facilities),
                           Legend = rep(VB, times=6),
                           Variable.name = rep(VA, each = nrow(t1)))

ggplot(t1_reshaped,aes("",independent.variable,fill=Legend)) + 
  geom_bar(stat = "identity", width = 1) + 
  theme(legend.position = "right") +
  coord_polar(theta = "y") +
  geom_label(aes(label = paste(round(independent.variable,2),"%",sep="")), position = position_stack(vjust = 0.5))+
  facet_wrap(~Variable.name)+labs(y = "", x = "")

#te <- tapply(factor(d_2$Q68A), d_2$URBRUR, summary)
#ye<-as.data.frame(t(do.call("cbind", te)))
#View(ye)
URBRUR <- factor(d_2$URBRUR)
REPONSE <- factor(d_2$Q68E)
ggplot(d_2, aes(x=URBRUR)) +
  geom_bar(aes(fill=REPONSE))+
  labs(x = "URBRUR", y = "Overcrowded Classrooms") + coord_flip()
