library(haven)
library(dplyr)

d <- read_sav("./r project/merged_r5_data.sav")

INVALID  <- c(-1, 9, 988) # -1: missing, 9: don't know, 988: refused to answer

VA <- c("Food", "Water", "Medecin", "Crime")
VB <- c("0=Never", "1=Just once or twice", "2=Several times", "3=Many times", "4=Always")

d_2 <- select(d, Q8A, Q8B, Q8C, Q9B)
d_2 <- d_2[!(d$Q8A %in% INVALID | d$Q8B %in% INVALID | d$Q8C %in% INVALID | d$Q9B %in% INVALID), ]
View(d_2)

Food    <- summary(factor(d_2$Q9A))
Water   <- summary(factor(d_2$Q9B))
Medecin <- summary(factor(d_2$Q8C))
Crime   <- summary(factor(d_2$Q9B))

mat  <- cbind(Food, Water, Medecin, Crime)

data <- as.data.frame(mat)
rownames(data) <- VB
View(data)

tdata <- as.data.frame(t(mat))
names(tdata) <- c(0, 1, 2, 3, 4)
rownames(tdata) <- VA
View(tdata)

ggplot(data, aes(x = "", y = data$Food, fill = VB)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_label(aes(label = VB), position = position_stack(vjust = 0.5)) +
  labs(y = "", x = "")

ggplot(tdata, aes(x = "", y = tdata$`0`, fill = VA)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_label(aes(label = VA), position = position_stack(vjust = 0.5)) +
  labs(y = "", x = "")

ggplot(tdata, aes(x = VA, y = tdata$`0`, fill = VA)) +
  geom_histogram(stat="identity") +
  coord_flip() +
  labs(y="", x="")

ggplot(data, aes(x = VB, y = data$Crime, fill = VB)) +
  geom_histogram(stat="identity") +
  coord_flip() +
  labs(y="", x="")
