library(ggplot2)
library(qdap)
df <- read.csv("formula.csv")

df$Formula <- gsub("([0-9])([A-Z])", "\\1~\\2", df$Formula)
df$Formula <- gsub("([A-Z])([A-Z])", "\\1~\\2", df$Formula)
df$Formula <- gsub("([A-Z])([A-Z])", "\\1~\\2", df$Formula)
df$Formula <- paste0(df$Formula, "~")
df$Formula <- gsub("([A-Z])(~)", "\\11\\2", df$Formula)
df$Formula <- gsub("(Cl)", "X", df$Formula)
df$Formula <- gsub("(Hg)", "X", df$Formula)

df$C <- as.numeric(genXtract(df$Formula, "C", "~"))
df$H <- as.numeric(genXtract(df$Formula, "H", "~"))
df$O <- as.numeric(genXtract(df$Formula, "O", "~"))

df[is.na(df)] <- 0

df$OC <- df$O/df$C
df$HC <- df$H/df$C

ggplot(df, aes(df$HC, df$OC)) + geom_point()
