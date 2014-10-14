data <- read.csv("PoE_217AandD_raw_nonaveraged.csv")

rownames(data) <- make.names(data[, 1], unique = TRUE)
data$Compound <- NULL

data.frame(data, data$p <- apply(data, 1, function(x) {
        t.test(x[1:3], x[4:6], paired = TRUE)$p.value
} ))

data.frame(data, data$log_p <- apply(data, 1, function(y){
        -log10(y[7])
}))

data.frame(data, data$dif <- apply(data, 1, function(z){
        log2(mean(z[1:3], na.rm = TRUE) / mean(z[4:6], na.rm = TRUE))
}))

data$decreased <- as.factor(data$dif < -1 & data$log_p > 2)

df <- data.frame(data)

t1 <- subset(df, df$decreased == "TRUE")

f1 <- subset(df, df$decreased == "FALSE")

df$increased <- as.factor(data$dif > 1 & data$log_p > 2)

t2 <- subset(df, df$increased == "TRUE")

f2 <- subset(df, df$increased == "FALSE")

all_false <- subset(df, df$decreased == "FALSE" & df$increased == "FALSE")

library(plotly)
py <- plotly(username="egemenaydin", key="vh4bburwfl")

trace1 <- list(
        x = t1$dif,
        y = t1$log_p,
        mode = "markers",
        name = "Decreased",
        marker = list(
                color = "rgb(234, 153, 153)",
                size = 9
                ),
        type = "scatter"
        )

trace2 <- list(
        x = t2$dif,
        y = t2$log_p,
        mode = "markers",
        name = "Increased",
        marker = list(
                color = "rgb(164, 194, 244)",
                size = 9
        ),
        type = "scatter"
)

trace3 <- list(
        x = all_false$dif,
        y = all_false$log_p,
        mode = "markers",
        showlegend = FALSE,
        marker = list(
                color = "rgb(142, 124, 195)",
                size = 9
        ),
        type = "scatter"
)

data2 <- list(trace1, trace2, trace3)
layout <- list(
        xaxis = list(
                title = "log2 Fold Change", 
                showgrid = FALSE, 
                zeroline = FALSE
                font = list(
                        family = "Times New Roman"
                        size = 16
                )
        ), 
        yaxis = list(
                title = "log10 p", 
                showline = FALSE
                font = list(
                        family = "Times New Roman"
                        size = 16
                )
        )
        legend = list(
                font = list(
                family = "Times New Roman"
                size = 16
                )
        )
)
response <- py$plotly(data2, kwargs=list(layout=layout, filename="line-style", fileopt="overwrite"))
url <- response$url

browseURL(url, browser = "Firefox")
