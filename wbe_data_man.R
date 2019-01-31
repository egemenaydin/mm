dt <- read.csv("flow_data.csv")

dt[dt == 0] <- NA

dt$Date <- gsub('.{9}$', '', dt$Date)

dt <- na.omit(dt)

flow_dt <- data.frame(aggregate(. ~ Date, data = dt, FUN = mean))

flow_dt2 <- flow_dt[order(as.Date(flow_dt$Date, format = "%d/%b/%y")),]

wwq <- read.csv("wwq_data.csv")

bod <- dplyr::filter(wwq, Analyte == "DBO5_Carbonee")

bodN <-dplyr::filter(bod, grepl("NORD", Sampnam)) 

bodS <-dplyr::filter(bod, grepl("SUD", Sampnam))

dt_N <- dplyr::select(flow_dt2, Date, North)

dt_N <- rowr::cbind.fill(dt_N, bodN$Resultat, fill = NA)

colnames(dt_N) <- c("Date", "Flow (m3/s)", "BOD (mg/L)")

dt_N$'Flow (L/day)' <- dt_N$`Flow (m3/s)`*86400000

dt_N$'BOD_Load (g/day)' <- dt_N$`BOD (mg/L)`*dt_N$`Flow (L/day)`/1000

dt_N$Population <- dt_N$`BOD_Load (g/day)`/80

write.csv(dt_N, "MTL_N.csv")

dt_S <- dplyr::select(flow_dt2, Date, South)

dt_S <- rowr::cbind.fill(dt_S, bodS$Resultat, fill = NA)

colnames(dt_S) <- c("Date", "Flow (m3/s)", "BOD (mg/L)")

dt_S$'Flow (L/day)' <- dt_S$`Flow (m3/s)`*86400000

dt_S$'BOD_Load (g/day)' <- dt_S$`BOD (mg/L)`*dt_S$`Flow (L/day)`/1000

dt_S$Population <- dt_S$`BOD_Load (g/day)`/80

write.csv(dt_S, "MTL_S.csv")
