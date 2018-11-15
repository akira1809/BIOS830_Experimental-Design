(resp=read.table("respiratory_exchange_ratio.txt", header = T))

resp$new= paste(resp$SUBJECT, resp$PRTCOL)

ybar_ij. = NULL
for(i in 1:length(unique(resp$new))) {
  ybar_ij.[i] =  mean(subset(resp, new == as.character(unique(new))[i])$RATE)
}

# Plot `1'
par(mar = c(5,5,4,2))
plot(1:3, ybar_ij.[1:3], ylim=c(min(ybar_ij.)-0.025, max(ybar_ij.+0.05)),type = "b", lty = "dashed", pch = "1", ylab = expression(paste(y[hit])), 
     xaxt = "n", xlab = "Protocol i", cex.axis = 1.5, cex.lab = 1.25,cex = 1.25)
lines(x = 1:3, y = ybar_ij.[4:6], type = "b", lty = "dashed", pch= "2", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[7:9], type = "b", lty = "dashed", pch= "3", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[10:12], type = "b", lty = "dashed", pch= "4", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[13:15], type = "b", lty = "dashed", pch= "5", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[16:18], type = "b", lty = "dashed", pch= "6", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[19:21], type = "b", lty = "dashed", pch= "7", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[22:24], type = "b", lty = "dashed", pch= "8", cex = 1.25)
lines(x = 1:3, y = ybar_ij.[25:27], type = "b", lty = "dashed", pch= "9", cex = 1.25)
text(x = 2.75, y = 0.95, label = "Subject h", cex = 1.25)
axis(1, 1:3, label = 1:3, cex.axis = 1.5)