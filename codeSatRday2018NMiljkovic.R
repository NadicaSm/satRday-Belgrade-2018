# Code for presentation "Digital biosginal processing with R" for
# satRday 2018 Conference in Belgrade

# version:
# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32             
# status                                     
# major          3                           
# minor          4.3                         
# year           2017                        
# month          11                          
# day            30                          
# svn rev        73796                       
# language       R                           
# version.string R version 3.4.3 (2017-11-30)

library(EMD)

# sum of sines
N <- 1000
t <- seq(0, 10, length = N)
x <- sin(pi*t) + sin(3*pi*t) + sin(7*pi*t)  + 0.5*t
rez <- emd(x, t, boundary = "wave") # EMD application

# plots
png('signal.png', units='in', width=6, height=4, res=400)
plot(t, x, main = "Signal",
     type = "l")
grid()
dev.off()

png('resultEMD.png', units='in', width=6, height=4, res=400)
par(mfrow = c(rez$nimf+1, 1), mar = c(2,1,2,1)) # mar od margins (eng.)
rangeimf <- range(rez$imf)
for(ind in 1:rez$nimf) {
        plot(t, rez$imf[, ind], type="l", 
             xlab = "", ylab= "", ylim = rangeimf,
             main = paste(ind, ". IMF", sep = "")); abline(h = 0)
}
plot(t, rez$residue, xlab = "",
     ylab = "", main = "residual", type = "l",
     axes = FALSE); box()
dev.off()


# EMD for EMG signals from Pectoralis major
dat <- read.table("EMGpectoralis.txt")
head(dat)
emg <- dat$V1[1:8001] # we use first 8 s from first channel
fs <- 1000 # sampling frequency
vr <- seq(0, length(emg)/fs - 1/fs, by = 1/fs)
dev.off()

png('EMG.png', units='in', width=6, height=4, res=400)
plot(vr, emg, type = "l", 
     main = "EMG signal (Pectoralis muscle)",
     xlab = "time [s]", ylab = "amplitude [mV]")
grid()
dev.off()

rez2 <- emd(emg, vr, boundary = "wave") # apply EMD

png('EMGEMD.png', units='in', width=8, height=6, res=400)
par(mfrow = c(rez2$nimf+1, 1), mar = c(1.2,1.2,1.2,1.2)) 
rangeimf <- range(rez2$imf)
for(ind in 1:rez2$nimf) {
        plot(vr, rez2$imf[, ind], type="l", 
             xlab = "", ylab= "", ylim = rangeimf,
             main = paste(ind, ". IMF", sep = "")); abline(h = 0)
}
plot(vr, rez2$residue, xlab = "",
     ylab = "", main = "residual", type = "l",
     axes = FALSE); box()
dev.off()