jkwid <- as.data.table(read.csv("gnet_3d_-_register.csv",encoding = "UTF-8", sep = ","))
colnames(jkwid) <- c("datetime","accept","workerId","age","gen",                                                                                                                                                                
                     "school","health","sleep","coffee","smoke",                                                                                     
                     "alcdrug","colorvision")
jkwid <- jkwid[!is.na(age),]
