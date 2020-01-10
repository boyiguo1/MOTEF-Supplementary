 newdata <- read.table("newdata.txt",header=TRUE,colClasses="character")
 ## Missing value code is NA
 ## Change file name if needed
 ## Predicting means
 predicted <- function(){
 if(!is.na(k__Bacteria.p__Tenericutes.c__Mollicutes.o__RF39) & k__Bacteria.p__Te
 nericutes.c__Mollicutes.o__RF39 <=   3.020000000000000E-005 ){
 nodeid <-            2
 if(Treat.all == "0"){
 predict <- c(
  2.9009E+01,  7.2926E-01,  1.8511E+03,  2.6719E+03,  3.6397E+02,  4.0041E+02,  2.0785E+02,  1.3256E+02,  1.0646E+02,  8.7166E+01)}
 else if(Treat.all == "1"){
 predict <- c(
  3.1694E+01,  9.8262E-01,  2.7318E+03,  4.8081E+03,  4.1078E+02,  5.0168E+02,  2.1190E+02,  1.2654E+02,  1.0316E+02,  9.9225E+01)}
 else{stop("Error")}
 } else {
 if(is.na(k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verruc) | k__Bac
 teria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verruc <= 
  1.244998900000000E-002 ){
 nodeid <-            6
 if(Treat.all == "0"){
 predict <- c(
  2.7487E+01,  6.0210E-01,  1.8502E+03,  1.7840E+03,  3.1393E+02,  5.7853E+02,  1.7430E+02,  1.0738E+02,  9.4183E+01,  9.8515E+01)}
 else if(Treat.all == "1"){
 predict <- c(
  3.0142E+01,  8.9308E-01,  3.7074E+03,  5.2578E+03,  3.6962E+02,  4.9655E+02,  1.9293E+02,  1.1418E+02,  9.1556E+01,  1.0028E+02)}
 else{stop("Error")}
 } else {
 nodeid <-            7
 if(Treat.all == "0"){
 predict <- c(
  3.1387E+01,  2.2443E+00,  1.8588E+04,  7.8993E+04,  5.6323E+02,  7.5133E+02,  1.7768E+02,  8.9263E+01,  9.0957E+01,  9.1433E+01)}
 else if(Treat.all == "1"){
 predict <- c(
  2.5680E+01,  4.5200E-01,  3.2568E+03,  1.1715E+03,  3.4733E+02,  5.0562E+02,  1.8712E+02,  1.1906E+02,  1.0347E+02,  1.0085E+02)}
 else{stop("Error")}
 }
 }
 return(c(nodeid,predict))
 }
 ## end of function
 ##
 ## node contains terminal node ID of each case
 ## pred contains predicted value of each case
 node <- NULL
 pred <- NULL
 for(i in 1:nrow(newdata)){
   Treat.all <- as.character(newdata$Treat.all[i])
   if(Treat.all %in% Treat.all.values){
     Treat.all.1 <- if(Treat.all == "1") 1 else 0
   } else {
     Treat.all.1 <- NA
   }
   k__Bacteria.p__Tenericutes.c__Mollicutes.o__RF39 <- as.numeric(newdata$k__Bac
 teria.p__Tenericutes.c__Mollicutes.o__RF39[i])
   k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verruc <- as.numeric(ne
 wdata$k__Bacteria.p__Verrucomicrobia.c__Verrucomicrobiae.o__Verruc[i])
   tmp <- predicted()
   node <- c(node,tmp[1])
   pred <- c(pred,tmp[2:11])
 }
