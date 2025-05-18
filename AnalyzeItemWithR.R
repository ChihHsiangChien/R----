setwd("/media/pancala/3T/試題分析/0test")
#windows 用以下路徑
#setwd("C:/Users/user/Desktop/test")  
cardReading <-read.table("test.txt",header=TRUE,row.names="學號",fill = TRUE , sep="\t",as.is=T )


colnames(cardReading) <- c("subjectCode","subjectName","grade","class","number","studentName","score","studentAnswer")
attach(cardReading)

aver <-tapply(score,class,mean,na.rm=TRUE)
stdev <- tapply(score,class,sd,na.rm=TRUE)
subj <- subjectName[1]

stuNum <- length(studentAnswer)
queNum <- nchar(as.vector(studentAnswer[1]))



#畫出對全年級的成績分析圖
png(file=paste("成績分析_",subj,"_全年級.png",sep=""), width=2500, height=3500, res=300)
par(mfrow=c(2,1)) 

boxplot(score,xlab="全年級", ylab="成績",horizontal=TRUE,main=paste(subj,"全年級成績盒鬚圖",sep=""))
abline(v=mean(score,na.rm=T),col="red")

hist(score,breaks=20,freq=FALSE,main=paste(subj,"全年級成績與常態分佈",sep=""),ylab="頻率")
curve(dnorm(x, mean=mean(score,na.rm=T), sd=sd(score,na.rm=T)), add=TRUE, col='lightgrey',lwd=2)
abline(v=mean(score,na.rm=T),col="red")

dev.off()



png(file=paste("成績分析_",subj,"_成績分佈圖.png",sep=""), width=2500, height=3500, res=300)
par(mfrow=c(2,1)) 
hist(score,breaks=10,ylab="人數",main=paste(subj,"成績分佈圖10格",sep=""))
hist(score,breaks=20,ylab="人數",main=paste(subj,"成績分佈圖20格",sep=""))
dev.off()





png(file=paste("成績分析_",subj,"_各班成績盒鬚圖.png",sep=""), width=1500, height=800, res=120)
boxplot(score~class,data=cardReading,xlab="班級", ylab="成績",na.rm=TRUE,main=paste(subj,"各班成績盒鬚圖",sep=""))
lines(tapply(score,class,mean,na.rm=TRUE),col='blue',type='b')
abline(h=mean(score,na.rm=T),col='red')
dev.off()

png(file=paste("成績分析_",subj,"_各班成績盒鬚圖_orderByMean.png",sep=""), width=1500, height=800, res=120)
bymean <- with(cardReading,reorder(class,score,mean,na.rm=TRUE))
boxplot(score ~bymean)
lines(tapply(score,bymean,mean,na.rm=TRUE),col='blue',type='b')
abline(h=mean(score,na.rm=T),col='red')
dev.off()


png(file=paste("成績分析_",subj,"_各班成績盒鬚圖_orderByMedian.png",sep=""), width=1500, height=800, res=120)
bymedian <- with(cardReading,reorder(class,score,median,na.rm=TRUE))
boxplot(score ~bymedian)
lines(tapply(score,bymedian,mean,na.rm=TRUE),col='blue',type='b')
abline(h=mean(score,na.rm=T),col='red')
dev.off()


#放學生答案進sAns陣列
sAns<-matrix(NA,stuNum,queNum)
for(i in 1:stuNum){
  for (j in 1:queNum){
    sAns[i,j]<-substr(studentAnswer[i],j,j)
  }
}


# 獲取標準答案，放在陣列standAns  sOX
standAns=matrix(NA,queNum)
sOX=matrix(NA,length(sAns[,1]),queNum)
for(j in 1:queNum) 
{
  A=1
  B=1
  C=1
  D=1
  for(i in 1:stuNum){
    if(sAns[i,j]=="A") {A="0"}
    if(sAns[i,j]=="B") {B="0"}
    if(sAns[i,j]=="C") {C="0"}
    if(sAns[i,j]=="D") {D="0"}
  }
  if(A==1){standAns[j]<-"A"}
  if(B==1){standAns[j]<-"B"}
  if(C==1){standAns[j]<-"C"}
  if(D==1){standAns[j]<-"D"}
  
  #把學生studentAnswer變成0或1，放在sOX
  for(i in 1:stuNum){
    if(sAns[i,j]==".") {
      sAns[i,j] <- standAns[j]
      sOX[i,j] <- 1
    }else {
      sOX[i,j] <- 0
    }
  }
}
##



##把sOX的列名欄名分別用學生姓名和題號命名
colnames(sOX) <- 1:queNum
rownames(sOX) <- studentName

colnames(sAns) <- 1:queNum
rownames(sAns) <- studentName




# 用25%-33%    高分組：前25％-33％，低分組：後33％-25％
#threStuNum25<- ceiling(stuNum*1/4 )
#threStuNum33<- ceiling(stuNum*1/3 )
#threStuNum <- threStuNum33-threStuNum25
# 用25%    高分組：前25％，低分組：25％
threStuNum <- ceiling(stuNum*1/4)

threStuNum
(order(rowSums(sOX)))[stuNum:threStuNum]

accep <- round(colSums(sOX)/(stuNum),4) #acceptability 通過率

#top <- (order(rowSums(sOX)))[(stuNum-threStuNum33):(stuNum-threStuNum25)] #高分組的橫行號碼
#bottom <- (order(rowSums(sOX)))[threStuNum25:threStuNum33] #低分組的橫行號碼
top <- (order(rowSums(sOX)))[(stuNum-threStuNum+1):stuNum] #高分組的橫行號碼
bottom <- (order(rowSums(sOX)))[1:threStuNum] #低分組的橫行號碼

PH <- colSums(sOX[top,])/(threStuNum) #高分組各題答對率 PH
PL <- colSums(sOX[bottom,])/(threStuNum) #低分組各題答對率  PL

diff <- round((PH+PL)/2,4) #Difficulity  PH+PL/2 難度
discim <-round(PH-PL,4) #D=(PH-PL) 鑑別度





#畫出通過率的圖
png(file=paste("問題分析_",subj,"通過率.png",sep=""), width=1500, height=850, res=120)
plot(accep,type="h",xlab="題號",ylab="通過率",main=paste(subj,"各題通過率",sep="")) 
for(i in 1:queNum){
  text(x=i,y=accep[i]+0.01,labels=i)
}
abline(h=c(0.5,0.75),col='grey')
dev.off()





#畫出難度與鑑別度的圖
png(file=paste("問題分析_",subj,"_難度與鑑別度.png",sep=""), width=3500, height=2500, res=300)

#做成2乘2的組圖
par(mfrow=c(2,2))
# png(file=paste(subj,"難度.png",sep=""), width=1500, height=850, res=120)
plot(diff,type="h",xlab="題號",ylab="難度(答對程度)",main=paste(subj,"各題難度 (PH+PL)/2",sep="")) 
for(i in 1:queNum){
  text(x=i,y=diff[i]+0.01,labels=i,cex=0.7)
}
abline(h=c(0.5,0.75),col='grey')


#畫出鑑別度的圖
# png(file=paste(subj,"鑑別度.png",sep=""), width=1500, height=850, res=120)
plot(discim,type="h",xlab="題號",ylab="鑑別度",main=paste(subj,"各題鑑別度 PH-PL",sep="")) 
for(i in 1:queNum){
  text(x=i,y=discim[i]+0.01,labels=i,cex=0.7)
}
abline(h=c(0.25,0.40),col='grey')
# dev.off()



#畫出難度遞減的圖
diff.sort <- rev(sort(diff))
diff.order <- rev(order(diff))

# png(file=paste(subj,"難度遞減.png",sep=""), width=1500, height=850, res=120)
plot(rev(sort(diff)),type="c",xlab="題數",ylab="難度(答對程度)",main=paste(subj,"難度遞減排序",sep="")) 
abline(h=c(0.5,0.75),col='grey')
abline(v=seq(10,100,10),col='grey')
for(i in 1:queNum){
  text(x=i,y=diff.sort[i],labels=diff.order[i],cex=0.7)
}
# dev.off()




#畫出鑑別度遞減的圖
discim.sort <- rev(sort(discim))
discim.order <- rev(order(discim))

# png(file=paste(subj,"鑑別度遞減.png",sep=""), width=1500, height=850, res=120)
plot(rev(sort(discim)),type="c",xlab="題數",ylab="鑑別度",main=paste(subj,"鑑別度遞減排序",sep="")) 
abline(h=c(0.25,0.40),col='grey')
abline(v=seq(10,100,10),col='grey')
for(i in 1:queNum){
  text(x=i,y=discim.sort[i],labels=discim.order[i],cex=0.7)
}
dev.off()





#Order to Student Problem Chart 注意係數(caution index)
stuHL <- order(-rowSums(sOX)) #student High to low number(top to bottom)
proHL <- order(-colSums(sOX)) #problem High to low number(left to right)

sOX.orderStu  <- sOX[stuHL,] #order by sum of student scores
sOX.order  <- sOX.orderStu[,proHL] #order by sum of question scores

##以下計算問題注意係數
averScore <- sum(rowSums(sOX))/stuNum  #學生平均答對題數
queScoreOrder <- colSums(sOX.order) #由左至右排序試題答對率，各試題的答對數

CP <- matrix(NA,queNum)
for(i in 1:queNum){
  PTopZero <- sOX.order[1:queScoreOrder[i],i]==0  #在P曲線上方答0的學生號碼
  PTopZeroSum <- sum(rowSums(sOX.order)[1:length(PTopZero)]*PTopZero) #在P曲線上方答0的學生總分之總和
  
  if(queScoreOrder[i]<stuNum){  
    PBottomOne <- sOX.order[(queScoreOrder[i]+1):stuNum,i]==1  #試題在P曲線下方答1的學生號碼
    PBottomOneSum <- sum(rowSums(sOX.order)[(queScoreOrder[i]+1):stuNum]*PBottomOne) #在P曲線下方答1的學生總分之總和
      
  }else{
    PBottomOneSum <- 0
  }

  PTop <- sOX.order[1:queScoreOrder[i],i]  #在P曲線上方學生號碼
  PTopSum <-sum(rowSums(sOX.order)[1:length(PTop)]) #在P曲線上方的所有學生總分之總和
  
  CP[proHL[i]] <- (PTopZeroSum-PBottomOneSum)/(PTopSum-queScoreOrder[i]*averScore)  #問題注意係數
  CP[proHL[i]] <- round(CP[proHL[i]],digits = 2) #四捨五入
  }




#========================================
##以下計算學生注意係數
averScorebyQue <- sum(colSums(sOX))/queNum  #試題的平均答對人數
stuScoreOrder <- rowSums(sOX.order) #由上至下排序學生答對率，各學生的答對數

CS <- matrix(NA,stuNum)
for(i in 1:stuNum){

  SLeftZero <- sOX.order[i,1:stuScoreOrder[i]]==0  #在S曲線左方答0的問題號碼
  SLeftZeroSum <- sum(colSums(sOX.order)[1:length(SLeftZero)]*SLeftZero) #在S曲線左方答0的問題總分之總和
  
  if(stuScoreOrder[i]<queNum){  
    SRightOne <- sOX.order[i,(stuScoreOrder[i]+1):queNum]==1  #在S曲線右方答1的問題號碼
    SRightOneSum <- sum(colSums(sOX.order)[(stuScoreOrder[i]+1):queNum]*SRightOne) #在S曲線右方答1的問題總分之總和
    
  }else{
    SRightOneSum <- 0
  }
  
  SLeft <- sOX.order[i,1:stuScoreOrder[i]]  #在S曲線左方之問題號碼
  SLeftSum <-sum(colSums(sOX.order)[1:length(SLeft)]) #在S左方的所有問題總分之和
  
  CS[stuHL[i]] <- (SLeftZeroSum-SRightOneSum)/(SLeftSum-stuScoreOrder[i]*averScorebyQue)  #學生注意係數
  CS[stuHL[i]] <- round(CS[stuHL[i]],digits = 2) #四捨五入
}

CS[CS=="NaN"] <- 0 #把沒有值的設定為0







#畫出問題分析 難度對應鑑別度 問題注意係數

png(file=paste("問題分析_",subj,"_難度鑑別度問題注意係數.png",sep=""), width=3500, height=2500, res=300)
par(mfrow=c(1,2)) 

plot(CP,accep,type="n",xlim=c(0,1),ylim=c(0,1),xlab="問題注意係數",ylab="試題通過率",main=paste(subj,"問題注意係數-試題通過率",sep=""))
text(x=0.05,y=1,labels="A 優良試題",cex=0.7,col='grey')
text(x=0.05,y=0,labels="B 困難試題",cex=0.7,col='grey')
text(x=1-0.05,y=1,labels="A' 異質試題",cex=0.7,col='grey')
text(x=1-0.05,y=0,labels="B'拙劣試題",cex=0.7,col='grey')
for(i in 1:queNum){
  text(x=CP[i],y=accep[i],labels=i,cex=0.6,col="blue")
  }
abline(h=0.5,v=0.5,col='grey')




#畫出鑑別度-難度的圖
plot(discim,diff,type="n",xlab="鑑別度",ylab="難度(答對程度) (PH+PL)/2",ylim=c(0,1),main=paste(subj,"鑑別度-難度",sep="")) 
#plot(diff,discim,type="n",xlab="難度(答對程度)",ylab="鑑別度",main=paste(subj,"難度-鑑別度",sep="")) 
abline(h=0.5,v=c(0.25,0.4),col='grey')
for(i in 1:queNum){
  text(x=discim[i],y=diff[i],labels=i,cex=0.6,col="blue")
}
dev.off()





#畫出學生問題的SP表
for (i in 1:max(class)){

  png(file=paste("學生分析圖",subj,"科",i,"班.png",sep=""), width=2500, height=3500, res=300)
  par(mfrow=c(2,1)) 
 
  # --- S-P Chart for PNG: Data Preparation with Stable Sort ---
  sOX_class_png <- sOX[class==i, , drop = FALSE]
  sAns_class_png <- sAns[class==i, , drop = FALSE]
  studentName_class_png <- studentName[class==i]
  studentID_class_png <- rownames(sOX_class_png) # Assuming rownames are student IDs

  classStudent <- nrow(sOX_class_png)
  if (classStudent == 0) {
    # Handle empty class if necessary, e.g., plot an empty chart or skip
    plot(0,0,type="n", xlab="", ylab="", main=paste(i,"班的",subj,"科學生問題分析圖 (無學生資料)",sep=""))
    dev.off() # Close the PNG device for this empty plot
    next # Skip to the next class
  }

  # 1. Student Sort (Stable): Score (desc), then Student ID (asc)
  class_student_scores_png <- rowSums(sOX_class_png, na.rm = TRUE)
  # Create a data frame for sorting to handle ties
  # For descending score, use -score. For ascending id, use id directly.
  class_student_order_png <- order(-class_student_scores_png, studentID_class_png)
  
  sOX1 <- sOX_class_png[class_student_order_png, , drop = FALSE]
  sAns_for_SP_png <- sAns_class_png[class_student_order_png, , drop = FALSE] # sAns for SP chart text

  # 2. Question Sort (Stable): Pass count (desc), then Original Question Number (asc)
  class_question_pass_counts_png <- colSums(sOX1, na.rm = TRUE) # Use sOX1 (already student-sorted) for pass counts
  original_q_numbers_png <- 1:queNum
  # For descending pass_count, use -pass_count. For ascending q_num, use q_num directly.
  class_question_order_png <- order(-class_question_pass_counts_png, original_q_numbers_png)

  sOX1 <- sOX1[, class_question_order_png, drop = FALSE]
  SP <- sAns_for_SP_png[, class_question_order_png, drop = FALSE] # Apply question order to sAns for SP
  standAns_ordered_for_SP_png <- standAns[class_question_order_png] # Order standard answers accordingly
  
  #加進作答資訊
  for(j in 1:queNum){
    SP[,j][SP[,j] == standAns_ordered_for_SP_png[j]] <- "" #跟標準答案一樣的，設定為空白
  }
  
  SP
  classStudent
  rowSums(sOX1)
  
  plot(1:queNum,classStudent-colSums(sOX1),
       xlim=c(0,queNum),ylim=c(0,classStudent),
       type="s",col='blue',
       xlab = "",ylab = "",
       xaxt="n",yaxt="n",
       lwd=2,
       main=paste(i,"班的",subj,"科學生問題分析圖",sep=""))
  par(new=TRUE)
  plot(rowSums(sOX1),classStudent:1,
       xlim=c(0,queNum),ylim=c(0,classStudent),
       type="s",col='red',
       xlab = "",ylab = "",
       xaxt="n",yaxt="n",
       lwd=2
       )
  #設定邊界
  #par(mar=c(4, 5, 2, 1))
  #par()$mar
  
  axis(1,at=seq(1,queNum,1),
       label=paste(original_q_numbers_png[class_question_order_png],"(",colSums(sOX1, na.rm=TRUE),")",sep=""), 
       las=2,col="blue",cex.axis=0.8)
  axis(2,at=seq(1,classStudent,1),
       label=paste(rev(rownames(sOX1)),"(",rev(rowSums(sOX1, na.rm=TRUE)),")",sep=""),  # Keep rev() here as before
       las=2,col="red",cex.axis=0.8)
  
  
  abline(v=seq(1,queNum),col="lightgrey",lty=3)
  abline(h=seq(1,classStudent),col="lightgrey",lty=3)
  
  queNum
  for (p in 1:queNum) {
    for(q in 1:classStudent) {
      # Use rev(1:classStudent) or (classStudent - q + 1) to align text with correct student order
      text(x=p, y=(classStudent - q + 1), labels = SP[q, p], cex=0.8, col='black')
    }
  }

  
  
  
  
  
  #畫出各班的學生注意分析圖表
  CS[class==i]
  studentName[class==i]
  rowSums(sOX)[class==i]/queNum*100
  
  plot(CS[class==i],rowSums(sOX)[class==i]/queNum*100,type="n",
       xlim=c(0,max(CS)),ylim=c(0,100),
       xlab="學生注意係數",ylab="答對率",
       main=paste(i,"班的",subj,"科學生分析",sep=""))
  
  #par(mar=c(4, 5, 2, 2))
  
  text(x=CS[class==i],y=rowSums(sOX)[class==i]/queNum*100,labels=studentName[class==i],cex=0.75)
  text(x=0,y=80,labels="A",cex=1,col='grey')
  text(x=0,y=60,labels="B",cex=1,col='grey')
  text(x=0,y=40,labels="C",cex=1,col='grey')
  text(x=max(CS),y=80,labels="A'",cex=1,col='grey')
  text(x=max(CS),y=60,labels="B'",cex=1,col='grey')
  text(x=max(CS),y=40,labels="C'",cex=1,col='grey')
  abline(h=c(50,75),v=0.5,col='grey')
  dev.off()
}





#========================================

student <- matrix(NA,stuNum,6)
colnames(student) <- c("班級","座號","姓名","答對率","問題注意係數","判定類別")
student[,1] <- class
student[,2] <- number
student[,3] <- studentName
student[,4] <- rowSums(sOX)/queNum
student[,5] <- CS

rowSums(sOX)/queNum
for(i in 1:stuNum){
  if(CS[i]<=0.5&&student[i,4] >=0.75) student[i,6] <- "A"
  if(CS[i]<=0.5&&student[i,4]>=0.5&&student[i,4]<0.75) student[i,6] <- "B"
  if(CS[i]<=0.5&&student[i,4]<0.5) student[i,6] <- "C"
  
  if(CS[i]>0.5&&student[i,4]>=0.75) student[i,6] <- "A'"
  if(CS[i]>0.5&&student[i,4]>=0.5&&student[i,4]<0.75) student[i,6] <- "B'"
  if(CS[i]>0.5&&student[i,4]<0.5) student[i,6] <- "C'"
  }

student

write.csv(student,paste(subj,"學生分析資料.csv",sep = ""))

#========================================





#========================================

problem <- matrix(NA,queNum,18)
colnames(problem) <- c("通過率","難度","鑑別度","問題注意係數","標準答案",
                       "所有人A","所有B","所有C","所有D",
                       "高分A","高分B","高分C","高分D",
                       "低分A","低分B","低分C","低分D",
                       "判定類別")

problem[,1] <- accep
problem[,2] <- diff
problem[,3] <- discim
problem[,5] <- standAns



#計算all students各題答ABCD的有幾人
problem[,6] <- round(colSums(sAns=="A")/stuNum,2)
problem[,7] <- round(colSums(sAns=="B")/stuNum,2)
problem[,8] <- round(colSums(sAns=="C")/stuNum,2)
problem[,9] <- round(colSums(sAns=="D")/stuNum,2)



#計算top各題答A的有幾人
problem[,10] <- round(colSums(sAns[top,]=="A")/threStuNum,2)
problem[,11] <- round(colSums(sAns[top,]=="B")/threStuNum,2)
problem[,12] <- round(colSums(sAns[top,]=="C")/threStuNum,2)
problem[,13] <- round(colSums(sAns[top,]=="D")/threStuNum,2)


#計算bottom各題答A的有幾人
problem[,14] <- round(colSums(sAns[bottom,]=="A")/threStuNum,2)
problem[,15] <- round(colSums(sAns[bottom,]=="B")/threStuNum,2)
problem[,16] <- round(colSums(sAns[bottom,]=="C")/threStuNum,2)
problem[,17] <- round(colSums(sAns[bottom,]=="D")/threStuNum,2)


problem[,4] <- CP

for(i in 1:queNum){
  if(CP[i]<=0.5&&accep[i]>=0.5)problem[i,18] <- "A"
  else if(CP[i]<=0.5&&accep[i]<0.5)problem[i,18] <- "B"
  else if(CP[i]>0.5&&accep[i]>=0.5)problem[i,18] <- "A'"
  else if(CP[i]>0.5&&accep[i]<0.5)problem[i,18] <- "B'"
}

problem
write.csv(problem,paste(subj,"問題分析資料.csv",sep = ""))
#========================================





#===============================================
problem2 <- matrix("",queNum*5,10)
problem2[seq(1,queNum*5,5),1] <- "題號"
problem2[seq(2,queNum*5,5),1] <- "判定類別"
#problem2[seq(3,queNum*5,5),1] <- ""
#problem2[seq(4,queNum*5,5),1] <- ""
#problem2[seq(5,queNum*5,5),] <- ""

problem2[seq(1,queNum*5,5),2] <- c(1:queNum)

for(i in 1:queNum){
  if(CP[i]<=0.5&&accep[i]>=0.5) problem2[(5*(i-1)+2),2] <- "A"
  else if(CP[i]<=0.5&&accep[i]<0.5)problem2[(5*(i-1)+2),2] <- "B"
  else if(CP[i]>0.5&&accep[i]>=0.5)problem2[(5*(i-1)+2),2] <- "A'"
  else if(CP[i]>0.5&&accep[i]<0.5)problem2[(5*(i-1)+2),2] <- "B'"
}
#problem2[seq(3,queNum*5,5),2] <- ""
#problem2[seq(4,queNum*5,5),2] <- ""

#problem2[seq(1,queNum*5,5),3] <- ""
problem2[seq(2,queNum*5,5),3] <- "所有人"
problem2[seq(3,queNum*5,5),3] <- "高分組"
problem2[seq(4,queNum*5,5),3] <- "低分組"

problem2[seq(1,queNum*5,5),4] <- "A"
problem2[seq(1,queNum*5,5),5] <- "B"
problem2[seq(1,queNum*5,5),6] <- "C"
problem2[seq(1,queNum*5,5),7] <- "D"


problem2[(which(standAns=="A")-1)*5+1,4] <- "A*"
problem2[(which(standAns=="B")-1)*5+1,5] <- "B*"
problem2[(which(standAns=="C")-1)*5+1,6] <- "C*"
problem2[(which(standAns=="D")-1)*5+1,7] <- "D*"


problem2[seq(2,queNum*5,5),4] <- problem[,6]
problem2[seq(2,queNum*5,5),5] <- problem[,7]
problem2[seq(2,queNum*5,5),6] <- problem[,8]
problem2[seq(2,queNum*5,5),7] <- problem[,9]

problem2[seq(3,queNum*5,5),4] <- problem[,10]
problem2[seq(3,queNum*5,5),5] <- problem[,11]
problem2[seq(3,queNum*5,5),6] <- problem[,12]
problem2[seq(3,queNum*5,5),7] <- problem[,13]

problem2[seq(4,queNum*5,5),4] <- problem[,14]
problem2[seq(4,queNum*5,5),5] <- problem[,15]
problem2[seq(4,queNum*5,5),6] <- problem[,16]
problem2[seq(4,queNum*5,5),7] <- problem[,17]

problem2[,8] <- ""

problem2[seq(1,queNum*5,5),9] <- "通過率"
problem2[seq(2,queNum*5,5),9] <- "難度"
problem2[seq(3,queNum*5,5),9] <- "鑑別度"
problem2[seq(4,queNum*5,5),9] <- "問題注意係數"

problem2[seq(1,queNum*5,5),10] <- accep
problem2[seq(2,queNum*5,5),10] <- diff
problem2[seq(3,queNum*5,5),10] <- discim
problem2[seq(4,queNum*5,5),10] <- CP

write.csv(problem2,paste(subj,"問題分析資料2.csv",sep=""))
problem2
#===============================================



#===============================================

#OUTPUT
sink(paste(subj,"基本分析.txt",sep = ""))
aver
stdev
summary(score)
stem(score)

shapiro.test(score) #檢測是否常態分佈，若p小於0.05，表示為非常態分佈
sink()
#===============================================


#平均每人得分(Average)
mean(rowSums(sOX))
#最   大   值(Maximum) 
max(rowSums(sOX))
#最   小   值(Minimum) 
min(rowSums(sOX))
#全        距(Range)
max(rowSums(sOX))-min(rowSums(sOX))
#標   準   差(SD)    
sd(rowSums(sOX))

skewness<-function(x){
  sum(((x-mean(x))^3))/length(x)
}
skewness(rowSums(sOX))


kurtosis<-function(x){
  a=mean(x)
  n=length(x)
  m4=sum((x-a)^4)/n
  m2=sum((x-a)^2)/n
  kurt=m4/m2^2 -3
  kurt
}

kurtosis(rowSums(sOX))

# ===============================================
# 產生個別化學習分析報告 (HTML 格式)
# ===============================================

# ---- 控制變數：是否在班級S-P Chart中顯示其他同學的匿名作答 (O/X) ----
show_classmates_responses_in_sp_chart <- TRUE # 設定為 TRUE 以顯示，FALSE 則只顯示顏色背景
# --------------------------------------------------------------------

# 檢查 'student' 矩陣是否存在且包含必要欄位
if (!exists("student") || !all(c("姓名", "班級", "座號", "答對率", "問題注意係數", "判定類別") %in% colnames(student))) {
  stop("必要的 'student' 矩陣或其欄位不存在，無法產生個別報告。請確認先前的分析已正確執行。")
}
if (!exists("sOX") || !exists("sAns") || !exists("standAns") || !exists("diff") || !exists("discim")) {
  stop("必要的 sOX, sAns, standAns, diff, 或 discim 變數不存在。")
}
if (!exists("aver") || !exists("score") || !exists("subj") || !exists("queNum") || !exists("stuNum")) {
  stop("必要的 aver, score, subj, queNum, 或 stuNum 變數不存在。")
}




cat("開始產生個別學習報告...\n")
for (k in 1:stuNum) {
  student_name_k <- student[k, "姓名"]
  student_id_k <- rownames(cardReading)[k] # 學號 (來自 cardReading 的 row.names)
  student_class_k <- student[k, "班級"]
  student_seat_number_k <- student[k, "座號"]

  # --- 根據班級建立個別報告的子資料夾 ---
  # 確保班級名稱在路徑中是安全的
  safe_class_name_k <- gsub("[^A-Za-z0-9_\\-]", "_", as.character(student_class_k))
  class_specific_reports_dir <- file.path(paste0(subj, "_個別學習報告"), paste0("班級_", safe_class_name_k))
  
  if (!dir.exists(class_specific_reports_dir)) {
    dir.create(class_specific_reports_dir, recursive = TRUE) # recursive = TRUE 會一併建立父資料夾 (如果不存在)
    cat(paste0("已為班級 ", student_class_k, " 建立資料夾：", file.path(getwd(), class_specific_reports_dir), "\n"))
  }
  # ------------------------------------
  
  # 確保 sOX 的 rowname 與 studentName[k] 或 student[k,"姓名"] 對應
  # 假設 sOX 的 rownames 就是 studentName
  current_student_sOX_row_idx <- which(rownames(sOX) == student_name_k)
  if(length(current_student_sOX_row_idx) == 0) {
      cat(paste0("警告：在 sOX 矩陣中找不到學生 ", student_name_k, " 的作答記錄，跳過此學生。\n"))
      next
  }
  current_student_sOX_row_idx <- current_student_sOX_row_idx[1]


  student_num_correct_k <- sum(sOX[current_student_sOX_row_idx, ], na.rm = TRUE) 
  student_correct_rate_k <- as.numeric(student[k, "答對率"]) # 直接從 student 矩陣取

  # 取得班級平均 (aver 的 names 應該是班級代號)
  class_avg_k <- aver[names(aver) == as.character(student_class_k)]
  if (length(class_avg_k) == 0) class_avg_k <- NA 

  # 全體平均
  overall_avg <- mean(score, na.rm = TRUE)

  # 找出答錯的題目
  # 再次確認 sOX 的索引方式，此處假設 sOX 的列對應到學生 k
  incorrect_q_logical_indices <- sOX[current_student_sOX_row_idx, ] == 0 & !is.na(sOX[current_student_sOX_row_idx, ])
  incorrect_q_actual_numbers <- which(incorrect_q_logical_indices) # 這些是題號 (1, 2, ...)
  
  # --- 開始建構 HTML 內容 ---
  html_content <- paste0("<!DOCTYPE html><html lang='zh-TW'><head><meta charset='UTF-8'><title>學習分析報告 - ", student_name_k, "</title>")
  html_content <- paste0(html_content, "<style>",
                         "body{font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; margin: 20px; line-height: 1.6; color: #333;}",
                         "h1, h2, h3 {color: #0056b3;}",
                         "table{border-collapse: collapse; width: 90%; margin-top:15px; margin-bottom:25px; box-shadow: 0 2px 3px rgba(0,0,0,0.1);}",
                         "th, td{border: 1px solid #ddd; padding: 10px; text-align: left;}",
                         "th{background-color: #007bff; color: white;}",
                         "tr:nth-child(even){background-color: #f9f9f9;}",
                         ".container{max-width: 90%; margin: auto; background: #fff; padding: 20px; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1);}",
                         ".info-box{background-color: #e7f3fe; border-left: 6px solid #2196F3; margin-bottom: 15px; padding: 10px 12px;}",
                         "ul {list-style-type: none; padding-left: 0;}",
                         "li b {color: #0056b3;}",
                         ".footer {text-align: center; margin-top: 30px; font-size: 0.9em; color: #777;}",
                         # CSS for Bar Chart 
                         ".chart-container { margin-top: 25px; margin-bottom: 25px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; }",
                         ".chart-container h4 { margin-top:0; margin-bottom: 15px; color: #0056b3; text-align: center; font-size: 1.1em;}",
                         ".chart-bar-group { display: flex; align-items: center; margin-bottom: 10px; }",
                         ".bar-label { width: 140px; font-size: 0.85em; color: #495057; padding-right: 10px; text-align: right; flex-shrink: 0;}",
                         ".bar-wrapper { flex-grow: 1; background-color: #e9ecef; border-radius: 4px; height: 22px; position: relative;}",
                         ".bar { height: 100%; line-height: 22px; color: white; text-align: right; padding-right: 8px; border-radius: 4px; white-space: nowrap; overflow: hidden; font-size: 0.8em; box-sizing: border-box; transition: width 0.5s ease-in-out;}",
                         ".student-bar { background-color: #28a745; }", # Green 
                         ".class-bar { background-color: #ffc107; }",   # Yellow
                         ".overall-bar { background-color: #17a2b8; }", # Teal 
                         # CSS for Scatter Plot 
                         ".scatter-plot-container { margin-top: 30px; margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; }",
                         ".scatter-plot-title { margin-top:0; margin-bottom: 15px; color: #0056b3; text-align: center; font-size: 1.1em;}",
                         ".scatter-plot-main-area { display: flex; align-items: center; justify-content: center; }",
                         ".y-axis-label-scatter { writing-mode: vertical-rl; margin-right: 10px; font-size: 0.9em; color: #333; text-align: center; }",
                         ".scatter-plot-area { border: 1px solid #ccc; border-left: 2px solid #555; border-bottom: 2px solid #555; position: relative; background-color: white; }", # Removed width and height
                         ".scatter-d-zero-line { position: absolute; bottom: 50%; left: 0; width: 100%; height: 1px; background-color: #ddd; z-index: 0; }",
                         ".scatter-point { position: absolute; width: 24px; height: 24px; background-color: rgba(220, 53, 69, 0.7); color: white; border: 1px solid rgba(220, 53, 69, 0.9); border-radius: 50%; font-size: 0.7em; display: flex; align-items: center; justify-content: center; transform: translate(-50%, -50%); z-index: 1; box-shadow: 1px 1px 3px rgba(0,0,0,0.2); cursor: default; }",
                         ".x-axis-label-scatter { text-align: center; margin-top: 8px; font-size: 0.9em; color: #333; }",
                         # Axis tick labels (simple) 
                         ".y-axis-tick-label { position: absolute; left: -30px; font-size: 0.7em; color: #555; width: 25px; text-align: right; }",
                         ".x-axis-tick-label { position: absolute; bottom: -20px; font-size: 0.7em; color: #555; text-align: center; }",
                         ".tick-d-1 { top: -5px; }", # D=1.0 
                         ".tick-d-0 { top: calc(50% - 7px); }", # D=0.0 
                         ".tick-d-neg-1 { bottom: -5px; }", # D=-1.0 
                         ".tick-p-0 { left: -5px; }", # P=0.0 
                         ".tick-p-05 { left: calc(50% - 10px); }", # P=0.5 
                         ".explanation-box { background-color: #f0f8ff; border: 1px solid #add8e6; padding: 10px 15px; margin-top: 10px; margin-bottom: 20px; border-radius: 5px; font-size: 0.9em; }",
                         ".tick-p-1 { right: -5px; left: auto; }", # P=1.0 
                         # CSS for SP Chart 
                         ".sp-chart-container { margin-top: 30px; margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; overflow-x: auto; }",
                         ".sp-chart-title { margin-top:0; margin-bottom: 15px; color: #0056b3; text-align: center; font-size: 1.1em;}",
                         ".sp-table { border-collapse: collapse; margin: 0 auto; }",
                         ".sp-table th, .sp-table td { border: 1px solid #ddd; min-width: 8px; height: 8px; text-align: center; font-size: 0.70em; padding: 2px; }", # Adjusted min-width, height, and font-size
                         ".sp-table th.sp-q-header { white-space: nowrap; /* Keep question numbers from wrapping */ }",
                         ".sp-table td.sp-correct { background-color: #e9f5e9; }", # Light green for correct 
                         ".sp-table td.sp-incorrect { background-color: #fdecea; }", # Light red for incorrect 
                         ".sp-table tr.current-student-row td { border-top: 2px solid #007bff; border-bottom: 2px solid #007bff; }",
                         ".sp-table td.current-student-cell { font-weight: bold; }",
                         ".sp-table td.current-student-incorrect { background-color: #ffc0cb; color: #a52a2a; font-weight: bold; }", # Pink for current student incorrect 
                         ".code-map-container { margin-top: 15px; padding: 10px; background-color: #f9f9f9; border: 1px solid #eee; border-radius: 4px; font-size: 0.85em; }",
                         "</style>")
  html_content <- paste0(html_content, "</head><body><div class='container'>")
  
  html_content <- paste0(html_content, "<h1>學習分析報告</h1>")
  html_content <- paste0(html_content, "<h2>科目：", subj, "</h2>")
  html_content <- paste0(html_content, "<div class='info-box'>",
                         "<p><b>學生：</b>", student_name_k, "</p>",
                         "<p><b>學號：</b>", student_id_k, "</p>",
                         "<p><b>班級：</b>", student_class_k, "班</p>",
                         "<p><b>座號：</b>", student_seat_number_k, "</p>",
                         "</div>")

  # 整體表現摘要
  html_content <- paste0(html_content, "<h3>整體表現摘要</h3><ul>")
  html_content <- paste0(html_content, "<li><b>您的得分 (答對題數/總題數)：</b>", student_num_correct_k, " / ", queNum, 
                         " (答對率: ", round(student_correct_rate_k * 100, 1), "%)</li>")
  if (!is.na(class_avg_k) && length(class_avg_k) > 0) {
    html_content <- paste0(html_content, "<li><b>您班級的平均得分：</b>", round(class_avg_k, 1), " 分 (全班 ", length(score[class==student_class_k & !is.na(score)]), " 班)</li>")
  } else {
    html_content <- paste0(html_content, "<li><b>您班級的平均得分：</b>資料暫缺</li>")
  }
  html_content <- paste0(html_content, "<li><b>全體平均得分：</b>", round(overall_avg, 1), " 分 (全體 ", length(score[!is.na(score)]), " 位)</li>")
  html_content <- paste0(html_content, "</ul>")

  # --- CSS 長條圖 ---
  student_rate_percent <- round(student_correct_rate_k * 100, 1)
  class_avg_rate_percent <- NA
  if (!is.na(class_avg_k) && queNum > 0) {
    class_avg_rate_percent <- round(class_avg_k,1)
  }
  overall_avg_rate_percent <- NA
  if (!is.na(overall_avg) && queNum > 0) {
    overall_avg_rate_percent <- round(overall_avg,1)
  }

  html_content <- paste0(html_content, "<div class='chart-container'><h4>成績比較圖 (答對率 %)</h4>")
  # 您的答對率
  html_content <- paste0(html_content, "<div class='chart-bar-group'>")
  html_content <- paste0(html_content, "<span class='bar-label'>您的答對率:</span>")
  html_content <- paste0(html_content, "<div class='bar-wrapper'><div class='bar student-bar' style='width:", student_rate_percent, "%;'>", student_rate_percent, "%</div></div>")
  html_content <- paste0(html_content, "</div>")
  # 班級平均答對率
  if (!is.na(class_avg_rate_percent)) {
    html_content <- paste0(html_content, "<div class='chart-bar-group'>")
    html_content <- paste0(html_content, "<span class='bar-label'>班級平均答對率:</span>")
    html_content <- paste0(html_content, "<div class='bar-wrapper'><div class='bar class-bar' style='width:", class_avg_rate_percent, "%;'>", class_avg_rate_percent, "%</div></div>")
    html_content <- paste0(html_content, "</div>")
  }
  # 全體平均答對率
  if (!is.na(overall_avg_rate_percent)) {
    html_content <- paste0(html_content, "<div class='chart-bar-group'>")
    html_content <- paste0(html_content, "<span class='bar-label'>全體平均答對率:</span>")
    html_content <- paste0(html_content, "<div class='bar-wrapper'><div class='bar overall-bar' style='width:", overall_avg_rate_percent, "%;'>", overall_avg_rate_percent, "%</div></div>")
    html_content <- paste0(html_content, "</div>")
  }
  html_content <- paste0(html_content, "</div>") # end chart-container

  # 答錯題目詳情
  html_content <- paste0(html_content, "<div class='explanation-box'>",
                         "<h4>關於「題目難度」與「題目鑑別度」：</h4>",
                         "<p><b>題目難度 (P值)：</b>代表這道題目對所有參與測驗的同學來說有多容易。P值越高 (越接近1.0)，表示題目越簡單，答對的人越多；P值越低 (越接近0.0)，表示題目越困難，答對的人越少。</p>",
                         "<ul><li>P值 > 0.8：通常表示非常簡單的題目。</li><li>P值 0.5 ~ 0.8：表示中等偏易的題目。</li><li>P值 0.2 ~ 0.5：表示中等偏難的題目。</li><li>P值 < 0.2：通常表示非常困難的題目。</li></ul>",
                         "<p><b>題目鑑別度 (D值)：</b>代表這道題目能不能有效地區分出學習表現較好和較弱的同學。D值越高 (越接近1.0)，表示這題越能讓高分群答對、低分群答錯；D值接近0或負數，表示高分群反而容易答錯。</p>",
                         "<p>當您查看下方答錯的題目時，可以參考這些指標，了解您錯的題目是屬於哪種類型。</p>",
                         "</div>")
  html_content <- paste0(html_content, "<h3>答錯題目詳情</h3>")
  if (length(incorrect_q_actual_numbers) > 0) {
    html_content <- paste0(html_content, "<table><tr><th>題號</th><th>您的答案</th><th>正確答案</th><th>題目難度(P)</th><th>題目鑑別度(D)</th></tr>")
    for (q_num in incorrect_q_actual_numbers) {
      # 確保 sAns 的索引方式正確
      student_ans_for_q <- sAns[current_student_sOX_row_idx, q_num]
      correct_ans_for_q <- standAns[q_num] # standAns 是向量，直接用題號索引
      
      q_difficulty <- ifelse(q_num <= length(diff), round(diff[q_num], 2), "N/A")
      q_discrimination <- ifelse(q_num <= length(discim), round(discim[q_num], 2), "N/A")
      
      extra_note <- ""
      if (!is.na(q_difficulty) && q_difficulty >= 0.80) { # 假設 P 值 >= 0.8 代表簡單題
        extra_note <- " <strong style='color:orange;'>(簡易失分，請留意！)</strong>"
      }

      html_content <- paste0(html_content, "<tr>")
      html_content <- paste0(html_content, "<td>", q_num, "</td>")
      html_content <- paste0(html_content, "<td>", student_ans_for_q, "</td>")
      html_content <- paste0(html_content, "<td>", correct_ans_for_q, "</td>")
      html_content <- paste0(html_content, "<td>", q_difficulty, "</td>")
      html_content <- paste0(html_content, "<td>", q_discrimination, "</td>")
      html_content <- paste0(html_content, "</tr>")
    }
    html_content <- paste0(html_content, "</table>")
  } else {
    html_content <- paste0(html_content, "<p>恭喜！您所有題目都答對了，或沒有被記錄為錯誤的題目。</p>")
  }

  # --- 作答代號說明 (放在答錯題目詳情表格下方) ---
  html_content <- paste0(html_content, "<div class='code-map-container'>")
  html_content <- paste0(html_content, "<h4>作答代號說明：</h4>")
  html_content <- paste0(html_content, "<ul>")
  html_content <- paste0(html_content, "<li>F = AB, G = AC, H = AD, I = AE</li>")
  html_content <- paste0(html_content, "<li>J = BC, K = BD, L = BE</li>")
  html_content <- paste0(html_content, "<li>M = CD, N = CE</li>")
  html_content <- paste0(html_content, "<li>O = DE</li>")
  html_content <- paste0(html_content, "<li>P = ABC, Q = ABD, R = ABE</li>")
  html_content <- paste0(html_content, "<li>S = ACD, T = ACE, U = ADE</li>")
  html_content <- paste0(html_content, "<li>V = BCD, W = BCE, X = BDE</li>")
  html_content <- paste0(html_content, "<li>Y = CDE</li>")
  html_content <- paste0(html_content, "<li>Z = ABCD, * = ABCE, $ = ABDE, % = ACDE, , = BCDE</li>") # 注意逗號的顯示
  html_content <- paste0(html_content, "<li># = ABCDE</li>")  
  html_content <- paste0(html_content, "<li>= (等號) 代表空白或未作答。</li>")
  html_content <- paste0(html_content, "</ul>")
  html_content <- paste0(html_content, "</div>")

  # --- CSS 散佈圖 (答錯題目之難度 vs 鑑別度) ---
  if (length(incorrect_q_actual_numbers) > 0) {
    plot_width_px <- 600
    plot_height_px <- 400

    html_content <- paste0(html_content, "<div class='scatter-plot-container'>")
    html_content <- paste0(html_content, "<h4 class='scatter-plot-title'>答錯題目 難度-鑑別度分佈圖</h4>")
    html_content <- paste0(html_content, "<div class='scatter-plot-main-area'>") # Flex container for Y-label and plot
    html_content <- paste0(html_content, "<div class='y-axis-label-scatter'>題目鑑別度</div>")
    html_content <- paste0(html_content, "<div class='scatter-plot-area' style='width:", plot_width_px, "px; height:", plot_height_px, "px;'>") # Dynamically set width and height
    html_content <- paste0(html_content, "<div class='scatter-d-zero-line'></div>") # Line for D=0

    # Y-axis tick labels
    html_content <- paste0(html_content, "<div class='y-axis-tick-label tick-d-1'>1.0</div>")
    html_content <- paste0(html_content, "<div class='y-axis-tick-label tick-d-0'>0.0</div>")
    html_content <- paste0(html_content, "<div class='y-axis-tick-label tick-d-neg-1'>-1.0</div>")
    # X-axis tick labels
    html_content <- paste0(html_content, "<div class='x-axis-tick-label tick-p-0'>0.0</div>")
    html_content <- paste0(html_content, "<div class='x-axis-tick-label tick-p-05'>0.5</div>")
    html_content <- paste0(html_content, "<div class='x-axis-tick-label tick-p-1'>1.0</div>")

    for (q_num in incorrect_q_actual_numbers) {
      # 從全域變數 diff 和 discim 獲取原始數值
      # 難度 P (diff) 範圍 0 到 1
      # 鑑別度 D (discim) 範圍 -1 到 1
      current_q_difficulty_val <- NA
      if (q_num <= length(diff)) current_q_difficulty_val <- diff[q_num]
      
      current_q_discrimination_val <- NA
      if (q_num <= length(discim)) current_q_discrimination_val <- discim[q_num]

      if (!is.na(current_q_difficulty_val) && !is.na(current_q_discrimination_val) &&
          is.finite(current_q_difficulty_val) && is.finite(current_q_discrimination_val)) {
        
        # 確保 P 在 [0,1], D 在 [-1,1]
        p_val <- max(0, min(1, current_q_difficulty_val))
        d_val <- max(-1, min(1, current_q_discrimination_val))

        # 計算 CSS 位置
        # X 軸 (難度 P): left 從 0% 到 100%
        left_percent <- p_val * 100
        # Y 軸 (鑑別度 D): bottom 從 0% (D=-1) 到 100% (D=1)
        bottom_percent <- ((d_val + 1) / 2) * 100
        
        point_html <- paste0("<div class='scatter-point' style='left:", left_percent, "%; bottom:", bottom_percent, "%;' title='題號:", q_num, ", 難度P:", round(p_val,2) , ", 鑑別度D:", round(d_val,2), "'>", q_num, "</div>")
        html_content <- paste0(html_content, point_html)
      }
    }
    html_content <- paste0(html_content, "</div></div>") # End scatter-plot-area and scatter-plot-main-area
    html_content <- paste0(html_content, "<div class='x-axis-label-scatter'>題目難度</div>")
    html_content <- paste0(html_content, "</div>") # End scatter-plot-container
  }

  # --- CSS S-P Chart (班級匿名) ---
  html_content <- paste0(html_content, "<div class='sp-chart-container'>")
  html_content <- paste0(html_content, "<h4 class='sp-chart-title'>您所在班級 S-P 分析圖 (匿名)</h4>")
  html_content <- paste0(html_content, "<p style='font-size:0.85em; text-align:center; color:#555;'>此圖顯示您班級同學在各題的作答情況。題目由左至右按通過人數多到少排列，同學由上至下按總分高到低排列。您的作答行已特別標示。</p>")

  # 1. 篩選出該班級的學生
  class_indices_k <- which(class == student_class_k)
  if (length(class_indices_k) > 0) {
    sOX_class_k <- sOX[class_indices_k, , drop = FALSE]
    sAns_class_k <- sAns[class_indices_k, , drop = FALSE]
    studentName_class_k <- studentName[class_indices_k]
    studentID_class_k <- rownames(sOX_class_k) # Assuming rownames are student IDs
    
    # 2. 排序學生 (高分到低分) - Stable Sort
    class_student_scores_k <- rowSums(sOX_class_k, na.rm = TRUE)
    # Create a data frame for sorting to handle ties
    # For descending score, use -score. For ascending id, use id directly.
    class_student_order_k <- order(-class_student_scores_k, studentID_class_k)
    
    sOX_class_ordered_k <- sOX_class_k[class_student_order_k, , drop = FALSE]
    sAns_class_ordered_k <- sAns_class_k[class_student_order_k, , drop = FALSE]
    studentName_class_ordered_k <- studentName_class_k[class_student_order_k]

    # 3. 排序題目 (通過人數多到少 / 難度易到難) - Stable Sort
    class_question_pass_counts_k <- colSums(sOX_class_ordered_k, na.rm = TRUE) # Use student-ordered sOX for pass counts
    original_q_numbers_html <- 1:queNum
    # For descending pass_count, use -pass_count. For ascending q_num, use q_num directly.
    class_question_order_k <- order(-class_question_pass_counts_k, original_q_numbers_html)
    
    sOX_class_final_k <- sOX_class_ordered_k[, class_question_order_k, drop = FALSE]
    sAns_class_final_k <- sAns_class_ordered_k[, class_question_order_k, drop = FALSE]
    standAns_ordered_k <- standAns[class_question_order_k] # 確保標準答案也跟著題目排序
    original_q_numbers_ordered_k <- original_q_numbers_html[class_question_order_k] # 排序後的原始題號

    # 找到目前學生在排序後班級列表中的位置
    current_student_rank_in_class_k <- which(studentName_class_ordered_k == student_name_k)

    html_content <- paste0(html_content, "<table class='sp-table'><thead><tr><th>生</th>")
    # 表頭：排序後的題號
    for(q_idx in 1:ncol(sOX_class_final_k)){
      html_content <- paste0(html_content, "<th class='sp-q-header'>", original_q_numbers_ordered_k[q_idx] ,"</th>")
    }
    html_content <- paste0(html_content, "<th>總</th></tr></thead><tbody>")

    # 表格內容
    for(s_idx in 1:nrow(sOX_class_final_k)){
      row_class_attr <- ""
      if (length(current_student_rank_in_class_k) > 0 && s_idx == current_student_rank_in_class_k[1]) {
        row_class_attr <- " class='current-student-row'"
      }
      html_content <- paste0(html_content, "<tr", row_class_attr, ">")
      # 學生標示 (匿名，但可以標示當前學生)
      student_label <- paste0( s_idx)
      if (length(current_student_rank_in_class_k) > 0 && s_idx == current_student_rank_in_class_k[1]) {
        student_label <- "<strong>你</strong>"
      }
      html_content <- paste0(html_content, "<td>", student_label, "</td>")

      for(q_idx in 1:ncol(sOX_class_final_k)){
        cell_content <- "&nbsp;" # 預設空白
        cell_class <- ""
        is_current_student_cell <- (length(current_student_rank_in_class_k) > 0 && s_idx == current_student_rank_in_class_k[1])

        if (sOX_class_final_k[s_idx, q_idx] == 1) { # 答對
          cell_class <- "sp-correct" # 預設答對樣式
          if (is_current_student_cell) {
            cell_class <- paste(cell_class, "current-student-cell") # 當前學生答對
            # 對於當前學生答對，cell_content 維持預設的 &nbsp; 或可設為 "✓"
            # cell_content <- "✓" 
          } else if (show_classmates_responses_in_sp_chart) { 
            cell_content <- "&nbsp;" # 其他同學答對，內容留空，依賴背景色，或用 "✓"
            # cell_content <- "✓"
          }
        } else { # 答錯
          cell_class <- "sp-incorrect" # 預設答錯樣式
          if (is_current_student_cell) {
            cell_content <- sAns_class_final_k[s_idx, q_idx]
            cell_class <- "current-student-incorrect" # 更明顯的錯誤標示
          } else if (show_classmates_responses_in_sp_chart) {
            cell_content <- sAns_class_final_k[s_idx, q_idx] # 其他同學答錯，顯示其作答
          }
        }
        html_content <- paste0(html_content, "<td class='", cell_class, "'>", cell_content, "</td>")
      }
      html_content <- paste0(html_content, "<td>", class_student_scores_k[class_student_order_k[s_idx]], "</td>") # 該行學生總分
      html_content <- paste0(html_content, "</tr>")
    }
    html_content <- paste0(html_content, "</tbody></table>")
  } else {
    html_content <- paste0(html_content, "<p>無法獲取您班級的 S-P 分析圖資料。</p>")
  }
  html_content <- paste0(html_content, "</div>") # end sp-chart-container

  # 學習風格分析
  student_cs_k <- student[k, "問題注意係數"]
  student_category_k <- student[k, "判定類別"]
  
  html_content <- paste0(html_content, "<h3>學習風格分析 (參考用)</h3><ul>")
  html_content <- paste0(html_content, "<li><b>您的學生注意係數 (CS)：</b>", student_cs_k, "</li>")
  html_content <- paste0(html_content, "<li><b>您的學習類型判定：</b>", student_category_k, "</li>")
  html_content <- paste0(html_content, "</ul>")

  # 增加詳細建議
  detailed_advice <- ""
  if (student_category_k == "A") {
    detailed_advice <- "表現優異，作答模式與高分群一致。請繼續保持，並可挑戰更深入的學習內容。"
  } else if (student_category_k == "B") {
    detailed_advice <- "表現中等，作答模式符合預期。可針對答錯的題目加強複習，爭取進步。"
  } else if (student_category_k == "C") {
    detailed_advice <- "表現有較大進步空間，作答模式顯示基礎可能不夠穩固。建議您從答錯的題目開始，逐一釐清觀念，特別是那些較為基礎的題目。可尋求老師或同學協助。"
  } else if (student_category_k == "A'") {
    detailed_advice <- "整體表現優異，但作答模式有些特殊。可能是在部分較有把握的題目上出現失誤，或是對某些題目的理解與眾不同。建議回顧答錯的題目，檢查是否有粗心或審題不清的情況。"
  } else if (student_category_k == "B'") {
    detailed_advice <- "表現中等，但作答模式有些特殊。可能在部分題目上有猜測的情況，或對某些概念的理解不夠穩定。建議針對答錯的題目進行深入分析，找出問題癥結。"
  } else if (student_category_k == "C'") {
    detailed_advice <- "表現有較大進步空間，且作答模式特殊。這可能表示您在許多題目上感到困難，或答題策略需要調整。強烈建議您與老師討論，找出學習上的主要困難點，並制定針對性的複習計畫。"
  }
  
  if (detailed_advice != "") {
    html_content <- paste0(html_content, "<p><b>給您的學習建議：</b>", detailed_advice, "</p>")
  }



  html_content <- paste0(html_content, "<p><small><b>註解說明：</b><br>",
                         "<b>學生注意係數 (CS):</b> 此指標衡量您作答模式與群體典型模式的一致性。CS 值較低 (例如 < 0.5) 通常表示您的作答模式與預期相符（即高分群答對難題，低分群答錯易題）。CS 值較高可能表示您的作答模式較為特殊，例如高分者錯了簡單題，或低分者猜對了困難題，值得進一步探討原因。<br>",
                         "<b>學習類型判定 (A, B, C, A', B', C'):</b> 這是基於您的整體答對率和學生注意係數 (CS) 所做的初步分類，僅供參考。一般而言：<br>",
                         "&nbsp;&nbsp;A/B/C 型：表示作答模式與群體表現趨勢較為一致。A代表高分群，B代表中等，C代表待加強。<br>",
                         "&nbsp;&nbsp;A'/B'/C' 型：表示作答模式可能存在特殊之處 (CS值較高)。A'雖屬高分群但作答模式特殊，B'為中等程度但模式特殊，C'為待加強且模式特殊。這些情況建議您回顧答題過程，了解是否有粗心、猜測或對特定概念理解不清等狀況。",
                         "</small></p>")

  # HTML 結尾
  html_content <- paste0(html_content, "</div></body></html>") # 結束 container div 和 body/html
  
  # 儲存 HTML 檔案
  # 確保學生姓名和學號在檔名中是安全的
  safe_student_name_k <- gsub("[^A-Za-z0-9_\\-\\.\u4e00-\u9fa5]", "_", student_name_k) # 保留中文
  safe_student_id_k <- gsub("[^A-Za-z0-9_\\-\\.]", "_", student_id_k)
  
  report_filename <- file.path(class_specific_reports_dir, paste0(subj, "_", safe_student_id_k, "_", safe_student_name_k, "_學習分析報告.html"))
  
  tryCatch({
    # 使用 con <- file(..., encoding = "UTF-8") 來確保正確寫入中文
    con <- file(report_filename, "w", encoding = "UTF-8")
    writeLines(html_content, con, useBytes = FALSE) # useBytes = FALSE 配合 encoding
    close(con)
  }, error = function(e) {
    cat("錯誤：無法寫入學生 ", student_name_k, " 的報告：", e$message, "\n")
  })
  
  # 打印進度 (可選)
  if (k %% 10 == 0 || k == stuNum) { # 每10位學生或最後一位學生時打印進度
    cat(paste0("已產生 ", k, " / ", stuNum, " 份報告...\n"))
  }
}

cat(paste0("\n個別學習報告已全數產生完畢，存放於 '", file.path(getwd(), paste0(subj, "_個別學習報告")), "' 主資料夾下的各班級子資料夾中。\n"))
