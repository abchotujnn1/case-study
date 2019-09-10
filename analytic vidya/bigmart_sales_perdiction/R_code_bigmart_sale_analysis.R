library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(corrplot)
library(xgboost)
library(cowplot)
#loading the data
train<-read.csv("Train_UWu5bXk.csv")
test<-read.csv("Test_u94Q5KV.csv")
str(train)
str(test)
#understanding the data
dim(train)
dim(test)
names(train)
names(test)

train[,Item_Outlet_Sales:=NA]
combi<-rbind(train,test)

#univariate analysis
#1.target variable(numeric variable)
ggplot(combi)+geom_histogram(aes(combi$Item_Outlet_Sales),binwidth=100,fill="orange")+xlab("Item_Outlet_sales")
#2.independent variable(numeric variable)
p1<-ggplot(combi)+geom_histogram(aes(combi$Item_Weight),binwidth = 0.5,fill="blue")
p2<-ggplot(combi)+geom_histogram(aes(combi$Item_Visibility),binwidth = 0.005,fill="blue")
p3<-ggplot(combi)+geom_histogram(aes(combi$Item_MRP),binwidth = 1,fill="blue")
plot_grid(p1,p2,p3,ncol = 1)
#3.independent variable(categorical variable)
      #1)Item_Fat_Content plot
ggplot(combi%>%group_by(combi$Item_Fat_Content)%>%summarise(count=n()))+
       geom_bar(aes(combi$Item_Fat_Content,count),stat="identity",fill="blue")


combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"]="Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="low fat"]="Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"]="Regular"
ggplot(combi%>%group_by(Item_Fat_Content)%>%summarise(count=n()))+
      geom_bar(aes(Item_Fat_Content,count),stat="identity","coral1")
       #2)Item_Type plot
p4<-ggplot(combi%>%group_by(Item_Type)%>%summarise(count=n()))+
           geom_bar(aes(Item_Type,count),stat="identity",fill="coral1")+xlab("")+
           geom_label(aes(Item_Type,count,label=count),vjust=0.5)+
           theme(axis.text.x = element_text(angle = 45,hjust = 1))+
           ggtitle("Item_Type")
        #3)Outlet_Identifier
p5<-ggplot(combi%>%)
           