
##Econometrics Presentation1

#タイタニックの沈没事故では、女性、子供を優先して救出ボートに載せたそうです
#このことは本当なのか
#今回はタイタニック号の生存者・犠牲者データを使って
#[子供は優先的に救出されたのかを調べます]

setwd("~/Econometrix2018")
train <-read.csv("train.csv")
View(train)

#元のデータには年齢は載っていまが、
#このデータをそのまま回帰分析に使ってしまうと結果が不明瞭になるので
#15歳以上、未満で「子供フラグ」を作成しました。

titanic <- read.csv("titanic_df.csv")
View(titanic)

#子供と生存率の因果関係を調べる上で交絡因子に注意します。
#交絡因子と考えうる要素は部屋のグレード(Pclass)です。
#すなわち、子供連れの家族は低いグレードの部屋に泊まる傾向があり、
#低いグレードの利用者は助かりにくいという関係で、
#子供が助かりにくい相関が単回帰の結果としてでてしまう可能性があります。
#そこで交絡因子をモデルに組み込むことで真の因果関係を知ることができます。

titanic_lm1<-lm(Survived~child+Pclass,data=titanic)
summary(titanic_lm1)

#ここで、部屋のグレードと子供の説明変数の共線性を確認します

summary(lm(child~Pclass,data=titanic))
#Multiple R-squared:  0.01403
#VIF
1/(1-0.01403)
#1.01423
summary(lm(Pclass~child,data=titanic))
1/(1-0.01403)

#########
#できない）titanic$Sex<-ifelse(titanic$Sex=="female", 1, 0)
train$Embarked<-as.factor(train$Embarked)
titanic$Pclass<-as.factor(titanic$Pclass)

summary(titanic)

#step-wise
titanic2<-na.omit(titanic)

titanic_glm1<-glm(Survived~.-X-Name-Ticket-Cabin-Age,data=titanic2)
summary(titanic_glm1)
titanic_glm2<-step(titanic_glm1)
summary(titanic_lm2)

#childデータ抽出
child_data<-titanic[titanic$child =="1", ]
summary(child_data)
titanic_glm3<-glm(Survived~.-X-Name-Ticket-Cabin-Age,data=child_data,family="binomial")
titanic_lm4<-step(titanic_glm3)
summary(titanic_lm4)

#Pclassデータ抽出
Pclass1_data<-titanic[titanic$Pclass =="1", ]
summary(Pclass_data)
summary(glm(Survived~child,data=Pclass1_data))

Pclass2_data<-titanic[titanic$Pclass =="2", ]
summary(glm(Survived~child,data=Pclass2_data))

Pclass3_data<-titanic[titanic$Pclass =="3", ]
summary(glm(Survived~child,data=Pclass3_data))

