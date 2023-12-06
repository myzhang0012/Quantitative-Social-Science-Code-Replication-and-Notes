## 1. 将数据加载到 R 中并检查数据的维度。获得数据概况。有多少观测值？覆盖了多少年？
getwd() #查看工作路径
turnout <- read.csv("turnout.csv") #将数据加载进来
summary(turnout) #获得数据概况
range(turnout$year) #查看覆盖的年份


## 2. 根据投票年龄人口或 VAP 计算投票率。使用有投票资格的人数或 VEP 计算投票率，观察不同
##根据投票年龄人口计算
VER_rate<- turnout$total / turnout$VEP * 100
VAR_rate <- turnout$total / (turnout$VAP + turnout$overseas) * 100
describe <- function(x){ # function takes one input
  min <- min(x)
  max <- max(x)
  med <- median(x)
  mean <- mean(x)
  sd <- sd(x)
  var <- var(x)
  out <- c(min, max, med, mean, sd, var) # define the output
  names(out) <- c("最小值", "最大值", "中位数", "平均数", "标准差", "方差") # add labels
  return(out) # end function by calling output
}
describe(VER_rate)
describe(VAR_rate)


## 3.计算 VAP 和 ANES 的估计投票率之间的差异。
describe(turnout$ANES)
difVAAN <- VAR_rate - turnout$ANES
describe(difVAAN)
range(difVAAN)


## 4.比较总统选举和中期选举的 VEP 投票率和 ANES 投票率。
## 总统选举四年一届，中间国会选举为“中期选举”
President <- turnout[c(1,3,5,7,9,11,13,14),] #总统选举的数据
Congress <- turnout[c(2,4,6,8,10,12),] #国会中期选举的数据
PVER_rate<- President$total / President$VEP * 100 #总统VEP投票率
CVER_rate <- Congress$total / Congress$VEP * 100 #国会中期VEP投票率
describe(PVER_rate)
PVER_rate
describe(President$ANES)
President$ANES
describe(CVER_rate)
CVER_rate
describe(Congress$ANES)
Congress$ANES
#看两种类型选举钟ANES估计偏误的差异
Pdifference <- PVER_rate - President$ANES #总统选举差异
Cdifference <- PVER_rate - Congress$ANES #中期国会选举差异
describe(Pdifference)
describe(Cdifference)


## 5.通过选举年将数据分成两个阶段。在每个周期内分别计算 VEP 投票率和 ANES 投票率之间的差值。
t1 <- turnout[c(1 : 7),] 
t2 <- turnout[c(8 : 14),]
t1_VER_rate<- t1$total / t1$VEP * 100
t2_VER_rate<- t2$total / t2$VEP * 100
difference_t1 <- t1$VER - t1$ANES
describe(difference_t1)
difference_t2 <- t2$VER - t2$ANES
describe(difference_t2)
describe(difference_t1 - difference_t2)

## 6.ANES 不采访囚犯和海外选民。计算2008年 VAP 投票率的调整。比较调整后的 VAP 投票率与未经调整的 VAP、VEP 和 ANES 投票率。
VAP_adj <- turnout$VAP - turnout$felons - turnout$noncit
VAR_new <- (turnout$total - turnout$osvoters) / (VAP_adj - turnout$overseas) * 100
VAR_new[14] 

