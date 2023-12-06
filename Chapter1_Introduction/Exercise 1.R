## 1. �����ݼ��ص� R �в�������ݵ�ά�ȡ�������ݸſ����ж��ٹ۲�ֵ�������˶����ꣿ
getwd() #�鿴����·��
turnout <- read.csv("turnout.csv") #�����ݼ��ؽ���
summary(turnout) #������ݸſ�
range(turnout$year) #�鿴���ǵ����


## 2. ����ͶƱ�����˿ڻ� VAP ����ͶƱ�ʡ�ʹ����ͶƱ�ʸ�������� VEP ����ͶƱ�ʣ��۲첻ͬ
##����ͶƱ�����˿ڼ���
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
  names(out) <- c("��Сֵ", "���ֵ", "��λ��", "ƽ����", "��׼��", "����") # add labels
  return(out) # end function by calling output
}
describe(VER_rate)
describe(VAR_rate)


## 3.���� VAP �� ANES �Ĺ���ͶƱ��֮��Ĳ��졣
describe(turnout$ANES)
difVAAN <- VAR_rate - turnout$ANES
describe(difVAAN)
range(difVAAN)


## 4.�Ƚ���ͳѡ�ٺ�����ѡ�ٵ� VEP ͶƱ�ʺ� ANES ͶƱ�ʡ�
## ��ͳѡ������һ�죬�м����ѡ��Ϊ������ѡ�١�
President <- turnout[c(1,3,5,7,9,11,13,14),] #��ͳѡ�ٵ�����
Congress <- turnout[c(2,4,6,8,10,12),] #��������ѡ�ٵ�����
PVER_rate<- President$total / President$VEP * 100 #��ͳVEPͶƱ��
CVER_rate <- Congress$total / Congress$VEP * 100 #��������VEPͶƱ��
describe(PVER_rate)
PVER_rate
describe(President$ANES)
President$ANES
describe(CVER_rate)
CVER_rate
describe(Congress$ANES)
Congress$ANES
#����������ѡ����ANES����ƫ��Ĳ���
Pdifference <- PVER_rate - President$ANES #��ͳѡ�ٲ���
Cdifference <- PVER_rate - Congress$ANES #���ڹ���ѡ�ٲ���
describe(Pdifference)
describe(Cdifference)


## 5.ͨ��ѡ���꽫���ݷֳ������׶Ρ���ÿ�������ڷֱ���� VEP ͶƱ�ʺ� ANES ͶƱ��֮��Ĳ�ֵ��
t1 <- turnout[c(1 : 7),] 
t2 <- turnout[c(8 : 14),]
t1_VER_rate<- t1$total / t1$VEP * 100
t2_VER_rate<- t2$total / t2$VEP * 100
difference_t1 <- t1$VER - t1$ANES
describe(difference_t1)
difference_t2 <- t2$VER - t2$ANES
describe(difference_t2)
describe(difference_t1 - difference_t2)

## 6.ANES ���ɷ������ͺ���ѡ�񡣼���2008�� VAP ͶƱ�ʵĵ������Ƚϵ������ VAP ͶƱ����δ�������� VAP��VEP �� ANES ͶƱ�ʡ�
VAP_adj <- turnout$VAP - turnout$felons - turnout$noncit
VAR_new <- (turnout$total - turnout$osvoters) / (VAP_adj - turnout$overseas) * 100
VAR_new[14] 
