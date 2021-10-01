#기본 환경설정 2가지 
#- 자동 줄바꿈 옵션 설정



a <- 1
a
b <- 2
b
d <- 3.5
d

a+b


var1 <- c(1,2,5,7,8)
var1

var2 <- (1:5)
var2

str1 <- "a"
str2 <- "text"
str3 <- "Hello world!"


str4 <- c("1", "a","b")
str4


#R에서 데이터분석은 함수로 시작해서 함수로 끝난다.

x <- c(1,2,3)
x

mean(x)

max(x)

min(x)
# 패키지 설치
install.packages("ggplot2")

#임포트
library(ggplot2)

x <- c("a","b","a","c")
x
qplot(x)



qplot(data = mpg, x = hwy)


qplot(data = mpg, x = drv, y = hwy)

qplot(data = mpg, x= drv, y= hwy, geom = "boxplot", colour = drv)


?qplot

#Q1. 80,60,70,50,90 다섯명의 학생의 시험 결과 변수 std 입력하세요.
data <- c(80,60,70,50,90)

#Q2. 학생들의 평균점수를 구하세요
avg = mean(data)
  avg


#평균 점수를 새로운 변수 avg에 담아서 출력하세요

  
  
#데이터프레임 - 가장 많이 사용하는 데이터 형태. 행과 열로 구성된 사각형 모양의 표처럼
#열은 속성 - 세로로 나열되는 열은 속성을 나타낸다. 컬럼, 변수, 피처...
#행의 속성 - 가로로 나열되는 행, 행은 row 케이스 레코드...
  
#뎅터가 크다라는 의미 = 행이 많다 또는 열이 많다.
#데이터 분석의 입장에서는 열이 중요하다. 
  
eng <- c(90,80,60,70)
eng

math <- c(50,60,90,30)
math

df_mid <- data.frame(eng,math)
df_mid

class <- c(1,1,2,2)
class

df_mid = data.frame(eng,math,class)
df_mid

mean(df_mid$eng)

mean(df_mid$math)



# 데이터프레임 한번에 만들기

df_mid = data.frame(eng = c(90,80,60,70),
                    math = c(50,60,90,30),
                    class = c(1,1,2,2))
df_mid



#readxl 패키지 설치와 로드
install.packages("readxl")
library(readxl)

df_exam = read_excel("excel_exam.xlsx")
#이름행이 없을때는
#df_exam = read_excel("excel_exam.xlsx", col_name = F)
#csv를 읽어올 때는
#df_exam = read.csv(excel_exam.csv", header = F)


mean(df_exam$english)

mean(df_exam$science)


# 데이터를 분석하기 위해 읽어오면 데이터를 다양하게 파악하자
#1. 데이터의 앞부분 확인하기

head(df_exam) #기본으로 6번 행까지 확인한다.
head(df_exam, 10) # 지정한 숫자만큼 출력한다.

#2. 데이터의 뒷부분 확인하기

tail(df_exam)
tail(df_exam,10)

#3. 데이터가 몇행 몇열로 구성되어 있는지 확인 dim()
dim(df_exam)
exam = df_exam

#데이터의 열값들의 속성의 구조를 파악 str()

str(exam)

#변수들의 값을 요약한 summary()
summary(exam)   #요약 통계


#뷰어창에서 데이터 확인 View ( V가 대문자)
View(exam)




library(ggplot2)

mpg = as.data.frame(ggplot2::mpg)

head(mpg)
tail(mpg, 10)
dim(mpg)
str(mpg)
summary(mpg)
View(mpg)

#데이터에 관한 설명 보기
?mpg

df_raw = data.frame(var1 = c(1,2,1), var2 = c(2,3,2))
df_raw

install.packages("dplyr")  #데이터 가공에 사용되는 패키지
library(dplyr)

R.version

df_new <-df_raw

df_new <-rename(df_new,v2=var2)

df_new

mpg_new <- mpg

mpg_new <- rename(mpg,city =cty,highway =hwy)

View(mpg_new)

#파생변수 만들기

df <- df_raw

df$sum <- df$var1+df$var2

df

mpg_new$total  <- (mpg_new$city+mpg_new$highway)/2

mean(mpg_new$total)

#조건문을 활용해서 파생변수 만들자

summary(mpg_new$total)

hist(mpg_new$total)

mpg_new$test = ifelse(mpg_new$total >= 20, "pass", "fail")

head(mpg_new,20)

table(mpg_new$test)

install.packages("ggplot2")
library(ggplot2)
qplot(mpg_new$test)

# QUIZ 30 이상이면 A 20 이상이면 B 20 미만은 C 이름은 grade


mpg_new$test = ifelse(mpg_new$total >= 30, "A", ifelse(mpg_new$total >=20, "B", "C")

table(mpg_new)





#종합 QUIZ
#1. ggplot2의 midwest를 데이터프레임으로 가져와서 데이터의 특성을 파악하자 6개
mid <- data.frame(ggplot2::midwest)
head(mid)
tail(mid)
dim(mid)
str(mid)
summary(mid)
View(mid)



#2. poptotal 변수명을 total로 변경 popasian을 asian으로
names(mid)[1] = c("total2")
names(mid)[5] = c("total")
names(mid)[10] = c("popasian")
head(mid)

#mii <- rename(midwest, total = poptotal) 이런 식으로도 가능하다

#3. total과 asian 변수로 전체 인구 대비 아시아 인구 백분율을 파생변수로 만들어서 히스토그램 그리기
mid$popr <- mid$asian/ mid$total * 100
hist(midwest$popr)
#4. 아시아 인구 백분율의 평균을 구하고 그 평균을 초과하면 large아니면 small 파생변수 생성
mean(midwest$popr)






















