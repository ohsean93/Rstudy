#일표본 검증
alternative=c("two.sided","less","greater")
mu=0

x=rnorm(30)

mean(x)
t.test(x)
t.test(x,mu=2)
t.test(x,alternative = "less",mu=2)

#이표본검증
#독립
head(sleep)
tail(sleep)
dim(sleep)
sleep

sleep2=sleep[c(1,2)]
sleep2

var.test(extra~group,sleep2)
t.test(extra~group,data=sleep2,paired=F,var.equal=T)

#연관, 짝표본
t.test(extra~group,data=sleep,paired=T)
t.test(sleep$extra[sleep$group=='1'],sleep$extra[sleep$group=='2'],paired=T)

#일표본 비율
prop.test(42,100)
prop.test(42,100,0.5)

binom.test(42,100)
binom.test(42,100,0.5)

#이표본 비율
prop.test(c(45,55),c(100,90))
