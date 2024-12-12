
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)

#cas 1: IC per la mitja mu al 95%
#primer dic que X barra (estimacio de la mitja) = mu
# Tenim que la P(-m <Xbarra=mu>m) = 0.95
#condico 1) x segeuix una normal amb mu i sigma
# 2) coneixem sigma ^2
# m = qnorm(0.975)siga/sqrt(n) aixo es el marge d'error
#Cas 2: IC (1-alpha) %
# (l,u) = ( Xbarra-Zalpha/2 , Xbarra + Zalpha/2 sigma/sqrt(n))




x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
x
xbarra <- mean(x)
xbarra
sigma <- sqrt(25)
sigma
n <- length(x)
n
z005 <- qnorm(0.95) #alpha 0.1, confiança del 90%
z005
c(xbarra-z005*sigma/sqrt(n),xbarra+z005*sigma/sqrt(n))

#instalar libreria
install.packages("BSDA")
library(BSDA)
z.test(x,sigma.x=sigma,conf.level =0.9)#aixo serveix per calcular el interval i per test d'hypotesi
#mirem si l'interval es el mateix, si
#criteri 1:
# si la mu zero esta dins del ic llavors acceptem H0 que era dir que la mu zero podia fer la mu real
# si la mu zero no esta ic llavors no acceptem H0
#criteri 2
#si H0 es veritat mirem l'erro
# xbar-mu /sigma/sqrt(n) = Z
# aixo em diu que tan lluny estic de la mu nula

# H0: mu =500
# H1 : mu != 500
zc <- qnorm(0.95)
zc
muo <- 500
muo
zobs <- (xbarra-muo)/(sigma/sqrt(n))
zobs
z.test(x,sigma.x=sigma, conf.level =0.9, mu=muo)
# aixo es per dues cues
#el nostre z obs cau fora de zcritic per tant hem de rebutjar la hipotesi


#criteri 2 : si zobs esta entre -zcrit i +zcrit acceptem Ho
# si zobs no esta entre la regio d'acceptacio llavors rebutjo Ho

#criteri 3 p-valure
# si el p valor es major que alpha  accepto
#menretes mes petit sifui el p valor kmes raons per rebutjar la hipotesi

pvalue <- 2*pnorm(-zobs)
pvalue

#cola superior
#HO : mu <= muo (mes igual que hi hagi mneys del que toca, soc rata)
#H1 : hipotesi alternativa, em preocupa que mu > muo (cola superior)

# el tipus de cua hi dit la hipo alternativ



# per cola superior
z.test(x,sigma.x=sigma, conf.level = 0.9, mu=muo, alternative="greater")


n1 <- (qnorm(0.975)*sigma)^2
n1
# no es partit per 2 pq l longitud es dos vegades el z*sigma per tent s'anula el denominador

# apartat c sigma desconeguda conf del 99%
# no tenim ni mu pero podem fer servir la mitja muestral xbar
# per fer la sigma utilitzem la estimacio muestral dela sigma S
#T = Xbar-mu/ (S/sqrt(n))

xbar <- mean (x)
xbar
t0005 <- qt(0.995,n-1)
s <- sd(x)
c(xbar-t0005*s/sqrt(n),xbar+t0005*s/sqrt(n))
# ara la varianá que hem estimat es 6 i per tant hem augmentat l'interval de confiança i fa que le mu que deiem que era igual a 500 ara si que entra a dins de l'interval per tant acceptem H0

t.test(x,conf.leve=0.99)

# hipotesi 
#H0 = mu = muo= 500
# H1 : mu != muo = 500 (hipotesi dues cues vull clavar el numero)

t.test(x, alternative= "two.sided", conf.level=0.99, mu =500)
alpha <- 1-0.99
alpha
