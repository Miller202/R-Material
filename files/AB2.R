### INTERVALO DE CONFIAN�A




## Estimativa pontual

# Exemplo:
# Considere a seguinte amostra de observa��es no m�dulo de elasticidade (GPa) de esp�cimes
# de um processo de fundi��o. X: 44,2; 43,9; 44,7; 44,2; 44,0; 43,8; 44,6; 43,1.
# Obter uma estimativa para m�dia, vari�ncia e desvio padr�o da amostra.

x <- c(44.2, 43.9, 44.7, 44.2, 44.0, 43.8, 44.6, 43.1)
mean(x) # media
var(x)  # variancia
sd(x)   # desvio padr�o




## Intervalo de confian�a da m�dia - VARI�NCIA CONHECIDA

# Exemplo:
# Uma m�quina produz rolamentos que apresentam desvio padr�o de '0,042' polegadas em seu
# di�metro. Desejando-se conhecer o di�metro m�dio dos rolamentos produzidos por esta 
# m�quina, extraiu-se uma amostra de '100' rolamentos, observando-se uma m�dia igual a 
# '0,824' polegadas: Obter o intervalo com '0,90' de confian�a para o verdadeiro 
# di�metro m�dio dos rolamentos.

alfa = 0.10 # Obs: 1 - alfa = 90%, logo, alfa = 10%
desvio = 0.042
media = 0.824
n = 100

zc = qnorm(1 - alfa/2, 0, 1) # procura-se o valor na tabela Z
zc = round(zc, 2)
zc

erro = zc * desvio / sqrt(n) # f�rmula para encontrar o e0
erro = round(erro, 5)
erro

cat("(", media - erro, ",", media + erro, ")") # intervalo obtido




## Intervalo de confian�a da m�dia - VARI�NCIA DESCONHECIDA

# Exemplo 1:
# Em um processo para obten��o de compostos qu�micos de tintas, obteve-se os seguintes
# tempos: 90, 92, 92, 95, 98, 99, 100, 100, 100 e 117 segundos.
# Construir um intervalo com n�vel confian�a de 95%.

valores <- c(90, 92, 92, 95, 98, 99, 100, 100, 100, 117)

# utilizando a fun��o t.test

t.test(valores, conf = 0.95)

# sem uso da fun��o

alfa = 0.05
n = length(valores)
desvio = sd(valores)
media = mean(valores)

tc = qt(p = 1 - alfa/2, df = n - 1)
tc = round(tc, 3)

erro = tc * desvio / sqrt(n)
erro = round(erro, 3)

cat("(", media - erro, ",", media + erro, ")")


# Exemplo 2:
# Foram realizados testes glic�micos em 25 pacientes ap�s um jejum de 8 horas.
# Os resultados s�o apresentados na tabela abaixo. Encontrar um intervalo de confian�a
# de n�vel de 95 %

testegli <- c(80, 118, 100, 90, 83, 117, 95, 84, 102, 80, 112, 78, 
              102, 121, 82, 77, 88, 73, 104, 88, 132, 91, 103, 140, 101)
t.test(testegli, conf = 0.95)




## Intervalo de confian�a - PROPORCAO AMOSTRAL

# Exemplo 1:
# Considere testes de vazamentos identificados em dois munic�pios A e B. No
# munic�pio A foram examinados 500 edif�cios e em 100 apresentaram falhas. No B foram
# examinados 1000 edif�cios e em 300 apresentaram falhas.
# Construir um intervalo com n�vel confian�a de 95% para o munic�pio A.

alfa = 0.05
n = 500
p = 100/500

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 5)

cat("(", p - erro, ",", p + erro, ")")


# Exemplo 2:
# Um partido deseja estimar a propor��o de eleitores favor�veis a um determinado 
# candidato. Uma mostra piloto de 2.500 eleitores revelou 60% dos eleitores s�o 
# favor�veis a este candidato. Elaborar um intervalo de confian�a de 95%.

alfa = 0.05
n = 2500
p = (0.6 * 2500) / 2500

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 2)

cat("(", p - erro, ",", p + erro, ")")




### TESTES DE HIP�TESE




## Testes BILATERAIS

# Exemplo:
# De uma popula��o normal com vari�ncia 36, tira-se uma amostra aleat�ria de tamanho 16,
# obtendo-se uma m�dia de 43. Ao n�vel de signific�ncia de 10%, testar as hip�teses:

# Hip�teses: H0 : u = 45, H1 : u != 45.

n = 16; media = 43; mu = 45; desvio = 6;

# como alfa(RC) = 0.10, RNC = 90%, sendo [5%, 90%, 5%] o intervalo bilateral.

Zalfa = qnorm(0.05) # valores cr�ticos
Zalfa
#output: -1.644854, ent�o, -1,64 < RNC < 1,64.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estat�stico
Zcalc
#output: -1.333333

# Como Zcalc pertence ao intervalo RNC, n�o h� ind�cios para rejeitar h0, ou seja,
# a m�dia � de 45, com 10% de risco.




## Testes unilaterais � ESQUERDA

# Exemplo:
# Uma f�brica anuncia que o �ndice de nicotina dos cigarros da marca X � inferior a 26 mg
# por cigarro. Um laborat�rio realiza 10 an�lises do �ndice e obt�m: 26; 24; 23; 22; 
# 28; 25; 27; 26; 28 e 24. Sabe-se que o �ndice de nicotina dos cigarros da marca X 
# se distribui normalmente com vari�ncia 5,36mg. Pode-se aceitar a afirma��o
# do fabricante, ao n�vel de signific�ncia de 5%?

# h0: u = 26, h1: u < 26.

amostra <- c(26, 24, 23, 22, 28, 25, 27, 26, 28, 24)

n = length(amostra); media = mean(amostra); mu = 26; desvio = sqrt(5.36);

# como alfa = 0.05, RNC = 95%, sendo [5%, 95%] o intervalo unilateral � esquerda

Zalfa = qnorm(0.05) # valor cr�tico
Zalfa
#output: -1.644854, ent�o RNC > -1,64.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estat�stico
Zcalc
#output: -0.9561271

# Como Zcalc pertence ao intervalo RNC, N�o h� ind�cios para se rejeitar h0 ao n�vel 
# de 5% e conclu�mos que a afirma��o do fabricante � falsa.




## Testes unilaterais � DIREITA

# Exemplo:
# Um fabricante de lajotas de cer�mica introduz um novo material em sua fabrica��o para
# aumentar a resist�ncia m�dia, que � de 206 Kg. A resist�ncia das lajotas tem 
# distribui��o normal, com desvio padr�o de 12 Kg. Retira-se uma amostra de 30 lajotas,
# obtendo-se x = 210 Kg. Ao n�vel de signific�ncia de 10%, pode o fabricante afirmar que a resist�ncia
# m�dia de suas lajotas tenha aumentado?

# h0: u = 206, h1: u > 206.

n = 30; media = 210; mu = 206; desvio = 12;

# como alfa = 0.10, RNC = 90%, sendo [90%, 10%] o intervalo unilateral � DIREITA

Zalfa = qnorm(0.1) # valor cr�tico
-Zalfa
#output: 1.281552, ent�o RNC < 1.28.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estat�stico
Zcalc
#output: 1.825742

# Como Zcalc > Zalfa, Zcalc est� fora do intervalo RNC, ent�o rejeita-se h0, ou seja,
# o fabricante pode concluir que a resist�ncia m�dia de suas lajotas aumentou.




## Testes de hip�tese para a M�dia com Vari�ncia DESCONHECIDA

# Exemplo 1:
# Os registros dos �ltimos anos de um col�gio atestam para os calouros admitidos uma nota
# m�dia 115 (teste vocacional). Para testar a hip�tese de que a m�dia de uma nova turma �
# a mesma das turmas anteriores, retirou-se uma amostra de 20 notas, obtendo-se m�dia 118
# e desvio padr�o 20. Admita um n�vel de signific�ncia de 5% para efetuar o teste.

# h0: u = 115, h1: u != 115.

n = 20; media = 118; mu = 115; desvio = 20; alfa = 0.05;

# Alfa = 0.05, RNC = 95%, sendo [2.5%, 95%, 2.5%] o intervalo bilateral

T_alfa = qt(1 - alfa/2, df = n - 1) # valor cr�tico
T_alfa
#output: 2.093024, ent�o -2.093024 < RNC < 2.093024.

T_calc = (media - mu) / (desvio / sqrt(n)) # teste estat�stico
T_calc
#output: 0.6708204

# Como T_calc est� dentro do intervalo RNC, ou seja, fora da regi�o cr�tica, rejeitamos
# h1 e aceitamos h0, pois ao n�vel de 5% n�o h� ind�cios de altera��o na m�dia.



# Exemplo 2:
# Deseja-se investigar se uma certa mol�stia que ataca o rim altera o consumo de oxig�nio
# desse �rg�o. Para indiv�duos sadios, admite-se que esse consumo tem distribui��o Normal
# com m�dia 12cm3/min. Os valores medidos em cinco pacientes com a mol�stia foram: 14,4;
# 12,9; 15,0; 13,7 e 13,5. Qual seria a conclus�o, ao n�vel de 1% de signific�ncia?

# h0: u = 12, h1: u != 12

x <- c(14.4, 12.9, 15, 13.7, 13.5)
n = length(x); media = mean(x); mu = 12; desvio = sd(x); alfa = 0.01;

# alfa = 0.01, RNC = 99%, sendo [0.5%, 99%, 0.5%] o intervalo bilateral

T_alfa = qt(1 - alfa/2, df = n - 1) # valor cr�tico
T_alfa
#output: 4.604095, ent�o -4.604095 < RNC < 4.604095.

T_calc = (media - mu) / (desvio / sqrt(n)) # teste estat�stico
T_calc
#output: 5.209881

# Como T_calc > T_alfa, est� fora do intervalo RNC, ent�o, rejeitamos a h0 e aceitamos h1,
# ou seja, indiv�duos portadores da mol�stia t�m m�dia alterada.

t.test(x, conf = 0.99) # pegar intervalo de confian�a



# Exemplo 3:
# h0: u = 127, h1: u != 127

x <- c(125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123,
       123, 123, 124, 124)
n = length(x); media = mean(x); mu = 127; desvio = sd(x); alfa = 0.05;

# alfa = 0.05, RNC = 95%, sendo [2.5%, 95%, 2.5%] o intervalo bilateral

T_alfa = qt(1 - alfa/2, df = n - 1) # valor cr�tico
T_alfa
#output: 2.144787, ent�o -2.144787 < RNC < 2.144787.

T_calc = (media - mu) / (desvio / sqrt(n)) # teste estat�stico
T_calc
#output: -12.2526

# T_calc est� fora do intervalo RNC de -2.144787 a 2.144787, ent�o rejeitamos a hip�tese
# nula h0 e aceitamos h1, ou seja, a tens�o � diferente de 127 V.




## P-VALUE (valor da probabilidade) ou n�vel descritivo do teste...

## rejeitar h0 se p-value <= alfa, aceitar h0 se p-value > alfa

# Exemplo 1:
# Realizar teste de hip�tese, utilizado p-value, para uma popula��o com m�dia $ 2.500
# e m�dia da amostra $ 2.590, desvio padr�o $ 285 e o tamanho da amostra 50. Considere
# n�vel de signific�ncia de 5%.

# h0: u = 2500, h1: u != 2500

n = 50; media = 2590; mu = 2500; desvio = 285; alfa = 0.05;

# teste bilateral, ou seja, o p-value ser� o dobro de (1 - pnorm(Z)).
Zcalc = (media - mu) / (desvio / sqrt(n)) #output: 2.232969 (valor Z)
Pvalue = 2 * (1 - pnorm(Zcalc)) #output: 0.025551 (p-value)

# como p-value < alfa, rejeitamos h0.


# Exemplo 2:
# Com base em um teste z unilateral a 5% de signific�ncia, p�de-se concluir que a
# m�dia u � maior que 20 uma vez que a estat�stica z obtida foi de 2,5.

# h0: u = 20, h1: u > 20.

Zcalc = 2.5
Pvalue = (1 - pnorm(Zcalc)) #output: 0.006209665 (p-value), obs: unilateral.

# como p-value < alfa, rejeitamos h0, ou seja, u pode ser maior que 20.




## Teste de Hip�tese para PROPOR��O...

# Exemplo:
# Um candidato a deputado estadual afirma que ter� 60% dos votos dos eleitores de uma
# cidade. Um instituto de pesquisa colhe uma amostra de 300 eleitores dessa cidade, 
# encontrando 160 que votar�o no candidato. Esse resultado mostra que a afirma��o do
# candidato � verdadeira, ao n�vel de 5%?

# h0: u = 0.60, h1: u != 0.60

n = 300; x = 160; p = 0.60; p1 = x / n; alfa = 0.05;

Zalfa = qnorm(0.025) #bilateral, portanto, [2.5%, 95%, 2.5%];
-Zalfa #output: 1.959964, ent�o, intervalo -1.96 < RNC < 1.96.

Zcalc = (p1 - p) / sqrt(p * (1 - p) / n)
Zcalc #output: -2.357023

# como Zcalc est� fora do intervalo RNC de -1.96 a 1.96, rejeitamos h0, ou seja,
# a afirma��o do candidato � falsa a 5% de risco, aceitamos h1.




## Teste de Hip�tese para DUAS M�DIAS...


# DADOS PAREADOS

# Exemplo:
# Seja o problema de verificar se um novo algoritmo de busca em um banco de dados � 
# mais r�pido que o algoritmo atualmente usado. Para fazer a compara��o dos dois 
# algoritmos, planeja-se realizar uma amostra aleat�ria de dez buscas experimentais.
# Em cada realiza��o, uma dada busca � realizada pelos dois algoritmos e o tempo de 
# resposta � registrado para ambos os processos. Considerando dez realiza��es, existe
# diferen�a entre as velocidades de busca para os dois algoritmos? Verificar, a um 
# n�vel de 5% de signific�ncia.

# h0: u1 - u2 = ud = 0, h1: ud != 0.

A <- c(22, 21, 28, 30, 33, 33, 26, 24, 31, 22)
B <- c(25, 28, 26, 36, 32, 39, 28, 33, 30, 27)
D <- c(A - B)
n = length(A); alfa = 0.05;

T_alfa = qt(1 - alfa/2, df = n - 1) # valor cr�tico
T_alfa #output: 2.262157, intervalo de -2.26 a 2.26. 

t.test(A, B, mu = 0, paired = T, conf.level = 0.95)

T_calc = -2.8246

# como T_calc est� fora do intervalo do T_alfa, rejeitamos h0, Conclu�mos ent�o que 
# h� diferen�a na velocidade de busca dos dados dos dois algoritmos.




# AMOSTRAS INDEPENDENTES

# Exemplo (com vari�ncias populacionais conhecidas):
# Uma organiza��o de educa��o de consumidores afirma que h� uma diferen�a entre a 
# m�dia da divida do cart�o de credito de homens e mulheres. Sabe-se, de estudos 
# anteriores, que o desvio padr�o para divida da mulheres � de U$ 750 e, dos homens, 
# U$ 800. Os resultados de uma pesquisa aleat�ria de 200 indiv�duos de cada grupo 
# foram: m�dia divida das mulheres U$ 2.290, m�dia da divida dos homens U$ 2.370. 
# Verifique se, ao n�vel de 5%, se a afirma��o da organiza��o est� correta.

# h0: U.h = U.m, h1: U.h != U.m

n1 = 200; n2 = 200; media1 = 2290; media2 = 2370; ds1 = 750; ds2 = 800;
alfa = 0.05

Zalfa = qnorm(0.025) # teste bilateral, ent�o [2.5%, 95%, 2.5%];
Zalfa #output: -1.959964, intervalo -1.96 < RNC < 1.96.

Zobs = ((media1 - media2) - 0) / sqrt((ds1 * ds1 / n1) + (ds2 * ds2 / n2))
Zobs #output: -1.031721

# como Zobs est� dentro do intervalo RNC de -1.96 a 1.96, aceitamos h0 e rejeita-se h1,
# ou seja, n�o h� evid�ncias de que haja diferen�a entre a divida m�dia dos homens e a
# divida m�dia das mulheres.




## Teste para compara��o de DUAS VARIANCIAS...

# Exemplo 1:
# Em um estudo, foram coletados dados das tens�es m�ximas suportadas (MPa) em dois 
# tipos de madeiras da regi�o do Sudoeste do Paran�: Ita�ba e Cedrinho. Os valores 
# est�o apresentados com objetivo de verificar se as vari�ncias dos dois grupos s�o 
# homog�neas ou n�o, ao n�vel 5% de signific�ncia.

# h0: ds1 = ds2, h1: ds1 != ds2.

x <- c(54.07, 45.92, 44.10, 39.36, 38.46, 40.20, 40.93, 45.24)
y <- c(40.42, 32.64, 45.67, 41.62, 45.08, 34.73, 32.58, 38.96)

ds1 = sd(x); ds2 = sd(y); media1 = mean(x); media2 = mean(y);
var1 = ds1 * ds1 #output: 25.85646
var2 = ds2 * ds2 #output: 27.17505

# utilizando a tabela F, com signific�ncia alfa/2 = 0.025, temos F(2.5%, 7, 7) = 4,99.

Fcalc = var2 / var1 #output: 1.051

# como Fcalc est� dentro do intervalo RNC de -4,99 a 4,99, aceitamos h0 e rejeitamos h1.


# Exemplo 2: repeti��o do exemplo 1 por�m utilizando a fun��o var.test.

var.test(y, x)
#o output retorna um valor F correspondente ao Fcalc = 1.051.
# Temos a mesma conclus�o, Fcalc < Falfa.




## Teste para igualdade de DUAS PROPOR��ES

# Exemplo:
# Em um estudo de 200 mulheres adultas selecionadas aleatoriamente e 250 homens adultos,
# ambos usu�rios de internet, 30% das mulheres e 38% dos homens disseram que planejam 
# comprar online ao menos uma vez no m�s seguinte. Ao n�vel de signific�ncia de 10%, 
# testar a afirma��o de que h� uma diferen�a entre a propor��o de homens e mulheres,
# usu�rios de internet, que planejam comprar online.

m = 0.30 * 200
h = 0.38 * 250

prop.test(x = c(m, h), n = c(200, 250), conf.level = 0.90)

# Ao n�vel de 10%, como o p-value(0.09397) � menor que 0.10, rejeita-se h0, pois h�
# evid�ncias que haja diferen�a entre a propor��o de homens e a propor��o de mulheres
# usu�rios de internet.




## AN�LISE DE VARI�NCIA

# Exemplo 1:
# Foram selecionados v�rios autom�veis de 3 modelos diferentes e neles colocados a mesma
# quantidade de gasolina. Existe diferen�a entre a dist�ncia m�dia percorrida pelos 
# diferentes tipos de autom�veis? Fa�a o teste com n�vel de signific�ncia de 0,05?

Kmcarros <- data.frame(KM = c(254, 263, 241, 237, 251, 234, 218, 235, 227, 216, 
                              200, 222, 197, 206, 204), 
                       Modelos = c("Modelo1", "Modelo1", "Modelo1", "Modelo1",
                                   "Modelo1", "Modelo2", "Modelo2", "Modelo2",
                                   "Modelo2", "Modelo2", "Modelo3", "Modelo3",
                                   "Modelo3", "Modelo3", "Modelo3"))

result = aov(Kmcarros$KM ~ factor(Kmcarros$Modelos))
anova(result) # a partir do output, temos que F = 25.276.
# como temos df 2 e 12, buscando na tabela F para 5% encontraremos Fcrit = 3.89

# F > Fcrit, ent�o rejeitamos h0 para alfa = 0.05, ou seja, h� evid�ncia de que
# pelo menos uma das m�dias u � diferente das outras.

TukeyHSD(result) # compara��es m�ltiplas 

# Como todo p-adj < 0.05, temos evid�ncia de diferen�as de consumo entre 
# os tr�s modelos de autom�veis.


# Exemplo 2:
# Um experimento comparou a resist�ncia � compress�o (lb) de quatro tipos diferentes 
# de caixas. O objetivo � avaliar se a resist�ncia m�dia difere para as quatro caixas 
# avaliadas, isto �, deseja-se verificar se a vari�vel resposta apresenta diferen�a na 
# presen�a dos diferentes n�veis.? Fa�a o teste com n�vel de signific�ncia de 0.05.

dat <- data.frame(Caixa = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2,
                            3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4),
          Resistencia = c(655.5, 788.3, 734.3, 721.4, 679.1, 699.4, 789.2,
                          772.5, 786.9, 686.1, 732.1, 774.8, 737.1, 639.0,
                          696.3, 671.7, 717.2, 727.1, 535.1, 628.7, 542.4,
                          559.0, 586.9, 520.0))

result = aov(dat$Resistencia ~ factor(dat$Caixa))
anova(result) # a partir do output, temos que F = 25.094.
# como temos df 3 e 20, buscando na tabela F para 5% encontraremos Fcrit = 3.10

# temos que F > Fcrit, ent�o rejeitamos h0 ao n�vel de 5% de signific�ncia, 
# ou seja, H� ind�cios que a resist�ncia m�dia real depende do tipo de caixa. 

TukeyHSD(result) # compara��es m�ltiplas
# observando os resultados onde p-adj < 0.05, temos que a caixa 4 � a que mais difere.




## CORRELA��O LINEAR

# Crit�rios para verificar o n�vel da correla��o:
# valor de r / Correla��o; 0,0 / nula; 0,0 ---| 0,3 / fraca; 0,3 ---| 0,6 / media;
# 0,6 ---| 0,9 / forte; 0,9 ---| 0,99 / fort�ssima; 1 / perfeita.


# Exemplo 1:
# uma pesquisa pretende verificar se h� correla��o significativa entre o peso total
# do lixo descartado, por dia, nos hot�is, com o peso do papel contido nesse lixo.

x <- c(10.47, 19.85, 21.25, 24.36, 27.38,
       28.09, 33.61, 35.73, 38.33, 49.14)
y <- c(2.43, 5.12, 6.88, 6.22, 8.84,
       8.76, 7.54, 8.47, 9.55, 11.43)
n = length(x); alfa = 0.05;

# encontrar o n�vel de correla��o:
r = cor(x, y) #output: 0.9206232 -- fort�ssima

# teste de hip�tese para exist�ncia da correla��o:
# utilizaremos a tabela t de student, para o risco de 5%, temos o seguinte:
# h0: p = 0, h1: p!= 0; buscar na tabela t, para amostra de n - 2 e 5% bilateral.

t_tabela = qt(1 - alfa/2, df = n - 2) #output: 2.306004

t_calc = (r * sqrt(n - 2)) / (sqrt(1 - (r * r))) #output: 6.668976

# como t_calc > t_tabela, rejeita-se h0, concluindo, com risco de 5%, que h� correla��o.


# Exemplo 2:
# Calcular r e t para os dados abaixo:

x <- c(5, 8, 7, 10, 6, 7, 9, 3, 8, 2)
y <- c(6, 9, 8, 10, 5, 7, 8, 4, 6, 2)
n <- length(x); alfa = 0.01;

# encontrar o n�vel de correla��o:
r = cor(x, y) #output: 0.9112421 -- fort�ssima

# utilizaremos a tabela t de student, para o risco de 1%, temos o seguinte:
# h0: p = 0, h1: p!= 0; buscar na tabela t, para amostra de n - 2 e 1% bilateral.

t_tabela = qt(1 - alfa/2, df = n - 2) #output: 3.355387

t_calc = (r * sqrt(n - 2)) / (sqrt(1 - (r * r))) #output: 6.25774

# como t_calc > t_tabela, rejeita-se h0, concluindo, com risco de 1%, que h� correla��o.

# OBS: A fun��o cor.test() retorna um valor p que tem por hip�tese nula que o
# coeficiente de correla��o � igual a zero na popula��o correspondente.
cor.test(x, y, method = "pearson")




## REGRESS�O LINEAR

# A equa��o da reta da regress�o � dada por Y = ax + b; 
# O coeficiente de determina��o "R2" � dado por r * r.

# Exemplo 1:
# Determinar equa��o da reta de regress�o do primeiro exemplo de correla��o.

regressao = lm(y ~ x)
regressao #output: x = 0.2131, intersept = 1.3836, ent�o: Y = 0.2131x + 1.3836.


# Exemplo 2:
# Determinar o R2 e a equa��o da reta do segundo exemplo de correla��o.

r = 0.9112421; r * r; #output: 0.8303622.
regressao = lm(y ~ x)
regressao #output: x = 0.8632, intersept = 0.8889, ent�o Y = 0.8232x + 0.8889.


# Exemplo 3:
# Consumo de cerveja e temperatura. As vari�veis foram observadas em nove localidades com
# as mesmas caracter�sticas demogr�ficas e socioecon�micas.

x <- c(16, 31, 38, 39, 37, 36, 36, 22, 10)
y <- c(290, 374, 393, 425, 406, 370, 365, 320, 269)

r = cor(x, y); #output: 0.9615505
R2 = r* r; #output: 0.9245793

regressao = lm(y ~ x)
regressao #output: x = 4.739, intersept = 217.366, ent�o Y = 4.739x + 217.366.

