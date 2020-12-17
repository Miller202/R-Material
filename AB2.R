### INTERVALO DE CONFIANÇA




## Estimativa pontual

# Exemplo:
# Considere a seguinte amostra de observações no módulo de elasticidade (GPa) de espécimes
# de um processo de fundição. X: 44,2; 43,9; 44,7; 44,2; 44,0; 43,8; 44,6; 43,1.
# Obter uma estimativa para média, variância e desvio padrão da amostra.

x <- c(44.2, 43.9, 44.7, 44.2, 44.0, 43.8, 44.6, 43.1)
mean(x) # media
var(x)  # variancia
sd(x)   # desvio padrão




## Intervalo de confiança da média - VARIÂNCIA CONHECIDA

# Exemplo:
# Uma máquina produz rolamentos que apresentam desvio padrão de '0,042' polegadas em seu
# diâmetro. Desejando-se conhecer o diâmetro médio dos rolamentos produzidos por esta 
# máquina, extraiu-se uma amostra de '100' rolamentos, observando-se uma média igual a 
# '0,824' polegadas: Obter o intervalo com '0,90' de confiança para o verdadeiro 
# diâmetro médio dos rolamentos.

alfa = 0.10 # Obs: 1 - alfa = 90%, logo, alfa = 10%
desvio = 0.042
media = 0.824
n = 100

zc = qnorm(1 - alfa/2, 0, 1) # procura-se o valor na tabela Z
zc = round(zc, 2)
zc

erro = zc * desvio / sqrt(n) # fórmula para encontrar o e0
erro = round(erro, 5)
erro

cat("(", media - erro, ",", media + erro, ")") # intervalo obtido




## Intervalo de confiança da média - VARIÂNCIA DESCONHECIDA

# Exemplo 1:
# Em um processo para obtenção de compostos químicos de tintas, obteve-se os seguintes
# tempos: 90, 92, 92, 95, 98, 99, 100, 100, 100 e 117 segundos.
# Construir um intervalo com nível confiança de 95%.

valores <- c(90, 92, 92, 95, 98, 99, 100, 100, 100, 117)

# utilizando a função t.test

t.test(valores, conf = 0.95)

# sem uso da função

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
# Foram realizados testes glicêmicos em 25 pacientes após um jejum de 8 horas.
# Os resultados são apresentados na tabela abaixo. Encontrar um intervalo de confiança
# de nível de 95 %

testegli <- c(80, 118, 100, 90, 83, 117, 95, 84, 102, 80, 112, 78, 
              102, 121, 82, 77, 88, 73, 104, 88, 132, 91, 103, 140, 101)
t.test(testegli, conf = 0.95)




## Intervalo de confiança - PROPORCAO AMOSTRAL

# Exemplo 1:
# Considere testes de vazamentos identificados em dois municípios A e B. No
# município A foram examinados 500 edifícios e em 100 apresentaram falhas. No B foram
# examinados 1000 edifícios e em 300 apresentaram falhas.
# Construir um intervalo com nível confiança de 95% para o município A.

alfa = 0.05
n = 500
p = 100/500

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 5)

cat("(", p - erro, ",", p + erro, ")")


# Exemplo 2:
# Um partido deseja estimar a proporção de eleitores favoráveis a um determinado 
# candidato. Uma mostra piloto de 2.500 eleitores revelou 60% dos eleitores são 
# favoráveis a este candidato. Elaborar um intervalo de confiança de 95%.

alfa = 0.05
n = 2500
p = (0.6 * 2500) / 2500

zc = qnorm(1 - alfa/2, 0, 1)
zc = round(zc, 2)

erro = zc * sqrt(p * (1 - p) / n)
erro = round(erro, 2)

cat("(", p - erro, ",", p + erro, ")")




### TESTES DE HIPÓTESE




## Testes BILATERAIS

# Exemplo:
# De uma população normal com variância 36, tira-se uma amostra aleatória de tamanho 16,
# obtendo-se uma média de 43. Ao nível de significância de 10%, testar as hipóteses:

# Hipóteses: H0 : u = 45, H1 : u != 45.

n = 16; media = 43; mu = 45; desvio = 6;

# como alfa(RC) = 0.10, RNC = 90%, sendo [5%, 90%, 5%] o intervalo bilateral.

Zalfa = qnorm(0.05) # valores críticos
Zalfa
#output: -1.644854, então, -1,64 < RNC < 1,64.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estatístico
Zcalc
#output: -1.333333

# Como Zcalc pertence ao intervalo RNC, não há indícios para rejeitar h0, ou seja,
# a média é de 45, com 10% de risco.




## Testes unilaterais à ESQUERDA

# Exemplo:
# Uma fábrica anuncia que o índice de nicotina dos cigarros da marca X é inferior a 26 mg
# por cigarro. Um laboratório realiza 10 análises do índice e obtém: 26; 24; 23; 22; 
# 28; 25; 27; 26; 28 e 24. Sabe-se que o índice de nicotina dos cigarros da marca X 
# se distribui normalmente com variância 5,36mg. Pode-se aceitar a afirmação
# do fabricante, ao nível de significância de 5%?

# h0: u = 26, h1: u < 26.

amostra <- c(26, 24, 23, 22, 28, 25, 27, 26, 28, 24)

n = length(amostra); media = mean(amostra); mu = 26; desvio = sqrt(5.36);

# como alfa = 0.05, RNC = 95%, sendo [5%, 95%] o intervalo unilateral à esquerda

Zalfa = qnorm(0.05) # valor crítico
Zalfa
#output: -1.644854, então RNC > -1,64.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estatístico
Zcalc
#output: -0.9561271

# Como Zcalc pertence ao intervalo RNC, Não há indícios para se rejeitar h0 ao nível 
# de 5% e concluímos que a afirmação do fabricante é falsa.




## Testes unilaterais à DIREITA

# Exemplo:
# Um fabricante de lajotas de cerâmica introduz um novo material em sua fabricação para
# aumentar a resistência média, que é de 206 Kg. A resistência das lajotas tem 
# distribuição normal, com desvio padrão de 12 Kg. Retira-se uma amostra de 30 lajotas,
# obtendo-se x = 210 Kg. Ao nível de significância de 10%, pode o fabricante afirmar que a resistência
# média de suas lajotas tenha aumentado?

# h0: u = 206, h1: u > 206.

n = 30; media = 210; mu = 206; desvio = 12;

# como alfa = 0.10, RNC = 90%, sendo [90%, 10%] o intervalo unilateral à DIREITA

Zalfa = qnorm(0.1) # valor crítico
-Zalfa
#output: 1.281552, então RNC < 1.28.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estatístico
Zcalc
#output: 1.825742

# Como Zcalc > Zalfa, Zcalc está fora do intervalo RNC, então rejeita-se h0, ou seja,
# o fabricante pode concluir que a resistência média de suas lajotas aumentou.




## Testes de hipótese para a Média com Variância DESCONHECIDA


## não utilizei a tabela t de student, fiz pelo padrão Z.


# Exemplo 1:
# Os registros dos últimos anos de um colégio atestam para os calouros admitidos uma nota
# média 115 (teste vocacional). Para testar a hipótese de que a média de uma nova turma é
# a mesma das turmas anteriores, retirou-se uma amostra de 20 notas, obtendo-se média 118
# e desvio padrão 20. Admita um nível de significância de 5% para efetuar o teste.

# h0: u = 115, h1: u != 115.

n = 20; media = 118; mu = 115; desvio = 20;

# Alfa = 0.05, RNC = 95%, sendo [2.5%, 95%, 2.5%] o intervalo bilateral

Zalfa = qnorm(0.025) # valor crítico
Zalfa
#output: -1.959964, então -1,96 < RNC < 1,96.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estatístico
Zcalc
#output: 0.6708204

# Como Zcalc está dentro do intervalo RNC, ou seja, fora da região crítica, rejeitamos
# h1 e aceitamos h0, pois ao nível de 5% não há indícios de alteração na média.



# Exemplo 2:
# Deseja-se investigar se uma certa moléstia que ataca o rim altera o consumo de oxigênio
# desse órgão. Para indivíduos sadios, admite-se que esse consumo tem distribuição Normal
# com média 12cm3/min. Os valores medidos em cinco pacientes com a moléstia foram: 14,4;
# 12,9; 15,0; 13,7 e 13,5. Qual seria a conclusão, ao nível de 1% de significância?

# h0: u = 12, h1: u != 12

x <- c(14.4, 12.9, 15, 13.7, 13.5)
n = length(x); media = mean(x); mu = 12; desvio = sd(x)

# alfa = 0.01, RNC = 99%, sendo [0.5%, 99%, 0.5%] o intervalo bilateral

Zalfa = qnorm(0.005) # valor crítico
Zalfa
#output: -2.575829, então -2,57 < RNC < 2,57.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estatístico
Zcalc
#output: 5.209881

# Como Zcalc > 2,57, está fora do intervalo RNC, então, rejeitamos a h0 e aceitamos h1,
# ou seja, indivíduos portadores da moléstia têm média alterada.

t.test(x, conf = 0.99) # pegar intervalo de confiança



# Exemplo 3:
# h0: u = 127, h1: u != 127

x <- c(125, 124, 125, 125, 125, 125, 124, 123, 122, 123, 123,
       123, 123, 124, 124)
n = length(x); media = mean(x); mu = 127; desvio = sd(x)

# alfa = 0.05, RNC = 95%, sendo [0.025%, 99%, 0.025%] o intervalo bilateral

Zalfa = qnorm(0.025) # valor crítico
Zalfa
#output: -1.959964, então -1,96 < RNC < 1,96.

Zcalc = (media - mu) / (desvio / sqrt(n)) # teste estatístico
Zcalc
#output: -12.2526

# Zcalc < Zalfa, está fora do intervalo RNC, então rejeitamos a hipótese nula,
# aceitamos h1, ou seja, a tensão é diferente de 127 V.




## P-VALUE (valor da probabilidade) ou nível descritivo do teste...




## Teste de Hipótese para PROPORÇÃO...




## Teste de Hipótese para DUAS MÉDIAS...




## Teste para comparação de DUAS VARIANCIAS...




## Teste para igualdade de duas Proporções

# Exemplo:
# Em um estudo de 200 mulheres adultas selecionadas aleatoriamente e 250 homens adultos,
# ambos usuários de internet, 30% das mulheres e 38% dos homens disseram que planejam 
# comprar online ao menos uma vez no mês seguinte. Ao nível de significância de 10%, 
# testar a afirmação de que há uma diferença entre a proporção de homens e mulheres,
# usuários de internet, que planejam comprar online.

m = 0.30 * 200
h = 0.38 * 250

prop.test(x = c(m, h), n = c(200, 250), conf.level = 0.90)

# Ao nível de 10%, como o p-value(0.09397) é menor que 0.10, rejeita-se h0, pois há
# evidências que haja diferença entre a proporção de homens e a proporção de mulheres
# usuários de internet.

