# RESUMO DAS FÓRMULAS DE PROBABILIDADE

# Combinacao
choose(n,k)

# Arranjo
arranjo <- factorial(n) / factorial(n-k)

# Exercicio exemplo
# 35 professores, 21 homens e 14 mulheres. Comissao de 3 prof sera formada em sorteio.
# Qual a prob da comissao ser formado por pelo menos 2 mulheres?

Pm2m <- choose(21,1) * choose(14,2) / choose(35,3)
Pm2m <- Pm2m + (choose(21,0) * choose(14,3) / choose(35,3))
Pm2m


# Esperanca matematica
# E(x) = x1 * P(X = x1) + ... + xn * P(X = xn)

# Variancia
# Var(x) = E(x^2) - [E(x)]^2
var(x)

# Desvio padrao
# fórmula de desvio padrao: sd = sqrt(n * p * (1 - p))
sd(x)


# Distribuicao Binomial
dbinom(x, size = , prob = )

# Cumulative probability function for Binomial distribution
pbinom(x, size = , prob = )


# Distribuicao Poisson
# obs: lambda é a frequencia de ocorrencias.
dpois(x, lambda = )

# Cumulative function for Poisson distribution
ppois(x, lambda = )



# Distribuicao Normal
# obs: o desvio padrao é a raiz quadrada da variancia


# Exemplo 1: Seja um distribuicao normal com media 220 e variancia 16, calcule:

# P(210 <= X <= 228)
pnorm(228,220,4) - pnorm(210,220,4)

# P(X <= 225)
pnorm(225,220,4)


# Exemplo 2: Determina as probabilidades

# P(X > 0.6)
1 - pnorm(0.6)

# P(0.8 < X < 1.23)
pnorm(1.23) - pnorm(0.8)


# Exemplo 3: Distribuiçao normal com media de 170cm e variancia de 36cm(dp = 6)

# probabilidade de altura maior que 1,79m
1 - pnorm(179, 170, 6)

#ou

# calculando Z = (179 - 170)/6 = 1,5
1 - pnorm(1.5)


# Exemplo 4: Qual o tempo de prova, de modo que 95% dos alunos terminem no prazo,
# dada a média de 120 minutos e desvio padrao de 15 minutos.

qnorm(0.95, 120, 15)
