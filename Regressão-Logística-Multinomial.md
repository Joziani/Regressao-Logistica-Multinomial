Regressão Logística Multinomial
================
Joziani Mota Vieira

<div style='text-align: justify'>

# Conceitos

## Regressão Linear e Logística

A **Análise de Regressão** é utilizada para verificar se há relação
entre a variável resposta, ou também chamada de variável dependente, com
uma ou mais variáveis independentes.

A Regressão mais conhecida é a regressão linear, sendo amplamente
utilizada e a mais simples. Para esse método é necessário que a variável
resposta seja quantitativa contínua. E é necessário que a variável
resposta tenha uma relação linear com as variáveis independentes.

Temos também a Regressão Logística que é usada quando a variável
resposta é uma variável categórica binária, ou seja, com apenas duas
categorias.

## Regressão Logística Multinomial

A *Regressão Logística Multinomial* é similar à Regressão Logística, a
diferença é que para utilizá-la a variável resposta deve ser qualitativa
nominal com três ou mais categorias. Entre as categorias deve-se
escolher uma de referência, assim como na regressão logística binária,
para conseguir compará-la com as outras categorias. O recomendado é que
as categorias sejam 0, 1, 2, …, k, sendo k a quantidade de categorias da
variável resposta menos um. Assim, a categoria 0 será a de referência.

# Exemplos no cotidiano

  - Pode ser usada para prever qual melhor plano de saúde, odontológico
    etc. Dentre três ou mais planos;
  - Previsão de faixa etária;
  - Mostrar os fatores que influenciam na escolha do modo de transporte
    por estudantes universitários, idosos etc.;
  - Análise de sentimentos.

# Aplicação (Script)

  - O banco de dados contém informações de 167 eleitores que votaram nas
    eleições de 2016 nos Estados Unidos. O banco traz as seguintes
    informações: em quem o eleitor votou (Donald Trump - 0, Hillary
    Clinton - 1 ou Outros - 2), qual a sua etnia (não-caucasiano - 0 ou
    caucasiano - 1), qual a sua confiança na política (avaliada em uma
    escala de 1 a 10) e o resultado de uma escala sobre liberalismo
    econômico (também, de 1 a 10). Vamos avaliar quais desses fatores
    influenciam na decisão de voto.

  - O pacote utilizado é o **VGAM**.

<!-- end list -->

``` r
if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)} # Manipulação de dados
if(!require(VGAM)){install.packages('VGAM'); require(VGAM)} # Pacote com a função ara criar o modelo
```

## Modelo

``` r
dados <- readxl::read_xlsx('RegressãoMultinomial.xlsx')
dados
```

    ## # A tibble: 167 x 4
    ##     Voto Etnia Escala_conf Lib_econ
    ##    <dbl> <dbl>       <dbl>    <dbl>
    ##  1     2     1           3        3
    ##  2     1     1           2        7
    ##  3     0     1           7        1
    ##  4     0     1           9        5
    ##  5     1     1           2        6
    ##  6     0     1           6        3
    ##  7     0     1           6        3
    ##  8     1     0           6       10
    ##  9     0     1           6        4
    ## 10     2     1           7        8
    ## # ... with 157 more rows

``` r
modelo <- VGAM::vglm(formula = Voto ~ ., 
                      family = multinomial, 
                      data = dados)

t0 <- summary(modelo)@coef3
O.R. <- exp(t0[,1])
L.I <- exp(confintvglm(modelo)[,1])
L.S <- exp(confintvglm(modelo)[,2])
I.C.95 <- c()
for(i in 1:length(O.R.)) {
  I.C.95[i] <- paste('[', format(round(L.I[i], 2), nsmall=2),
                     '; ', format(round(L.S[i], 2), nsmall=2),
                     ']', sep='')}
t.final <- data.frame(round(O.R.,2), I.C.95, round(t0[,4], 3))
colnames(t.final) <- c('O.R.', 'I.C. - 95% (O.R.)', 'Valor-p')
rownames(t.final) <- c('Intercepto - 1',
                       'Intercepto - 2',
                       'Etnia - 1',
                       'Etnia - 2',
                       'Escala de confiança - 1',
                       'Escala de confiança - 2',
                       'Liberalismo Econômico - 1',
                       'Liberalismo Econômico - 2')
t.final
```

    ##                           O.R. I.C. - 95% (O.R.) Valor-p
    ## Intercepto - 1            1.20      [0.33; 4.37]   0.787
    ## Intercepto - 2            0.05      [0.01; 0.27]   0.000
    ## Etnia - 1                 4.78     [1.81; 12.59]   0.002
    ## Etnia - 2                 1.47      [0.58; 3.75]   0.421
    ## Escala de confiança - 1   1.12      [0.96; 1.30]   0.163
    ## Escala de confiança - 2   1.14      [0.96; 1.34]   0.129
    ## Liberalismo Econômico - 1 0.75      [0.62; 0.90]   0.003
    ## Liberalismo Econômico - 2 1.51      [1.24; 1.86]   0.000

## Coeficiente de Determinação

``` r
## Forma 1

prob <- predict(modelo, type = 'response')
pred <- apply(prob, MARGIN = 1, FUN = which.max)
pred <- factor(pred, labels = levels(as.factor(dados$Voto)))

R2 <- cbind(dados$Voto, pred) %>% 
    Hmisc::rcorr(type = 'spearman')

print(paste0(round((R2$r[2,1]^2)*100, 2), '%'))
```

    ## [1] "17.09%"

## Acurácia, Sensibilidade e Especificidade

``` r
mod.confusion <- caret::confusionMatrix(pred, as.factor(dados$Voto))
mod.accuracy <- round(as.numeric(mod.confusion$overall[1]),3)
mod.sensitivity <- round(as.numeric(mod.confusion$byClass[,1]),3)
mod.specificity <- round(as.numeric(mod.confusion$byClass[,2]),3)

tabela <- data.frame(Acurácia = mod.accuracy,
           Sensibilidade = mod.sensitivity,
           Especificidade = mod.specificity)

rownames(tabela) <- c('Donald Trump', 'Hillary Clinton', 'Outros')

tabela
```

    ##                 Acurácia Sensibilidade Especificidade
    ## Donald Trump       0.665         0.813          0.717
    ## Hillary Clinton    0.665         0.745          0.812
    ## Outros             0.665         0.243          0.931
