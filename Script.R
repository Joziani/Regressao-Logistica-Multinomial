

## Pacotes ---------------------------------------------------------------------

if(!require(tidyverse)){install.packages('tidyverse'); require(tidyverse)} 
if(!require(VGAM)){install.packages('VGAM'); require(VGAM)} 


## Modelo ----------------------------------------------------------------------

dados <- readxl::read_xlsx('RegressãoMultinomial.xlsx')
dados

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


## Coeficiente de Determinação -------------------------------------------------

prob <- predict(modelo, type = 'response')
pred <- apply(prob, MARGIN = 1, FUN = which.max)
pred <- factor(pred, labels = levels(as.factor(dados$Voto)))

R2 <- cbind(dados$Voto, pred) %>% 
  Hmisc::rcorr(type = 'spearman')

print(paste0(round((R2$r[2,1]^2)*100, 2), '%'))


## Acurácia, Sensibilidade e Especificidade ------------------------------------

mod.confusion <- caret::confusionMatrix(pred, as.factor(dados$Voto))
mod.accuracy <- round(as.numeric(mod.confusion$overall[1]),3)
mod.sensitivity <- round(as.numeric(mod.confusion$byClass[,1]),3)
mod.specificity <- round(as.numeric(mod.confusion$byClass[,2]),3)

tabela <- data.frame(Acurácia = mod.accuracy,
                     Sensibilidade = mod.sensitivity,
                     Especificidade = mod.specificity)

rownames(tabela) <- c('Donald Trump', 'Hillary Clinton', 'Outros')

tabela