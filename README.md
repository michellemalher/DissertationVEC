# Dissertação de Mestrado - Modelos de inflação de preços e salários no Brasil

### Introdução 

Este repositório contém o código de um dos dos modelos VEC desenvolvidos para a dissertação de mestrado em Economia na UFRJ. 

- O **objetivo** é investigar a relação entre inflação de preços e de salários no Brasil e seus determinantes.
- Para isso, é estimada uma curva de Phillip que incorporam elementos de custo e de demanda para identificar quais exerceram maior influência sobre o IPCA no período de análise.
- O modelo econométrico utilizado é um VEC com janela temporal de março de 2013 a janeiro de 2022

### Base de dados

- Inflação de preços: IPCA geral (IBGE)
- Inflação de salários: rendimento da (PNADC Mensal)
- inflação importada: índice de commodities do BCB (BCB)
- Índice de demanda: hiato do PIB (BCB/IBGE)

### Resultados
- Impacto positivo da inflação salarial na inflação de preços a partir do décimo mês
- Não sugere impacto positivo no sentido contrário
- Impacto positivo do IC-Br na inflação de preços em todos os modelos 
- Quando IC-Br inserido em 1 defasagem, seu impacto continuou significativo e com magnitude similar
- Inidcador de demanda não se mostrou estatisticamente significativo

### Gráficos de IRF
![irf](https://user-images.githubusercontent.com/7675006/235946920-c6333758-aa0b-47e3-96db-0240bc7362d1.png)
