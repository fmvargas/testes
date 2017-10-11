---
title: "sales predict"
author: "Filipe"
date: "11 de outubro de 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

# Quanto de cada produto venderei nos próximos 3 meses?

# pacotes
# ---------
library(ggplot2)
library(caret)
library(dplyr)
library(lubridate)
library(TSclust)
library(forecast)

# --------
#load data
# --------

salesData <- read.csv("./desafio.csv/desafio.csv")
# ----

# Análise exploratória dos dados
# ------------------------------

str(salesData)

# verificação de dados ausentes
sum(is.na(salesData) == TRUE)
       
# Dimensional reduction

## Eliminar atributos que não contribuem para a análise:
'
Não contribuem: order_id, code, price, pis_cofins, icms, tax_substitution, liquid_cost, order_status, process_date, source_channel.
Contribuem: quantity, category, capture_date, process_status.
'
sales <- salesData[,c(3, 8, 11, 13)]

## Eliminar valores que não contribuem para a análise:

sales <- sales[sales$process_status == 'processado', -4]

## formatação de data
sales$capture_date <- as.Date(sales$capture_date)
str(sales)

## convert datetime to POSIXct
sales$capture_date <-as.POSIXct(sales$capture_date,
                                    format = "%Y-%m-%d",
                                    tz = "America/Sao_Paulo")

## agrupar por vendas diárias de cada produto

sales <- group_by(sales, capture_date,category)%>%summarise(un_vendidas=sum(quantity))

## agrupar por vendas mensais de cada produto

sales$month <- month(sales$capture_date)
sales$year <- year(sales$capture_date)
sales <- sales[sales$capture_date != '2017-06-01',]  # Eliminar valores do dia 01/06/2017, para os dados de 06/2017 não ficarem distorcidos.
sales <- na.omit(salesTESTE)

salesMonthly <- group_by(sales,
                         year, month,category) %>%
        summarize(un_vendidas=sum(un_vendidas))


#sazonalidade de venda de produtos

## diário
qplot(capture_date, category, data = sales)

## mensal
qplot(month, category, data = salesMonthly)


# ----

# a) separação em grupos de produtos, usando agrupamento não supervisionado.
# ---- 

# a.1) Criação das times series - cada produto, uma TS

# criação de objeto de vendas individuais por produto

prod01 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[1],-3] ; names(prod01) <- c("year", "month", "prod01")
prod02 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[2],-3] ; names(prod02) <- c("year", "month", "prod02")
prod03 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[3],-3] ; names(prod03) <- c("year", "month", "prod03")
prod04 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[4],-3] ; names(prod04) <- c("year", "month", "prod04")
prod05 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[5],-3] ; names(prod05) <- c("year", "month", "prod05")
prod06 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[6],-3] ; names(prod06) <- c("year", "month", "prod06")
prod07 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[7],-3] ; names(prod07) <- c("year", "month", "prod07")
prod08 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[8],-3] ; names(prod08) <- c("year", "month", "prod08")
prod09 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[9],-3] ; names(prod09) <- c("year", "month", "prod09")
prod10 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[10],-3] ; names(prod10) <- c("year", "month", "prod10")
prod11 <- salesMonthly[salesMonthly$category == levels(salesMonthly$category)[11],-3] ; names(prod11) <- c("year", "month", "prod11")

# criar e completar série com zeros nos meses vazios para que todas as séries tenham o mesmo tamanho

## criação dos vetores "year" e "month"
year <- c(rep(2016,7), rep(2017,5))
month <- c(6:12,1:5)


## criação do data frame inicial
products_salesInit <- data.frame(year, month)

## merge two data frames by ID and Country
products_sales <- merge(products_salesInit, prod01, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod02, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod03, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod04, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod05, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod06, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod07, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod08, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod09, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod10, by=c("year","month"), all.x =T)
products_sales <- merge(products_sales, prod11, by=c("year","month"), all.x =T)

## substituir NA's por zeros
products_sales[is.na(products_sales)] <- 0

# permanecer apenas com time Series

products_sales_TS <- products_sales[,- c(1,2)]

## clusterização

D1 <- diss(products_sales_TS, "COR")
summary(D1)
plot(D1)
C1 <- hclust(D1)
plot(C1)

# ----

# b) Previsão de venda para cada um dos produtos - demonstrar métricas de qualidade do modelo
# ----

# forecast

# fit an ARIMA model of order P, D, Q
fit1 <- arima(products_sales_TS$prod01)
fit2 <- arima(products_sales_TS$prod02)
fit3 <- arima(products_sales_TS$prod03)
fit4 <- arima(products_sales_TS$prod04)
fit5 <- arima(products_sales_TS$prod05)
fit6 <- arima(products_sales_TS$prod06)
fit7 <- arima(products_sales_TS$prod07)
fit8 <- arima(products_sales_TS$prod08)
fit9 <- arima(products_sales_TS$prod09)
fit10 <- arima(products_sales_TS$prod10)
fit11 <- arima(products_sales_TS$prod11)

# predictive accuracy

accuracy(fit1)
accuracy(fit2)
accuracy(fit3)
accuracy(fit4)
accuracy(fit5)
accuracy(fit6)
accuracy(fit7)
accuracy(fit8)
accuracy(fit9)
accuracy(fit10)
accuracy(fit11)

# predict next 3 observations

sales_predict_prod01 <- forecast(fit1, 3)
plot(sales_predict_prod01) 

sales_predict_prod02 <- forecast(fit2, 3)
plot(sales_predict_prod02) 

sales_predict_prod03 <- forecast(fit3, 3)
plot(sales_predict_prod03) 

sales_predict_prod04 <- forecast(fit4, 3)
plot(sales_predict_prod04) 

sales_predict_prod05 <- forecast(fit5, 3)
plot(sales_predict_prod05) 

sales_predict_prod06 <- forecast(fit6, 3)
plot(sales_predict_prod06) 

sales_predict_prod07 <- forecast(fit7, 3)
plot(sales_predict_prod07) 

sales_predict_prod08 <- forecast(fit8, 3)
plot(sales_predict_prod08) 

sales_predict_prod09 <- forecast(fit9, 3)
plot(sales_predict_prod09) 

sales_predict_prod10 <- forecast(fit10, 3)
plot(sales_predict_prod10) 

sales_predict_prod11 <- forecast(fit11, 3)
plot(sales_predict_prod11)

