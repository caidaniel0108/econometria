library(haven)
casen2022 <- read_dta("Base de datos Casen 2022 STATA.dta")
View(casen2022)
##### NÚMERO 1 #####
# Personas entre 18 y 60 años empleadas a tiempo completo
library(dplyr)
casen2022$o20 <- as.numeric(casen2022$o20)
casen2022 <- casen2022 %>%
  select(id_persona, edad, o10,
         o20, sexo, expr, esc, e6a,e6c_completo, ytrabajocor) %>% 
  filter(edad>=18 & edad<=60 & o20==1 & o10!=-88 & o10!=-99 & ytrabajocor!=-88 & ytrabajocor!=-99) %>%
  mutate(salarioxhora = ytrabajocor/o10) #46.766 obs

casen2022$univ <- ifelse(casen2022$e6a>=13 & casen2022$e6c_completo==1, 1,0)

casen2022$o10 <- as.numeric(casen2022$o10)
casen2022$o20 <- as.numeric(casen2022$o20)
casen2022$edad <- as.numeric(casen2022$edad)
casen2022$sexo <- as.factor(casen2022$sexo)
casen2022$esc <- as.numeric(casen2022$esc)
casen2022$e6a <- as.factor(casen2022$e6a)
casen2022$e6c_completo <- as.factor(casen2022$e6c_completo)
casen2022$ytrabajocor <- as.numeric(casen2022$ytrabajocor)
casen2022$univ <- as.numeric(casen2022$univ)
# TABLA RESUMEN: años de escolaridad, educación universitaria, ingreso laboral total, horas
# trabajadas, salario por hora trabajada.
# id_persona - id_vivienda - folio - edad - ytrabajocor
# - o10 - o20 - expr - sexo - esc

# educación universitaria se medirá como que tiene a menos nivel educacional 13 (profesional)
# es decir, e6a>=13 y e6c_completo=1.

tabla <- data.frame(variable=c("edad","horas","escolaridad","ingreso","salarioxhra","univ"),
                    media=c(mean(casen2022$edad, na.rm = TRUE),mean(casen2022$o10, na.rm = TRUE),mean(casen2022$esc, na.rm = TRUE),
                            mean(casen2022$ytrabajocor, na.rm = TRUE),mean(casen2022$salarioxhora, na.rm = TRUE),
                            mean(casen2022$univ, na.rm = TRUE)),
                    desv.std=c(sd(casen2022$edad, na.rm = TRUE),sd(casen2022$o10, na.rm = TRUE),sd(casen2022$esc, na.rm = TRUE),
                               sd(casen2022$ytrabajocor, na.rm = TRUE), sd(casen2022$salarioxhora, na.rm = TRUE),
                               sd(casen2022$univ, na.rm = TRUE)),
                    min=c(min(casen2022$edad, na.rm = TRUE),min(casen2022$o10, na.rm = TRUE),min(casen2022$esc, na.rm = TRUE),
                              min(casen2022$ytrabajocor, na.rm = TRUE), min(casen2022$salarioxhora, na.rm = TRUE),min(casen2022$univ, na.rm = TRUE)),
                    p5=c(quantile(casen2022$edad, .05,na.rm = TRUE),quantile(casen2022$o10, .05,na.rm = TRUE),
                         quantile(casen2022$esc,.05, na.rm = TRUE),quantile(casen2022$ytrabajocor,.05, na.rm = TRUE),
                         quantile(casen2022$salarioxhora, .05,na.rm = TRUE), quantile(casen2022$univ, .05,na.rm = TRUE)),
                    p95=c(quantile(casen2022$edad, .95,na.rm = TRUE),quantile(casen2022$o10, .95,na.rm = TRUE),
                         quantile(casen2022$esc,.95, na.rm = TRUE),quantile(casen2022$ytrabajocor,.95, na.rm = TRUE),
                         quantile(casen2022$salarioxhora, .95,na.rm = TRUE),quantile(casen2022$univ, .05,na.rm = TRUE)),
                    max=c(max(casen2022$edad, na.rm = TRUE),max(casen2022$o10, na.rm = TRUE),max(casen2022$esc, na.rm = TRUE),
                          max(casen2022$ytrabajocor, na.rm = TRUE), max(casen2022$salarioxhora, na.rm = TRUE),max(casen2022$univ, na.rm = TRUE)))

tabla[,2:7] <- round(tabla[,2:7], 2)

##### NÚMERO 2 #####
library(stargazer)
library(moderndive)

casen2022$sex <- ifelse(casen2022$sexo==1, 0,1) #0 hombre, 1 mujer

modelo <- lm(log(salarioxhora)~sexo, casen2022)
summary(modelo)
stargazer(modelo, type="text", align = TRUE)

# en promedio, los salarios de hombres y mujeres no son iguales puesto que
# hay una variación negativa respecto a las mujeres. Es decir, en promedio,las
# mujeres ganan un 10.2% menos que los hombres. Finalmente, se puede rechazar
# la hipótesis nula de que el sexo no es una variable significativa a cualquier
# nivel de significancia para explicar el salario.

#### NÚMERO 3 ####

deciles <- quantile(casen2022$esc, probs = seq(0,1,0.1), na.rm = TRUE)
casen2022$decil_var <- cut(casen2022$esc, breaks = deciles, labels = FALSE)


#### NÚMERO 4 ####

modelo2 <- lm(log(salarioxhora)~esc, casen2022)
stargazer(modelo2, type="text", align = TRUE)

# Al igual que en la figura anterior, el coeficiente de escolaridad es positivo.
# Lo que quiere decir, a grandes rasgos, que la escolaridad aporta de forma
# positiva a la explicación del salario. Es decir, mayores años de escolaridad
# aumentan, en promedio, un 9.92% el salario por hora.


#### NÚMERO 5 ####

modelo3 <- lm(esc~sex, casen2022)
stargazer(modelo3, type="text", align = TRUE)

# Dado el coeficiente de la variable sexo (1.092), en promedio los hombres
# tienen una mayor cantidad de años de escolaridad, lo que es significativo
# a todos los niveles de significancia.


#### NÚMERO 6 ####

modelo4 <- lm(log(salarioxhora)~sex+esc, casen2022)
stargazer(modelo4, type="text", align = TRUE)

# Luego de controlar por los años de escolaridad y sexo, la diferencia entre el
# salario de hombres y mujeres es mayor aún. Antes era de 10.2% y ahora es de
# 20.08%, por lo que se deduce que además del sexo, la escolaridad tiene
# un alto impacto en los salarios por hora. Por otra parte, son coeficientes
# significativos.