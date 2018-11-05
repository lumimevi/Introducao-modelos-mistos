#############################################################
#                                                           #
# Minicurso: Rstudio - Introdução aos modelos mistos        #
#                                                           #
############################################################

## Ministrante: Lourdes Milagros Mendoza Villavicencio
##Contato: lumimevi@gmail.com

##Data: 07/11/2018


###---- Explorando os Dados -----###

## Carregue o diretorio do trabalho

setwd("...... ") #complete aqui

## Chamar ao Banco de Dados

condor<- read.table("condor.csv" , ",", header=TRUE)
attach(.....)  #Complete codigo aqui

## Vamos visualizar parte dos dados

head(.... ) #complete codigo aqui

##Vamos visualizar nosso dados
View(condor)


#Vamos a visualizar os nomes das variaveis de nosso banco de dados
names( ..........) ##complete codigo aqui

## Queremos saber como o comprimento do corpo dos condores afeta aos resultados dos testes.


##Uma maneira de analisar esses dados seria ajustar um modelo linear a 
##todos os nossos dados, ignorando os locais e as cadeias montanhosas por enquanto.


modelo1 <- lm(...... ~ ....., data = condor) #Complete codigo aqui
anova(modelo1) 

## Vamos traçar os dados com ggplot2

library(ggplot2) ##carregamos o pacote


ggplot(........, aes(x = ComprimentoCorpo, y = Pontuacao)) +  #Complete Aqui
  geom_point() + geom_smooth(method = "lm")


#Parece que condores maiores saem melhor no teste de inteligência. Isso parece um pouco estranho: 
# o tamanho não deve realmente afetar os resultados das pontuações dos testes.

# E as  suposições são satisfeitas?

# Plotemos os resíduos: a linha vermelha deve ser quase plana, 
#como a linha cinza tracejada:

plot(modelo1, which = 1) ## imperfeito

## mas como este é um exemplo fictício, nós iremos com ele
## para seus próprios dados, tenha cuidado:

## Dê uma olhada rápida no qqplot também: 
## os pontos devem idealmente cair na linha tracejada diagonal:

plot(......, which = 2)   ##Complete codigo aqui

##No entanto, o que dizer de independência de observação? 
## Nossos dados são independentes? Coletamos várias amostras de oito cadeias de montanhas. 
##É perfeitamente plausível que os dados de cada cadeia de montanhas sejam mais semelhantes 
#entre si do que os dados de diferentes cadeias montanhosas: eles são correlacionados.

ggplot(......, aes(x = Montanha, y =  ....... , fill = Montanha)) + 
  geom_boxplot()  # Complete seu codigo aqui

#Poderíamos também plotar os pontos de cor e cor por Montanha:

ggplot(condor, aes(x = ComprimentoCorpo, y = Pontuacao, colour = .......)) + ##Complete seu codigo aqui
  geom_point(size = 2) +
  theme_classic())   


#Parece que as nossas montanhas variam tanto no comprimento do corpo do condor 
#quanto nas pontuações dos testes. 
#Isso confirma que nossas observações dentro de cada uma das faixas não são independentes. 
#Nós não podemos ignorar isso

##Então, o que faremos?

#Poderíamos executar muitas análises separadas e ajustar uma regressão para cada uma das  montanhas.
#Vamos dar uma rápida olhada nos dados divididos por montanha. 
#Nós usamos o facet_wrap para fazer isso:

ggplot(aes(ComprimentoCorpo, Pontuação), data = ......) + #Complete seu codigo aqui
  geom_point() + 
  facet_wrap(~ Montanha) + 
  xlab("Comprimento do Corpo do Condor") + 
  ylab(" Pontuação do teste")   

##Então são oito análises. E também temos locais diferentes, 
#o que, da mesma forma que as montanhas, não são independentes. 
#Assim, podemos executar uma análise para cada local em cada intervalo separadamente.

#Para fazer isso, teríamos que estimar um parâmetro de inclinação e intercepto para cada regressão. Veja bem...São dois parâmetros, três locais e oito montanhas, o que significa 48 estimativas de parâmetros (2 x 3 x 8 = 48)! Além disso, 


# Modifique o modelo atual

#Queremos usar todos os dados, mas contabilizar os dados provenientes de diferentes montanhas 
#(vamos colocar locais em espera por um segundo para simplificar as coisas).
#Adicione a montanha como efeito fixo ao nosso modelo2.


modelo2 <- lm(.......... ~ ComprimentoCorpo + ........... , data = ........) #Complete seu codigo aqui
summary(.....) #complete seu codigo aqui

#Agora o comprimento do corpo não é significativo. 
#O modelo acima está estimando a diferença nas pontuações dos testes entre as montanhas, 
#podemos ver todas elas na saída do modelo por summary(). Mas não estamos interessadas nisso, apenas queremos saber se o comprimento do corpo afeta os resultados dos testes 
#e queremos simplesmente controlar a variação proveniente das montanhosas.

#Isto é o que nos referimos como "fatores aleatórios" e assim chegamos a modelos de efeitos mistos.

#######################################
##### Modelos de efeitos mistos #######
#######################################

#Um modelo misto é uma boa escolha aqui: 
#ele nos permitirá usar todos os dados que temos (maior tamanho de amostra) e explicar as correlações entre os dados provenientes dos locais e as montanhas. Também estimaremos menos parâmetros e evitaremos problemas com comparações múltiplas 
#que encontraríamos usando regressões separadas.

#Nós vamos trabalhar com a *nlme*, então carregue o pacote (ou use install.packagesse você não tiver nlme no R).

library(.....) #complete o nome do pacote aqui.


#Neste caso particular, estamos procurando controlar os efeitos da montanha. 
#Nós não experimentamos todas as  montanhas do mundo (temos oito), 
#então nossos dados são apenas uma amostra de todas as montanhas existentes. 
#Não estamos realmente interessados no efeito de cada montanha específica na pontuação do teste, 
#mas sabemos que as pontuações dos testes podem ser correlacionadas, portanto, queremos controlar isso. Se especificamente escolhemos oito montanhas a priori e estivéssemos interessados  nesses intervalos e quiséssemos fazer previsões sobre eles, então a montanha seria montada como um efeito fixo. 

#Temos uma variável resposta, a pontuação do teste e estamos tentando explicar 
#parte da variação na pontuação do teste através do ajuste do comprimento do corpo 
#como um efeito fixo. Mas a variável resposta tem alguma variação residual 
#(ou seja, variação inexplicável) associada a cadeias de montanhas.  
#Ao usar efeitos aleatórios, estamos modelando essa variação inexplicável

modelomis1 <- lme(fixed= ...... ~ ComprimentoCorpo, random= ~1|Montanha, data = .....) #complete aqui
summary(.....) #Complete seu codigo aqui

#Podemos ver a variação para montanha = 18.43, que são importantes: 
#elas explicam muita variação, como sabemos disso? Podemos pegar a variância para a
#Montanha dividi-lo pela variância total: 18.43016/(18.43016+ 14.96042) # ~55%
#Assim, as diferenças entre montanhas explicam ~ 55% da variância, Tenha em mente que 55% da variação "sobraram" após a variação explicada pelos nossos efeitos fixos.
#Uma vez que consideramos as cadeias de montanhas, é óbvio que o comprimento do corpo do condor não explica realmente as diferenças nas pontuações dos testes. Tenha em mente que o efeito aleatório da serra é destinado a capturar todas as influências de cadeias de montanhas em resultados de testes se observamos essas influências explicitamente ou não, se essas influências são grandes ou pequenos etc . Podem ser muitas, muitas pequenas influências que, quando combinadas, afetam as pontuações dos testes e é isso que esperamos controlar.

#É uma boa prática dar uma olhada nos gráficos para verificar nossas suposições:


plot(.....) #Complete aqui seu codigo

qqnorm(resid(.......))#Complete aqui 
qqline(resid(.......))#Complete aqui 

############################
#Nosso segundo modelo misto#
############################


#Podemos ir em frente e encaixar um novo modelo, 
#que leve em conta tanto as diferenças entre as cadeias de montanhas 
#quanto as diferenças entre os locais dentro dessas cadeias montanhosas

modelomis2<- .....(fixed= Pontuacao~ ........, random= ~1|Montanha/Local, data=........) #Complete aqui
summary(......) # Complete Aqui


#Vamos a visualizar o que está acontecendo.  
#Você deve ser capaz de ver oito cadeias de montanhas com três locais (diferentes pontos de cor) 
#dentro delas, com uma linha ajustada em cada local.


ggplot(....., aes(x = ComprimentoCorpo, y = ......., colour = Local)) +   #Complete Aqui
  facet_wrap(~......, nrow=3) + geom_point() + theme_classic() + #Complete Aqui
  geom_line(data = cbind(condor, pred = predict(modelomis2)), aes(y = pred)) +
  theme()


##Bem feito por chegar aqui! 
#Agora você viu que a falha em explicar a correlação nos dados poderia levar a 
#resultados enganosos, parecia que a comprimento do corpo afetava a pontuação do teste 
#até que explicássemos a variação proveniente das cadeias de montanhas.
#Podemos ver agora que o comprimento do corpo não influencia os resultados dos testes

#ótimo! Podemos escolher Condores menores para qualquer treinamento futuro 
#os menores devem ser mais gerenciáveis








