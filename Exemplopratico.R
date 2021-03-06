#############################################################
#                                                           #
# Minicurso: Rstudio - Introdu��o aos modelos mistos        #
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

## Vamos tra�ar os dados com ggplot2

library(ggplot2) ##carregamos o pacote


ggplot(........, aes(x = ComprimentoCorpo, y = Pontuacao)) +  #Complete Aqui
  geom_point() + geom_smooth(method = "lm")


#Parece que condores maiores saem melhor no teste de intelig�ncia. Isso parece um pouco estranho: 
# o tamanho n�o deve realmente afetar os resultados das pontua��es dos testes.

# E as  suposi��es s�o satisfeitas?

# Plotemos os res�duos: a linha vermelha deve ser quase plana, 
#como a linha cinza tracejada:

plot(modelo1, which = 1) ## imperfeito

## mas como este � um exemplo fict�cio, n�s iremos com ele
## para seus pr�prios dados, tenha cuidado:

## D� uma olhada r�pida no qqplot tamb�m: 
## os pontos devem idealmente cair na linha tracejada diagonal:

plot(......, which = 2)   ##Complete codigo aqui

##No entanto, o que dizer de independ�ncia de observa��o? 
## Nossos dados s�o independentes? Coletamos v�rias amostras de oito cadeias de montanhas. 
##� perfeitamente plaus�vel que os dados de cada cadeia de montanhas sejam mais semelhantes 
#entre si do que os dados de diferentes cadeias montanhosas: eles s�o correlacionados.

ggplot(......, aes(x = Montanha, y =  ....... , fill = Montanha)) + 
  geom_boxplot()  # Complete seu codigo aqui

#Poder�amos tamb�m plotar os pontos de cor e cor por Montanha:

ggplot(condor, aes(x = ComprimentoCorpo, y = Pontuacao, colour = .......)) + ##Complete seu codigo aqui
  geom_point(size = 2) +
  theme_classic())   


#Parece que as nossas montanhas variam tanto no comprimento do corpo do condor 
#quanto nas pontua��es dos testes. 
#Isso confirma que nossas observa��es dentro de cada uma das faixas n�o s�o independentes. 
#N�s n�o podemos ignorar isso

##Ent�o, o que faremos?

#Poder�amos executar muitas an�lises separadas e ajustar uma regress�o para cada uma das  montanhas.
#Vamos dar uma r�pida olhada nos dados divididos por montanha. 
#N�s usamos o facet_wrap para fazer isso:

ggplot(aes(ComprimentoCorpo, Pontua��o), data = ......) + #Complete seu codigo aqui
  geom_point() + 
  facet_wrap(~ Montanha) + 
  xlab("Comprimento do Corpo do Condor") + 
  ylab(" Pontua��o do teste")   

##Ent�o s�o oito an�lises. E tamb�m temos locais diferentes, 
#o que, da mesma forma que as montanhas, n�o s�o independentes. 
#Assim, podemos executar uma an�lise para cada local em cada intervalo separadamente.

#Para fazer isso, ter�amos que estimar um par�metro de inclina��o e intercepto para cada regress�o. Veja bem...S�o dois par�metros, tr�s locais e oito montanhas, o que significa 48 estimativas de par�metros (2 x 3 x 8 = 48)! Al�m disso, 


# Modifique o modelo atual

#Queremos usar todos os dados, mas contabilizar os dados provenientes de diferentes montanhas 
#(vamos colocar locais em espera por um segundo para simplificar as coisas).
#Adicione a montanha como efeito fixo ao nosso modelo2.


modelo2 <- lm(.......... ~ ComprimentoCorpo + ........... , data = ........) #Complete seu codigo aqui
summary(.....) #complete seu codigo aqui

#Agora o comprimento do corpo n�o � significativo. 
#O modelo acima est� estimando a diferen�a nas pontua��es dos testes entre as montanhas, 
#podemos ver todas elas na sa�da do modelo por summary(). Mas n�o estamos interessadas nisso, apenas queremos saber se o comprimento do corpo afeta os resultados dos testes 
#e queremos simplesmente controlar a varia��o proveniente das montanhosas.

#Isto � o que nos referimos como "fatores aleat�rios" e assim chegamos a modelos de efeitos mistos.

#######################################
##### Modelos de efeitos mistos #######
#######################################

#Um modelo misto � uma boa escolha aqui: 
#ele nos permitir� usar todos os dados que temos (maior tamanho de amostra) e explicar as correla��es entre os dados provenientes dos locais e as montanhas. Tamb�m estimaremos menos par�metros e evitaremos problemas com compara��es m�ltiplas 
#que encontrar�amos usando regress�es separadas.

#N�s vamos trabalhar com a *nlme*, ent�o carregue o pacote (ou use install.packagesse voc� n�o tiver nlme no R).

library(.....) #complete o nome do pacote aqui.


#Neste caso particular, estamos procurando controlar os efeitos da montanha. 
#N�s n�o experimentamos todas as  montanhas do mundo (temos oito), 
#ent�o nossos dados s�o apenas uma amostra de todas as montanhas existentes. 
#N�o estamos realmente interessados no efeito de cada montanha espec�fica na pontua��o do teste, 
#mas sabemos que as pontua��es dos testes podem ser correlacionadas, portanto, queremos controlar isso. Se especificamente escolhemos oito montanhas a priori e estiv�ssemos interessados  nesses intervalos e quis�ssemos fazer previs�es sobre eles, ent�o a montanha seria montada como um efeito fixo. 

#Temos uma vari�vel resposta, a pontua��o do teste e estamos tentando explicar 
#parte da varia��o na pontua��o do teste atrav�s do ajuste do comprimento do corpo 
#como um efeito fixo. Mas a vari�vel resposta tem alguma varia��o residual 
#(ou seja, varia��o inexplic�vel) associada a cadeias de montanhas.  
#Ao usar efeitos aleat�rios, estamos modelando essa varia��o inexplic�vel

modelomis1 <- lme(fixed= ...... ~ ComprimentoCorpo, random= ~1|Montanha, data = .....) #complete aqui
summary(.....) #Complete seu codigo aqui

#Podemos ver a varia��o para montanha = 18.43, que s�o importantes: 
#elas explicam muita varia��o, como sabemos disso? Podemos pegar a vari�ncia para a
#Montanha dividi-lo pela vari�ncia total: 18.43016/(18.43016+ 14.96042) # ~55%
#Assim, as diferen�as entre montanhas explicam ~ 55% da vari�ncia, Tenha em mente que 55% da varia��o "sobraram" ap�s a varia��o explicada pelos nossos efeitos fixos.
#Uma vez que consideramos as cadeias de montanhas, � �bvio que o comprimento do corpo do condor n�o explica realmente as diferen�as nas pontua��es dos testes. Tenha em mente que o efeito aleat�rio da serra � destinado a capturar todas as influ�ncias de cadeias de montanhas em resultados de testes se observamos essas influ�ncias explicitamente ou n�o, se essas influ�ncias s�o grandes ou pequenos etc . Podem ser muitas, muitas pequenas influ�ncias que, quando combinadas, afetam as pontua��es dos testes e � isso que esperamos controlar.

#� uma boa pr�tica dar uma olhada nos gr�ficos para verificar nossas suposi��es:


plot(.....) #Complete aqui seu codigo

qqnorm(resid(.......))#Complete aqui 
qqline(resid(.......))#Complete aqui 

############################
#Nosso segundo modelo misto#
############################


#Podemos ir em frente e encaixar um novo modelo, 
#que leve em conta tanto as diferen�as entre as cadeias de montanhas 
#quanto as diferen�as entre os locais dentro dessas cadeias montanhosas

modelomis2<- .....(fixed= Pontuacao~ ........, random= ~1|Montanha/Local, data=........) #Complete aqui
summary(......) # Complete Aqui


#Vamos a visualizar o que est� acontecendo.  
#Voc� deve ser capaz de ver oito cadeias de montanhas com tr�s locais (diferentes pontos de cor) 
#dentro delas, com uma linha ajustada em cada local.


ggplot(....., aes(x = ComprimentoCorpo, y = ......., colour = Local)) +   #Complete Aqui
  facet_wrap(~......, nrow=3) + geom_point() + theme_classic() + #Complete Aqui
  geom_line(data = cbind(condor, pred = predict(modelomis2)), aes(y = pred)) +
  theme()


##Bem feito por chegar aqui! 
#Agora voc� viu que a falha em explicar a correla��o nos dados poderia levar a 
#resultados enganosos, parecia que a comprimento do corpo afetava a pontua��o do teste 
#at� que explic�ssemos a varia��o proveniente das cadeias de montanhas.
#Podemos ver agora que o comprimento do corpo n�o influencia os resultados dos testes

#�timo! Podemos escolher Condores menores para qualquer treinamento futuro 
#os menores devem ser mais gerenci�veis








