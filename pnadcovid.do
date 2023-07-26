

ren V1012 semana_p

ren V1013 mes_p

ren A001B1 dia

ren A001B2 mes

ren A001B3 ano_DN 

ren A002 idade

ren A003 sexo

ren A001A condição_domicilio





**************Identificação do indivíduo no domicílio através da data de nascimento
*********Trasformando variável numérica em string
tostring UF dia mes ano_DN sexo, replace
gen id=UF+dia+mes+ano_DN+sexo

order UF dia mes ano_DN sexo id




************************Indentificação domicílio (família) 
egen float dom = group(UPA V1008 )

sort dom A001


order UF UPA V1008 dom id A001 mes_p


*************Eliminar aos individuos que não declararam a DN***********
destring UF dia mes ano_DN sexo, replace
drop if dia==99





*************************Crinado novo ID******************
egen float ID = group(dom id)

order UF UPA V1008 dom id ID A001

sort dom ID




gen branco=1 if A004==1
replace branco=. if A004==9
replace branco=0 if A004>1


gen homem=1 if sexo==1
replace homem=0 if sexo==2


gen urbana=1 if V1022==1
replace urbana=0 if V1022==2


************Regiões*************

gen norte=1 if UF<18
replace norte=0 if norte==.

gen nordeste=1 if UF>=20 & UF<30
replace nordeste=0 if nordeste==.

gen sudeste=1 if UF>30 & UF<40
replace sudeste=0 if sudeste==.

gen sul=1 if UF>40 & UF<50
replace sul=0 if sul==.

gen centro_oeste=1 if UF>=50
replace centro_oeste=0 if centro_oeste==.



**************Morar na capital 
gen capital=1 if CAPITAL~=.
replace capital=0 if capital==.


****DUMMIES PARA AREA DE RESIDENCIA******

recode V1023 (1 2 3 =1) (4=0), gen(rm) // regiao metropolitana ou ride
label var rm "região metropolitana=1" 



************************Variáveis Família*************
*************Condição no domicílio 
gen cond_fam=condição_domicilio
recode cond_fam (1 =1)(2 3 =2) (4 5 6 =3) (7 8 9 10 11 12 13 14 15 16 17 18 19 20 =0) 
label var cond_fam "0 outro, 1 chefe, 2  conjuge, 3 filho"
tab cond_fam


************Criando variável filhos **************
bysort dom: egen filhos=total(cond_fam==3)


*****************Variável mulher com filho(chefe ou conjuge)*********
gen mae=1 if filhos>0 & sexo==2 & cond_fam==1 
replace mae=1 if filhos>0 & sexo==2 & cond_fam==2
replace mae=0 if mae==.



gen chefe=1 if cond_fam==1
replace chefe=0 if chefe==.

gen conju=1 if cond_fam==2
replace conju=0 if conju==.


gen outro=1 if cond_fam==0
replace outro=0 if outro==.



************Saber se tem conjuge (domicilio com conjuge)**********
bysort dom: egen conj=total(cond_fam==2)

***************Conjuge do chefe*************
gen conj1=1 if conj>0 & chefe==1
replace conj1=0 if conj1==.

*************Conjuge do cond_fam=2(conjuge)*************
gen conj2=1 if conj>0 & conju==1
replace conj2=0 if conj2==.


gen conjuge=conj1+conj2


order UF cond_fam mes_p sexo id dom q_filhos conj conju conj1 conj2



************Saber se tem filho (conjuge ou chefe)**********
***************filho do chefe*************
gen filho1=1 if filhos>0 & chefe==1
replace filho1=0 if filho1==.

*************filho do cond_fam=2(conjuge)*************
gen filho2=1 if filhos>0 & conju==1
replace filho2=0 if filho2==.


gen tem_filho=filho1+filho2




************Saber se tem filho com até 6 anos **********
gen f6=1 if filhos>0 & idade<=6 & cond_fam==3
replace f6=0 if f6==.

bysort dom: egen t_f6=total(f6==1)

***************filho do chefe*************
gen filho6ch=1 if t_f6>0 & chefe==1
replace filho6ch=0 if filho6ch==.

*************filho do cond_fam=2(conjuge)*************
gen filho6co=1 if t_f6>0 & conju==1
replace filho6co=0 if filho6co==.

gen filho6=filho6ch+filho6co






*****CAPITAL HUMANO*******

*******************Níveis de Educação******************
gen sem_educ=1 if A005==1 | A005==2
replace sem_educ=0 if  A005>2

gen fundamental=1 if  A005==3 |  A005==4
replace fundamental=0 if  A005==1 |  A005==2
replace fundamental=0 if  A005>4

gen medio=1 if  A005==5 |  A005==6
replace medio=0 if  A005<5
replace medio=0 if  A005>6

gen superior=1 if  A005==7 | A005==8
replace superior=0 if  superior==.


gen idade2=idade^2



keep if idade>=14 & idade<=65  // selecionando grupo de idade de 15 a 65 anos


**********Criando os grupos de idade************
gen id1=1 if idade<25
replace id1=0 if id1==.

gen id2=1 if idade>=25 & idade<35
replace id2=0 if id2==.

gen id3=1 if idade>=35 & idade<45
replace id3=0 if id3==.

gen id4=1 if idade>=45 & idade<55
replace id4=0 if id4==.

gen id5=1 if idade>=55 
replace id5=0 if id5==.




 
*****VAR DE MERCADO DE TRABALHO*******
***************Ter mais de um trabalho************
 gen trab2=1 if C006==1
 replace trab2=0 if C006==2
 
 
********************carteira de trabalho assinada*****
gen carteira=1 if C007B==1 | C007B==2
replace carteira=0 if C007B==3



*****dummies de ocupação do trabalho principal *************
tab C007, gen (ocup)



*****dummies de tipo de trabalho, cargo ou função *************
tab C007C, gen (ativ)

gen domestico=1 if C007C==1 | C007==2 
replace domestico=0 if C007>2 


gen escritorio=1 if ativ3==1 | ativ4==1 | ativ5==1 
replace escritorio=0 if escritorio==.

gen comercio=1 if ativ6==1 | ativ7==1 | ativ8==1 | ativ9==1 | ativ10==1
replace comercio=0 if comercio==.

gen serviços=1 if ativ11==1 | ativ18==1 | ativ19==1 | ativ20==1 | ativ21==1 | ativ22==1 | ativ23==1 
replace serviços=0 if serviços==. 

gen agricultura=1 if ativ12==1 | ativ13==1
replace agricultura=0 if agricultura==. 

gen motorista=1 if ativ14==1 | ativ15==1 | ativ16==1 | ativ17==1
replace motorista=0 if motorista==. 

gen educação=1 if ativ24==1 | ativ25==1
replace educação=0 if educação==. 

gen saude=1 if ativ26==1 | ativ27==1
replace saude=0 if saude==. 

gen segurança=1 if ativ29==1 | ativ30==1
replace segurança=0 if segurança==. 

gen artista=1 if ativ32==1
replace artista=0 if artista==.

gen nivel_superior=1 if ativ34==1
replace nivel_superior=0 if nivel_superior==.

gen tecnico=1 if ativ35==1
replace tecnico=0 if tecnico==. 

gen outros=1 if ativ36==1
replace outros=0 if outros==.


gen risco=1 if comercio==1 | saude==1 | educação==1 | artista==1 | ativ28==1 | ativ21==1 | segurança==1 | ativ14==1 | ativ3==1 | ativ5==1 | ativ17==1
replace risco=0 if risco==. & Ano==2020





*************Estar afastado de algum trabalho
gen afastado=1 if C002==1
replace afastado=0 if C002==2


***************Ter trabalhado pelo menos uma hora ou fez bico na semana passada********
gen empregado=1 if C001==1
replace empregado=0 if C001==2





**********Horas que normalmente trabalhava por semana (todos os trabalhos)***********
gen n_horas=C008

**********Horas trabalhadas de fato por semana (todos os trabalhos)***********
gen horas=C009

gen hr_mes= horas*4 //horas mes

****************Salario todos os trabalhos************
gen salario=C01012


gen sal_hr=salario/hr_mes //rendimento por hora

gen lnw=ln(sal_hr) //ln do rendimento hora






********************Para eliminar os individuos que não estiveram nos 2 períodos***
gen t=1 if Ano==2019
replace t=2 if Ano==2020


bysort ID: egen total_t=total(t)

drop if total_t<3
drop if total_t3




****************Comando para eliminar individuos duplicados por ano********
duplicates report ID t

duplicates tag ID t, gen(isdup)

drop if isdup>0







****************Empregado-Desempregado=*****************
gen nova=1 if Ano==2019 & empregado==1
replace nova=1 if Ano==2020 & empregado==0
replace nova=0 if nova==.

bysort ID: egen ED=total(nova==1)

replace ED=0 if ED<2

replace ED=1 if ED==2


****************Desempregado-Desemprego*****************
gen nova6=1 if Ano==2019 & empregado==0
replace nova6=1 if Ano==2020 & empregado==0
replace nova6=0 if nova6==.

bysort ID: egen DD=total(nova6==1)

replace DD=0 if DD<2

replace DD=1 if DD==2



**************Empregado -Empregado
bysort ID: egen EE=total(empregado==1)

replace EE=0 if EE<2

replace EE=1 if EE==2




**************Colocar quem continua desempregado como missing
replace ED=. if DD==1



**********Trabalho remoto*******
gen remoto=1 if C013==1
replace remoto=0 if C013==2

bysort ID: egen trab_remoto=total(remoto)

replace trab_remoto=. if DD==1



************Rendimento dos Desempregados********
***********Recebe Bolsa Família
gen BF=1 if D0031==1
replace BF=0 if D0031==2


***********Recebe Auxílio emergencial
gen auxilio=1 if D0051==1
replace auxilio=0 if D0051==2

************Recebe Seguro desemprego
gen seguro=1 if D0061==1
replace seguro=0 if D0061==2



******************Redução dos salários 

bysort ID: egen var=max(sal_hr)

gen reduc=1 if var==sal_hr & Ano==2019
replace reduc=0 if reduc==.


bysort ID: egen nov=total(reduc==1)

replace nov=. if ED==.
 

ren nov sal_reduc


global xvar homem branco id2 id3 id4 id5 fundamental medio superior
global familia chefe conjuge mae
global local urbana norte nordeste sul centro_oeste rm 
global trab trab_remoto trab2 carteira 


********************Probit com correção de seleção (balde, Boly e Avenyo, 2020)**********
heckprobit sal_reduc $xvar $familia $local $trab , sel(empregado=$xvar $familia tem_filho $local) vce(robust) nolog 
margins, dydx(*) pred(pmargin)


heckprobit ED $xvar $familia $local $trab , sel(empregado=$xvar $familia tem_filho $local) vce(robust) nolog 
margins, dydx(*) pred(pmargin)




***************************Descritiva**************
global xvar homem branco id1 id2 id3 id4 id5 sem_educ fundamental medio superior
global familia chefe conjuge tem_filho  
global local urbana norte nordeste sul centro_oeste sudeste rm 
global trab trab_remoto trab2 carteira 





***************************Pnad Covid**************
logout, save (descritivaRed1) word excel replace: sum $xvar $familia $local $trab if sal_reduc==1 & Ano==2020
logout, save (descritivaRed2) word excel replace: sum $xvar $familia $local $trab if sal_reduc==0 & Ano==2020


logout, save (descritivaDes1) word excel replace: sum $xvar $familia $local $trab if ED==1 & Ano==2020
logout, save (descritivaDes2) word excel replace: sum $xvar $familia $local $trab if ED==0 & Ano==2020


logout, save (descritivaTotal) word excel replace: sum $xvar $familia $local $trab sal_reduc ED if Ano==2020



*********************Correção IPCA 
gen wc=salario/1.005308473675 if Ano==2020
replace wc=salario if Ano==2019



********************Por nível educacional*********************
sum sal_hr if sem_educ==1 & Ano==2020
sum sal_hr if fundamental==1 & Ano==2020
sum sal_hr if medio==1 & Ano==2020
sum sal_hr if superior==1 & Ano==2020

sum sal_hr if sem_educ==1 & Ano==2019
sum sal_hr if fundamental==1 & Ano==2019
sum sal_hr if medio==1 & Ano==2019
sum sal_hr if superior==1 & Ano==2019


sum salario if sem_educ==1 & Ano==2020
sum salario if fundamental==1 & Ano==2020
sum salario if medio==1 & Ano==2020
sum salario if superior==1 & Ano==2020

sum salario if sem_educ==1 & Ano==2019
sum salario if fundamental==1 & Ano==2019
sum salario if medio==1 & Ano==2019
sum salario if superior==1 & Ano==2019


******************Salário corrigido
sum wc if sem_educ==1 & Ano==2020
sum wc if fundamental==1 & Ano==2020
sum wc if medio==1 & Ano==2020
sum wc if superior==1 & Ano==2020

sum wc if sem_educ==1 & Ano==2019
sum wc if fundamental==1 & Ano==2019
sum wc if medio==1 & Ano==2019
sum wc if superior==1 & Ano==2019




sum ED if sem_educ==1 & Ano==2020
sum ED if fundamental==1 & Ano==2020
sum ED if medio==1 & Ano==2020
sum ED if superior==1 & Ano==2020

sum ED if sem_educ==1 & Ano==2019
sum ED if fundamental==1 & Ano==2019
sum ED if medio==1 & Ano==2019
sum ED if superior==1 & Ano==2019




***************Dos que estão desempregados em 2020 - empregado==0

tab empregado BF
tab empregado auxilio 
tab empregado seguro 


tab BF if empregado==1
tab BF if empregado==0

tab auxilio if empregado==1
tab auxilio if empregado==0

tab seguro if empregado==1 
tab seguro if empregado==0





****************Dos que ficaram desempregados em 2020 - ED==1
tab ED BF
tab ED auxilio 
tab ED seguro 


tab BF if ED==1
tab BF if ED==0

tab auxilio if ED==1
tab auxilio if ED==0

tab seguro if ED==1 
tab seguro if ED==0



gen soma=BF+auxilio+seguro


       soma |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |     97,548       50.78       50.78
          1 |     73,382       38.20       88.97
          2 |     20,919       10.89       99.86
          3 |        267        0.14      100.00
------------+-----------------------------------
      Total |    192,116      100.00


