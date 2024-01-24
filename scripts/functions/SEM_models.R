###########
# - Define models
# - Model syntax
#   ~ implies direct relationship
#   ~~ is covariance
#   := corefficients
##########

#############
# -- MODEL Structure1

##############

' 
#Stability_full_model

stability ~ meanTair + meanWAI1 + a*comb_Fdis_all + cvPC2 + b*LAI + c*H_cwm + ShannonH.y + SpeciesRichness.x

#Regression
comb_Fdis_all ~ meanTair + meanWAI1 
LAI ~ meanTair + meanWAI1
H_cwm ~ meanTair + meanWAI1

'->Model_full


' 
#Stability_full_model_MI

stability ~ meanTair + meanWAI1 + a*comb_Fdis_all + cvPC2 + b*LAI + c*H_cwm + ShannonH.y + SpeciesRichness.x

#Regression
comb_Fdis_all ~ meanTair + meanWAI1 + SpeciesRichness.x + ShannonH.y
LAI ~ meanTair + meanWAI1
H_cwm ~ meanTair + meanWAI1

LAI ~~ H_cwm

'->Model_full2


' 
#Model_best_vars_fdiv

stability ~ meanTair + meanWAI1 + a*comb_Fdis_all + cvPC2 + b*LAI + c*H_cwm + d*comb_SLA_cwd

#Regression
comb_Fdis_all ~ meanTair + meanWAI1 + cvPC2 
comb_SLA_cwd ~ meanTair + meanWAI1 + cvPC2
LAI ~ meanTair + meanWAI1 + cvPC2
H_cwm ~ meanTair + meanWAI1 + cvPC2
LAI ~~ H_cwm


'->Model_bestvars_fdiv

' 
#Model_bestvars_fdiv_stability

comb_Fdis_all ~ meanTair + meanWAI1 + a*stability + cvPC2 + b*LAI + c*H_cwm + d*comb_SLA_cwd

#Regression
stability ~ meanTair + meanWAI1 + cvPC2 + comb_SLA_cwd + LAI
comb_SLA_cwd ~ meanTair + meanWAI1 + cvPC2 + LAI
LAI ~ meanTair + meanWAI1 + cvPC2
H_cwm ~ meanTair + meanWAI1 + cvPC2

LAI ~~ H_cwm

'->Model_bestvars_fdiv_stability

' 
#Model_best_vars_SR

stability ~ meanTair + meanWAI1 + a*SpeciesRichness.x + cvPC2 + b*LAI + c*H_cwm + d*comb_SLA_cwd

#Regression
SpeciesRichness.x ~ meanTair + meanWAI1 + cvPC2 
comb_SLA_cwd ~ meanTair + meanWAI1 + cvPC2
LAI ~ meanTair + meanWAI1 + cvPC2
H_cwm ~ meanTair + meanWAI1 + cvPC2
LAI ~~ H_cwm


'->Model_bestvars_SR

' 
#Model_bestvars_Shannon_stability

ShannonH.y ~ meanTair + meanWAI1 + a*stability + cvPC2 + b*LAI + c*H_cwm + d*comb_SLA_cwd

#Regression
stability ~ meanTair + meanWAI1 + cvPC2 + ShannonH.y + LAI
comb_SLA_cwd ~ meanTair + meanWAI1 + cvPC2 + LAI
LAI ~ meanTair + meanWAI1 + cvPC2
H_cwm ~ meanTair + meanWAI1 + cvPC2

LAI ~~ H_cwm

'->Model_bestvars_Shannon_stability


' 
#Model_best_vars_Shannon

stability ~ meanTair + meanWAI1 + a*ShannonH.y + cvPC2 + b*LAI + c*H_cwm + d*comb_SLA_cwd

#Regression
ShannonH.y ~ meanTair + meanWAI1 + cvPC2 
comb_SLA_cwd ~ meanTair + meanWAI1 + cvPC2
LAI ~ meanTair + meanWAI1 + cvPC2
H_cwm ~ meanTair + meanWAI1 + cvPC2
LAI ~~ H_cwm


'->Model_bestvars_Shannon


' 
#Small_model1

stability ~  a*comb_Fdis_all + cvPC2 + comb_SLA_cwd + meanWAI1

comb_Fdis_all ~ meanWAI1 + stability

'->Small_model1



# -- MODEL 2
' 
#PC1_model_struct_only2

PC1 ~ agb_100m_sept_2018_zp955x5  + max_hc + Nmass + LAImax

agb_100m_sept_2018_zp955x5 ~~ max_hc
LAImax ~~ agb_100m_sept_2018_zp955x5 
LAImax ~~ max_hc 

'-> PC1_model_struct_only2

' 
#PC1_model_climate_only1
PC1 ~ VPD_JN + P_JN + TA_JN + SW_IN_JN + CSWI
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN
VPD_JN ~~  P_JN
CSWI ~~  P_JN

'-> PC1_model_climate_only1

' 
#PC1_model_climate_only2
PC1 ~ VPD_JN + TA_JN + SW_IN_JN + CSWI
CSWI ~ P_JN
CSWI  ~   VPD_JN
CSWI  ~   TA_JN
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN
VPD_JN ~~  P_JN

'-> PC1_model_climate_only2


' 
#PC1_model_complete

agb_100m_sept_2018_zp955x5 ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
max_hc ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
LAImax ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
Nmass ~ VPD_JN + P_JN + TA_JN + SW_IN_JN

max_hc ~~ LAImax + agb_100m_sept_2018_zp955x5
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN

PC1 ~ agb_100m_sept_2018_zp955x5 + max_hc + LAImax + Nmass


#max_hc ~~ agb_100m_sept_2018_zp955x5
#agb_100m_sept_2018_zp955x5 ~~ LAImax
#max_hc ~~ LAImax

'-> PC1_model_complete

#############
# -- MODEL PC2
##############
' 
#PC2_model_struct_only

PC2 ~ agb_100m_sept_2018_zp955x5  + b1*LAImax + Nmass
LAImax ~ a1*max_hc 

agb_100m_sept_2018_zp955x5 ~~ max_hc
LAImax ~~ agb_100m_sept_2018_zp955x5 

i_1 := a1*b1


'-> PC2_model_struct_only

# -- MODEL 2
' 
#PC2_model_struct_only2

PC2 ~ agb_100m_sept_2018_zp955x5  + max_hc + Nmass + LAImax

agb_100m_sept_2018_zp955x5 ~~ max_hc
LAImax ~~ agb_100m_sept_2018_zp955x5 
LAImax ~~ max_hc 

'-> PC2_model_struct_only2

' 
#PC2_model_climate_only1
PC2 ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN
VPD_JN ~~  P_JN

'-> PC2_model_climate_only1

' 
#PC2_model_complete

agb_100m_sept_2018_zp955x5 ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
max_hc ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
LAImax ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
Nmass ~ VPD_JN + P_JN + TA_JN + SW_IN_JN

max_hc ~~ LAImax + agb_100m_sept_2018_zp955x5
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN

PC2 ~ agb_100m_sept_2018_zp955x5 + max_hc + LAImax + Nmass

#max_hc ~~ agb_100m_sept_2018_zp955x5
#agb_100m_sept_2018_zp955x5 ~~ LAImax
#max_hc ~~ LAImax

'-> PC2_model_complete

' 
#PC2_model_latent_strclimNout

struct =~ agb_100m_sept_2018_zp955x5  + LAImax + max_hc 
climate =~  VPD_JN + TA_JN + SW_IN_JN + CSWI

#Regression
PC2 ~ a*struct 
PC2 ~ b*Nmass 
PC2 ~ c*climate
struct ~ d*climate
Nmass ~ e*climate

#indirect effect
ad := a*d
eb := e*b
#total effect
total :=c+(a*d)+(e*b)
'->PC2_model_latent_strclimNout

' 
#PC2_model_combTVPDHeight

#struct =~ agb_100m_sept_2018_zp955x5  + LAImax + max_hc 
#climate =~  VPD_JN + TA_JN + SW_IN_JN + CSWI

#Regression
PC2 ~ a*max_hc 
PC2 ~ b*VPD_JN 
PC2 ~ c*TA_JN
PC2 ~ d*CSWI + P_JN

max_hc ~ e*VPD_JN + f*TA_JN + g*SW_IN_JN + h*CSWI

#indirect effect
ae := a*e
ef := a*f
ag := a*g
eh := a*h

#total effect
totalTA :=c+(a*f)

'->PC2_model_combTVPDHeight


' 
#PC2_model_combTVPDHeightLAI

#struct =~ agb_100m_sept_2018_zp955x5  + LAImax + max_hc 
#climate =~  VPD_JN + TA_JN + SW_IN_JN + CSWI

#Regression
PC2 ~ a*max_hc + LAImax
PC2 ~ b*VPD_JN 
PC2 ~ c*TA_JN
PC2 ~ d*CSWI + P_JN

max_hc ~ e*VPD_JN + f*TA_JN + g*SW_IN_JN + h*CSWI

#indirect effect
ae := a*e
ef := a*f
ag := a*g
eh := a*h

#total effect
totalTA :=c+(a*f)

'->PC2_model_combTVPDHeightLAI

' 
#PC2_model_combTVPDHeightAGB

#struct =~ agb_100m_sept_2018_zp955x5  + LAImax + max_hc 
#climate =~  VPD_JN + TA_JN + SW_IN_JN + CSWI

#Regression
PC2 ~ a*max_hc + agb_100m_sept_2018_zp955x5
PC2 ~ b*VPD_JN 
PC2 ~ c*TA_JN
PC2 ~ d*CSWI + P_JN

max_hc ~ e*VPD_JN + f*TA_JN + g*SW_IN_JN + h*CSWI

#indirect effect
ae := a*e
ef := a*f
ag := a*g
eh := a*h

#total effect
totalTA :=c+(a*f)

'->PC2_model_combTVPDHeightAGB
#############
# -- MODEL PC3
##############

' 
#PC3_model_latent_strclimNout

struct =~ agb_100m_sept_2018_zp955x5  + LAImax + max_hc 
climate =~  VPD_JN + TA_JN + SW_IN_JN + CSWI

#Regression
PC3 ~ a*struct 
PC3 ~ b*Nmass 
PC3 ~ c*climate
struct ~ d*climate
Nmass ~ e*climate

#indirect effect
ad := a*d
eb := e*b

#total effect
total :=c+(a*d)+(e*b)
'->PC3_model_latent_strclimNout

' 
#PC3_model_combClim_AGB

#struct =~ agb_100m_sept_2018_zp955x5  + LAImax + max_hc 
#climate =~  VPD_JN + TA_JN + SW_IN_JN + CSWI

#Regression
PC3 ~ a*agb_100m_sept_2018_zp955x5 + Nmass + b*VPD_JN + c*TA_JN + d*CSWI
agb_100m_sept_2018_zp955x5 ~ e*VPD_JN + f*TA_JN + g*CSWI

#indirect effect
ae := a*e
af := a*f
ag := a*g

#total effect
totalTA :=c+(a*f)
totalVPD :=b+(a*e)
totalCSWI :=d+(a*g)

'->PC3_model_combClim_AGB

' 
#PC3_model_combClim_AGBAge

#Regression
PC3 ~ a*agb_100m_sept_2018_zp955x5 + i*lnAge + Nmass + b*VPD_JN + c*TA_JN + d*CSWI
agb_100m_sept_2018_zp955x5 ~ e*VPD_JN + f*TA_JN + g*CSWI
agb_100m_sept_2018_zp955x5 ~ h*lnAge

#indirect effect
ae := a*e
af := a*f
ag := a*g
ah := a*h

#total effect
totalTA :=c+(a*f)
totalVPD :=b+(a*e)
totalCSWI :=d+(a*g)
totalAge :=i+(a*h)

'->PC3_model_combClim_AGBAge

' 
#PC3_model_struct_only

PC3 ~ agb_100m_sept_2018_zp955x5  + b1*LAImax + Nmass
LAImax ~ a1*max_hc 

agb_100m_sept_2018_zp955x5 ~~ max_hc
LAImax ~~ agb_100m_sept_2018_zp955x5 

#indirect effect
ab := a1*b1

'->PC3_model_struct_only

# -- MODEL 2
' 
#PC3_model_struct_only2

PC3 ~ agb_100m_sept_2018_zp955x5  + max_hc + Nmass + LAImax

agb_100m_sept_2018_zp955x5 ~~ max_hc
LAImax ~~ agb_100m_sept_2018_zp955x5 
LAImax ~~ max_hc 

'-> PC3_model_struct_only2

' 
#PC3_model_climate_only1
PC3 ~ VPD_JN + P_JN + TA_JN + SW_IN_JN + CSWI
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN
VPD_JN ~~  P_JN
CSWI ~~  P_JN

'-> PC3_model_climate_only1

' 
#PC3_model_climate_only2
PC3 ~ VPD_JN + TA_JN + SW_IN_JN + CSWI
CSWI ~ P_JN
CSWI  ~   VPD_JN
CSWI  ~   TA_JN
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN
VPD_JN ~~  P_JN

'-> PC3_model_climate_only2


' 
#PC3_model_complete

agb_100m_sept_2018_zp955x5 ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
max_hc ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
LAImax ~ VPD_JN + P_JN + TA_JN + SW_IN_JN
Nmass ~ VPD_JN + P_JN + TA_JN + SW_IN_JN

max_hc ~~ LAImax + agb_100m_sept_2018_zp955x5
VPD_JN ~~ TA_JN
SW_IN_JN ~~ VPD_JN
SW_IN_JN ~~ TA_JN
TA_JN ~~ P_JN

PC3 ~ agb_100m_sept_2018_zp955x5 + max_hc + LAImax + Nmass


#max_hc ~~ agb_100m_sept_2018_zp955x5
#agb_100m_sept_2018_zp955x5 ~~ LAImax
#max_hc ~~ LAImax

'-> PC3_model_complete

