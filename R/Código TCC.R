#TCC Jônatas Preussler

#'Função para dimensionar perfis de aço laminado (flexão e cortante)
#'
#'Essa função serve para dimensionar perfis de aço laminado sujeitos a momentos fletores e esforços cortantes
#'
#'@param Msd_Solicitante Momento fletor solicitante de cálculo (em N.mm)
#'@param Vsd_Solicitante Força cortante solicitante de cálculo (em N)
#'@param Lb Comprimento destravado da estrutura (em mm)
#'@param Cb Fator de modificação para diagrama de momento fletor não-uniforme (caso não haja, adotar igual a 1.0)
#'
#'@examples
#'Esc_perf(231240000,116400,3200,1)
#'
#'@export
Esc_perf <- function (Msd_Solicitante, Vsd_Solicitante, Lb, Cb){

library(readxl)
WLamin <- read_excel("Perfis Laminados.xlsx");

################################################################################
#Especificações do tipo de aço (ASTM A572-G50):

#Módulo de elasticidade longitudinal (GPa):
E <-  200000
#Módulo de elasticidade transversal (GPa):
G <- 77
#Resistência do aço ao escoamento (MPa):
fy <- 345
#Resistência do aço a ruptura (MPa):
fu <- 450
################################################################################
capivara = 0

Mrd_Final =  1

Vrd = 1

while (Mrd_Final<Msd_Solicitante||Vrd<Vsd_Solicitante) {

  capivara = capivara + 1
  ARROZ = capivara


  W <-  WLamin$VarTeste == ARROZ

  Ag <- WLam[W,"A"]*1E2;
  d <- WLam[W,"d"];
  h <- WLam[W,"h"];
  dd <- WLam[W,"dd"];
  tw <- WLam[W,"tw"];
  bf <- WLam[W,"bf"];
  tf <- WLam[W,"tf"];
  Ix <- WLam[W,"Ix"]*1E4;
  Iy <- WLam[W,"Iy"]*1E4;
  rx <- WLam[W,"rx"]*1E1;
  ry <- WLam[W,"ry"]*1E1;
  It <- WLam[W,"It"]*1E4;
  Cw <- WLam[W,"Cw"]*1E6;
  Wx <- WLam[W,"Wx"]*1E3;
  J <- WLam[W,"It"]*1E4;
  zx <- WLam[W,"Zx"]*1E3;
  ################################################################################
  #Cálculo da força resistente de cálculo (Vrd):

  kv <- 5

  Ya1 <- 1.1

  Vpl = 0.6*h*tw*fy;

  lambda = h/tw;

  lambdaP = 1.10*sqrt(kv*E/fy);

  lambdaR = 1.37*sqrt(kv*E/fy);

  if(lambda<=lambdaP){
    Vrd = Vpl/Ya1;
  }else if(lambda>lambdaP&&lambda<=lambdaR){
    Vrd = (lambdaP/lambda)*(Vpl/Ya1);
  }else{
    Vrd = (lambdaP/lambda)*(Vpl/Ya1);
  }
  ################################################################################
  #Cálculo da flambagem lateral por torção (FLT):

  lambda_FLT = Lb/ry;

  lambdaP_FLT = 1.76*sqrt(E/fy);

  Beta1 = (fy-0.3*fy)*Wx/(E*J);

  lambdaR_FLT = (1.38*sqrt(Iy*J)/(ry*J*Beta1))*sqrt(1+sqrt(1+27*Cw*Beta1^2/Iy));

  Mpl = zx*fy

  Mr <-  (fy-0.3*fy)*Wx

  Mcr <-  Cb*pi^2*E*Iy/Lb^2*sqrt(Cw/Iy*(1+0.039*J*Lb^2/Cw));

  if(lambda_FLT<=lambdaP_FLT){
    Mrd_FLT = Mpl/Ya1;
  }else if(lambda_FLT>lambdaP_FLT&&lambda_FLT<=lambdaR_FLT){
    if(((Cb/Ya1)*(Mpl-((Mpl-Mr)*((lambda_FLT-lambdaP_FLT)/(lambdaR_FLT-lambdaP_FLT)))))<=Mpl/Ya1){
      Mrd_FLT = (Cb/Ya1)*(Mpl-((Mpl-Mr)*((lambda_FLT-lambdaP_FLT)/(lambdaR_FLT-lambdaP_FLT))))
    }else{
      Mrd_FLT = Mpl/Ya1;
    }
  }else{
    if(Mcr/Ya1<=Mpl/Ya1){
      Mrd_FLT = Mcr/Ya1;
    }else{
      Mrd_FLT = Mpl/Ya1;
    }
  }
  ################################################################################
  #Cálculo da flambagem local da mesa comprimida (FLM):

  lambda_FLM = bf/(2*tf);

  lambdaP_FLM = 0.38*sqrt(E/fy);

  lambdaR_FLM = 0.83*sqrt(E/(fy-0.3*fy));

  Mcr_FLM = 0.69*E*Wx/(lambda_FLM^2)

  if(lambda_FLM<=lambdaP_FLM){
    Mrd_FLM = Mpl/Ya1;
  }else if(lambda_FLM>lambdaP_FLM&&lambda_FLM<=lambdaR_FLM){
    Mrd_FLM = 1/Ya1*(Mpl-((Mpl-Mr)*((lambda_FLM-lambdaP_FLM)/(lambdaR_FLM-lambdaP_FLM))));
  }else{
    Mrd_FLM = Mcr_FLM/Ya1;
  }
  ################################################################################
  #Cálculo da flambagem local da alma (FLA):

  lambda_FLA = dd/tw;

  lambdaP_FLA = 3.76*sqrt(E/fy);

  lambdaR_FLA = 5.70*sqrt(E/fy);

  if(lambda_FLA<=lambdaP_FLA){
    Mrd_FLA = Mpl/Ya1;
  }else if(lambda_FLA>lambdaP_FLA&&lambda_FLA<=lambdaR_FLA){
    Mrd_FLA = 1/Ya1*(Mpl-((Mpl-Mr)*((lambda_FLA-lambdaP_FLA)/(lambdaR_FLA-lambdaP_FLA))));
  }else{
    print("erro")
  }
  ################################################################################
  #Momento fletor resistente de cálculo:


  if(Mrd_FLT<=Mrd_FLM&&Mrd_FLT<=Mrd_FLA){
    Mrd_Final = Mrd_FLT;
  }else if(Mrd_FLM<=Mrd_FLT&&Mrd_FLT<=Mrd_FLA){
    Mrd_Final = Mrd_FLM;
  }else{
    Mrd_Final = Mrd_FLA;
  }
  ################################################################################


}

Perfil <- WLam[W,"Perfil"]


Perfil <- WLam[W,"Perfil"]

cat("\n\n")
cat(paste('O perfil mais adequado é:',Perfil))
cat("\n\n")
cat(paste("A força cortante resistente de cálculo é:", round(Vrd/1000,2),"kN"))
cat("\n\n")
cat(paste("O momento Resistente de cálculo para Flambagem Lateral com Torção (FLT) é:", round(Mrd_FLT/1000000,2),"kNm"))
cat("\n\n")
cat(paste("O momento Resistente de cálculo para Flambagem Local da Mesa Comprimida (FLM) é:", round(Mrd_FLM/1000000,2),"kNm"))
cat("\n\n")
cat(paste("O momento Resistente de cálculo para Flambagem Local da Alma (FLA) é:", round(Mrd_FLA/1000000,2),"kNm"))
cat("\n\n")
cat(paste("A taxa de utilização do perfil à força cortante é:", round(Vsd_Solicitante/Vrd,2)))
cat("\n\n")
cat(paste("A taxa de utilização do perfil ao momento fletor é:", round(Msd_Solicitante/Mrd_Final,2)))

}
