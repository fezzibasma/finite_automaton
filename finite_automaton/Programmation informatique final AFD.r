############################################################################
################	     Projet informatique 2014/2015 	  	################
################            AUTOMATE DETERMINISTE FINI   ################
############################################################################



##fonction pour lire le lire les lignes du fichier automate.txt :
lirefichier=function(fichier){
con = file(fichier, "r", blocking = FALSE)
N=length(readLines(fichier)) #to get number of lines in the txt
Lignes = strsplit(readLines(con,N), ";")
close(con)
return(Lignes)
}
###Test:
m_Lignes=lirefichier("Automate.txt")#renvoie � la description compl�te du fichier automate sous format .txt ligne par ligne.
###Nom des variables:
m_Alphabet = m_Lignes[2]#renvoie � l'ensemble alphabet. 
m_Etats = m_Lignes[3]#renvoie � l'ensemble etats(Q)
m_EtatInit = m_Lignes[4]#renvoie � l'etat initial
m_EtatTerm = m_Lignes[5]#renvoie � l'etat terminal ou l'etat gagnant
m_Trans = m_Lignes[6:length(m_Lignes)]#renvoie � la matrice de Transition
m_q0=unlist(m_EtatInit)
m_qf=unlist(m_EtatTerm)#pour mettre l'etat terminal sous forme d'un vecteur
m_Q=unlist(m_Etats)#pour rendre l'ensemble etats(Q)sous forme d'un vecteur
m_sigma=unlist(m_Alphabet)#pour rendre l'ensemble alphabet (sigma) sous forme d'un vecteur


##############################################################
#1)le mot contient il des lettres qui sont pas dans l'alphabet?
##Description: Cette fonction teste si chacune des lettres du mot appartient � l'alphabet
## valeur de retour: True si les lettres appartiennent toute � l'alphabet et False si au moins une lettre n'appartient pas � l'alphabet.

testerLettre_Alphabet=function(mot){

  longueur_mot=nchar(mot) ##longueur de la cha�ne de caract�res

  OK=TRUE

if (all(is.element(substring(mot,1:longueur_mot,1:longueur_mot),m_sigma))==FALSE){ ##la fonction all(v) : renvoie TRUE si toutes les valeurs sont TRUE. all(TRUE, TRUE, FALSE) donne FALSE.
				print("Au moins une lettre n'appartient pas � l'Alphabet.")
      OK=FALSE
                                                       }
return(OK)}

##############################################################
#2)L'�tat initial et les �tats terminaux sont-ils coh�rents avec Etats ?
testerEtats = function(q0,qF,Q){

  OK=TRUE

  if (is.element(q0,Q) == FALSE){
    print("L'�tat initial n'appartient pas � l'ensemble des �tats.")
  OK = FALSE                             }



  if (is.element(qF,Q) == FALSE){
    print("Au moins un des �tats terminaux n'appartient pas � l'ensemble des �tats.")
    OK = FALSE
                                        }

return(OK)}
#######################################################################################
#3)Les Transitions sont-elles biens g�r�es?
##Description:Cette fonction teste si les combinaisons obtenue � partir de vecteurs d'etats et d'alpha appartiennent � la matrice de Transition.


testerTransitions=function(Q,sigma,Trans)
{  
if(length(Trans)==length(Q)*length(sigma)) ##si cette condition est v�rifi�e,on v�rifie si pour tout etat q de l'ensemble Q et lettre de l'ensemble alphabet (sigma),il existe au plus une transition partant de q et portant l'�tiquette lettre.
{                                                ##sinon,il sort de la boucle.
 for(i in 1:length(Q))
 {
   for(j in 1 :length(sigma))
	{	 OK=FALSE  ##R�nitialisation de la variable OK par False � chaque it�ration de boucle.
		for(k in 1: length(Trans))
         {
			if(Trans[[k]][1]==Q[i] && Trans[[k]][2]== sigma[j])##test egalit� entre une combinaison (etats[i],sigma[j]) avec les 2 premi�res colonnes de la matrice de Transition.
			{			
			 OK=TRUE  
			}
           
         }
			if(OK==FALSE)##Test si OK a une valeur T ou F ,on retourne OK qui contient F.
			{		print("les transitions ne sont pas biens g�r�es")
					return (OK)
			}
			
	}	
 
}
}else{
print("les transitions ne sont pas biens g�r�es")
					OK=FALSE

}


 return (OK) ## OK va retouner TRUE ou False
}
			
#######################################################################################
#4)effectuer Transition
EffectuerTransition=function(Trans,etat,lettre){
for(i in 1:length(Trans))
{
	 if( Trans[[i]][1]== etat && Trans[[i]][2]==lettre)
		{
            return (Trans[[i]][3])
	    }
}
return (0) ##Si la lettre n'appartient pas � l'alphabet,il va retourner 0 au lieu NULL
}

######################################################################################
#machine                              
machine=function(mot){

flag1=FALSE
flag2=FALSE
flag3=FALSE
        if (testerLettre_Alphabet(mot)==TRUE){
                                        print ("Toutes les lettres du mot appartiennent � l'Alphabet")
										    flag3=TRUE}
										        
		if (testerEtats(m_q0,m_qf,m_Q)==TRUE){
					print("L'�tat initial et les �tats terminaux sont coh�rents avec Etats.")
										flag1=TRUE
                                                 }
		if (testerTransitions(m_Q,m_sigma,m_Trans)==TRUE){
					print("Les Transitions sont biens g�r�es.")
										flag2=TRUE
					                      }
if(flag1==TRUE && flag2==TRUE ){

if(flag3==TRUE){
etat=m_q0 ####automate est valide,le programme s'ex�cute 
for(i in 1:nchar(mot)){
		nouvetat=EffectuerTransition(m_Trans,etat,substring(mot,i,i))
					etat=nouvetat
					  }
    if(is.element(nouvetat,m_qf)==TRUE){
            print("Le mot est accept� par la machine")
		}
    else{
		    print("Le mot n'est pas accept� par la machine")
		}
		}else
		{
		print("L'automate est valide mais la machine est interrompue car au moins une lettre n'appartient pas � l'Alphabet")
		}		
}
else {print("L'Automate est non valid�e et donc ne peut pas �tre ex�cut�")}
}
#####Test 
machine("abaaa")