// PASSER DE 64x64 a 1x4096 (transformation d'une image 64*64 en ligne 1*4096)
function X=ligne (A)
    X=A';
    X=X(:);
    X=X';
endfunction

// construction de la matrice 100*4096
function X=construc() 
    for i=1:100
        A=imread("h:\Desktop\BDD1\FG1\visage ('+string(i)+').pgm");
        A=double(A); // la commande qui nous permet de faire fonctionner le code
        // car il y'a un problème d'octets, nous permet de tranformer la matrice d'entier en 
        // matrice de double
        X(i,:)=ligne(A)
    end
endfunction
 

// moyenne et ecart-type de chaque colonne de la matrice 100*4096
function M=meanAndStdev(X) 
    for j=1:4096
        M(1,j)=mean(X(:,j))
        M(2,j)=stdev(X(:,j))
    end
endfunction

// Y correspond  à la normalisation de la matrice X (100*4096)
function  Y=normalisation(X,M) 
   // X=construc()
    //M=meanAndStdev(X)
    for j=1:4096
        for i=1:100
            Y(i,j)=(X(i,j)-M(1,j))/M(2,j)
        end
    end
endfunction

 

 

 

// décomposition en valeur singulière de la matrice Y  pour obbtenir diagonalisation de la matrice C
function [P,D]= valeurSingu(Y) 
    [U,S,V]=svd(Y)
    D=S'*S // matrice diagonale (D=S'*S)  à l'aide de la démontration faite dans la partie 3
    P=V // matrice orthogonale(P=V) à l'aide de la démontration faite dans la partie 3
    //utilisation de la commande stacksize("max")car problème de mémoire
endfunction

 

function i=reductionDim(D)// fonction de la réduction des dimensions 
// renvoie le nombre de dimensions à garder pour un gain d'information de 95%
    printf("%d",trace(D))
    dim=0 // nouvelle dimension réduite
    i=0
   // on veut que 95% de l'information soit conserver après la réduction de dimension 
   while dim<0.95  
       i=i+1;
       dim= trace(D(1:i,1:i)) / trace(D); 
    end
    printf('Dimension = %f',dim);
    
  //////////////////// Test avec 80%/////////////////////////
    //P=P(:,1:i)
       // for l=1:4096
       //     for j=1:4096
         //       if l==j then
           //         s = (s + M(l,l))/trace(M) // calcul la trace pour chaque sous matrice carré
               //     i=i+1
              //      printf("s = %d", s)
             //   end

 

  //              if s>0.80 then
    //                break;
      //          end                
        //    end
        //end
    //////////////////////////////////////////////////////////
endfunction

 

// Apprentissage (formation de la nouvelle matrice Z normalisée de la base d'apprentissage dans la nouvelle base Pk = réduction de dimension de la base)
function [Pk,Z,M]=apprentissage(X)
    stacksize("max");
   // X=construc();
    M=meanAndStdev(X);
    Y=normalisation(X,M);
    [P,D]=valeurSingu(Y);
    k=reductionDim(D)
    Pk= P(:,1:k);// k nb de composante retenu
    Z=Y*Pk;// Coordonnées des individus
endfunction

 


// Ajout, ajoute une image Zp dans la base Pk à la matrice Z à l'aide du numéro image = position de l'image dans sa base de donnée 
function [Z,Zp]=ajout(M,Pk,Z,img)
    // pour nos test, nous avons préféré passer en paramètre le numéro de l'image plutot que le chemin complet
    A=imread("h:\Desktop\BDD1\FG1\visage ("+string(img)+").pgm");
    A=double(A);
    Xp=ligne(A);
    
    for j=1:4096
           Yp(1,j)=(Xp(1,j)-M(1,j))/M(2,j)
    end
    
     Zp=Yp*Pk;
        // coordonnées de la nouvelle image = nouvelle ligne de Z 
     Z=[Z;Zp]; // ajout a a la fin de Z (ne fonctionne pas)
   
endfunction

 

function Z=copieBDD(M,Pk,Z)//fonctionelle copie la BDD 
    // exprimer les coordonnées de toutes les images dans la nouvelle base 
    for i=101:1680
        [Z,Zp]=ajout(M,Pk,Z,i)
    end
endfunction
    
////////////////////////////////////
// RECONNAISSANCE FACIALE
////////////////////////////////////
function cooImg=reconnaissance_faciale(M,Pk,Z,img)
    
    //////////////////////////NORMALISATION & EXPRIMER IMAGE DANS NOUVELLE BASE Pk/////////////
    A=imread("h:\Desktop\BDD1\FG1\visage ("+string(img)+").pgm");
   // A=imread("h:\Desktop\BDD2\FG2\Adam_Sandler_0001.pgm");
    A=double(A); // transformation de la matrice d'entier en double pour eviter un buegue au niveau des octets
     Xp=ligne(A);
     
     for j=1:4096 // normalisation de l'image ajouté
            Yp(1,j)=(Xp(1,j)-M(1,j))/M(2,j)
     end
    
     Zp=Yp*Pk; // expression de l'image dans la base réduite Pk
   ///////////////////////////////////////////////////////////////////////////////////////////////
   
   
    [r,c]=size(Z) // RECUPERATION DE LA TAILLE DES LIGNES DE LA BASE D'APPRENTISSAGE
    
    for i=1:r // calcul des différentes normes de l'image Zp avec les images de la base d'apprentissage Z
        Zi=Z(i,:);
        d(1,i)= norm(Zp-Zi,2); 
    end
    
    [minimum,position] = min(d)// repérage de la norme minimale 
    
        
    if minimum < 29  then // seuil en fonction de l'img ADAM SANDLER personnalité que l'on connait
        cooImg = position            
    else
       cooImg = -1
    end
     if cooImg == -1 then
        disp("Aucune image n a été reconnue")
    else
 
       imshow(A) // image en entrée
      imageA=imread("h:\Desktop\BDD1\FG1\visage ("+string(cooImg)+").pgm")
       imshow(imageA)// image de la base d'apprentissage
    end
 
endfunction
