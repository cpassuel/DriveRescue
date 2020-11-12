{-----------------------------------------------------------------------------
 Unit Name: VirtualDrive
 Author:    Chris
 Purpose:
 History:
-----------------------------------------------------------------------------}


unit VirtualDrive;

interface

{type

TVirtualDrive = class(TGenericDrive)
private
	FFileName: string;
protected

public
  constructor Create; override;
  destructor Destroy; override;
published

end;
}

implementation

{
          
Lecture d'un secteur:

	Si Secteur dans cache alors
    	Renvoyer le secteur à partir du cache
    Sinon
    	Déterminer le fichier concerné
        Si fichier ouvert alors
        	Lire le fichier
            Mettre fichier comme dernier utilisé
            Mettre le secteur dans le cache
        Sinon
        	Si pool fichier ouvert complet alors
            	Fermer le fichier le plus vieux / Retirer du pool
                Ouvrir le fichier
                Lire le fichier
                Mettre fichier comme dernier utilisé
	            Mettre le secteur dans le cache
            sinon
            	Ajouter le fichier au pool
                Ouvrir le fichier
                Lire le fichier
                Mettre fichier comme dernier utilisé
	            Mettre le secteur dans le cache
            Finsi
        Finsi
    Finsi

}

// Comment gérer le LRU ?
// Pool de fichiers : Tlist
// Dernier fichier utilisé change quand un fichier est retirer du pool
// Premier fichier utilisé change quand on ajoute un fichier au pool et lors de lecture
end.
