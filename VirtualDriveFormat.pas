unit VirtualDrive;

interface

//
// Drive Info
// MBR dump Info
// Disk fragment info
// Partition fragment info.
// Creer un bloc séparé pour les infos SMART

TVirtualDriveHeader = packed record of
	dhSign:		Cardinal;		// Signature d'un fichier Virtual Drive
    dhSize:		Cardinal;		// Taille de la structure
    dhVersion:	Cardinal;		// version du fichier
    dhFlags:	Cardinal;
    // ------------------ Infos sur la géométrie du disque -----------------
	dhCylinders:	Cardinal;
	dhHeads:		Cardinal;
	dhSectperTrack:	Cardinal;
    dhSectorSize:	Cardinal;	// Taille des secteurs
    dhSectorCount:	Cardinal;	// Nombre de secteur du disque
    // ----------------------- Infos SMART ------------------------------
    dhModelName: array[0..9] of char;
    dhSerialNumber: array[0..9] of char;
    dhFirmwareRev: array[0..9] of char;
    // ------------------ Infos sur les fichiers fragments -----------------
    // Mettre dans une autre structure ????
    dhFragmentSize:	Cardinal;	// taille des fragments en secteurs
    dhFirstSector:	Cardinal;	// numéro du premier secteur de l'image (normalement 0)
    dhLastSector:	Cardinal;	//
end;


// Mettre un flag contenant le type d'info du fichier : MBR dump, partition dump,
// disk dump
// Taille = somme des MBR, EMBR, BS
//
// Info sur la table des MBR/EMBR et BootSectors
// Regles:
// Le premier secteur est la MBR
// L'offset entre un BS et la MBR dépend de la chaine des partitions précédentes
// Le BS suit obligatoirement le EMBR concerné
//

// partition fragment info : lba, nb sector, partentry

TVirtualDriveBadSector = packed record of
	dhSign:		Cardinal;		// Signature de la structure
    dhSize:		Cardinal;		// Taille de la structure (dépend du nombre de secteur défextueux)
    dhFlag:		Cardinal;		// n° secteur triés, ...
    dhBadSectCount:	Cardinal;	// Nombre de secteurs defectueux
    // ---------------------------
    // Les secteurs défectueux sont stockes ici dans un tableau (1 Cardinal par
    // secteur)
end;



TFragmentHeader = packed record of
	fhSign:		Cardinal;	  	// Signature d'un fichier fragment
    fhSize:		Cardinal;	  	// Taille de la structure octet
    fhFlags:	Cardinal;
    fhFirst:	Cardinal;		// 1er secteur que contient le fragment
    fhFragmentSize:	Cardinal;	// taille du fragments en secteurs
end;


Const
VIRTUAL_DRIVE_FILE_HEADER	= $00000000;
BAD_SECTOR_RECORD_HEADER	= $11111111;
FRAGMENT_FILE_HEADER		= $22222222;

// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation



// --------------------------- Fin de l'unité -----------------------------
end.
