{-----------------------------------------------------------------------------
 Unit Name: ConversionUnit
 Author:    Chris
 Purpose:	Contient des fonctions de conversion de taille de fichiers en chaines
 Date:		16/07/2016
 History:
 16/07/2016 : Added DateTimeToFileTime
 15/07/2016 : Added Big Endian <=> Little Endian conversion
 12/05/2005
-----------------------------------------------------------------------------}
unit ConversionUnit;


interface

Uses
	Windows;


//
// fonctions de conversion export�es
//

// ----------------- Conversion de taille ----------------------
function SizeTo2DigitByteString(const value : int64): string;
function SizeToRoundedByteString(const value : int64): string;
function SizeToRoundedKiloString(const size : int64) : string;

{
Entr�e : taille en octet
Unit�: Kio ou Ko
Pr�cision
3DigitCeil
3DigitFloor
3DigitInt
6DigitFloor
6digitceil
3digit2

unit� la plus proche
arrondi unit� sup�rieur
}

// ----------------- Conversion int64 ----------------------
function Int64ToThousandSepString(const valeur: int64): string;

// ----------------- Conversion de FileTime ----------------------
function FileTimeToDateTime(const ftime: TFileTime): TDateTime;
function FileTimeToString(const ftime: TFileTime): string;
procedure FileTimeToDateAndTimeString(const ftime: TFileTime; var datestr, timestr: string);
function DateTimeToFileTime(const FileTime: TDateTime): TFileTime;

// ----------------- Conversion BigEndian <=> LittleEndian
function SwapEndian16(Value: smallint): smallint; register;
function SwapEndian32(Value: integer): integer; register;
function SwapEndian64 (i: Int64): Int64;


// -------------------------------------------------------------------------
//							Messages pour l'interface
// -------------------------------------------------------------------------


const
//
// ---------------------- Chaines pour l'interface --------------------------
//
APPLI_NAME_STR		= 'Drive Recovery Alpha';
APPLI_VER_STR		= 'v0.13.0';
APPLI_TITLE_STR 	= APPLI_NAME_STR + ' ' + APPLI_VER_STR;
APPLI_COPYRIGHT_STR	= '(C) 2005-2016 par CP Software.';
APPLI_COMMENT_STR	= 'Ce logiciel est fourni "en l''�tat" sans aucune garantie, l''auteur ne saurait �tre tenu responsable des dommages �ventuels occasionn�s par l''utilisation de ce logiciel.';
APPLI_DATE_STR		= '12/05/2005';

APPLI_CHM_FILENAME = 'checkpart.chm';


resourcestring

// Chaines pour les unit�s infos (� traduire eventuellement)
BYTE_STRING		= 'octet(s)';
KBYTE_STRING	= 'Ko';
MBYTE_STRING	= 'Mo';
GBYTE_STRING	= 'Go';

//BYTE_STRING		= 'byte(s)';
//KBYTE_STRING	= 'KB';
//MBYTE_STRING	= 'MB';
//GBYTE_STRING	= 'GB';


// ----------------------- Forme principale -----------------------
NOTANADMIN_MSG			= 'Vous devez avoir les droits administrateurs pour lancer cette application.';
CONFIRM_EXIT_MSG		= 'Voulez-vous quitter l''application ?';

CURRENT_DIR_SUMMARY_STR		= '%d fichier(s) dans le r�pertoire courant (%s)';
SELECTED_FILES_SUMMARY_STR	= '%d Fichier(s) selectionn�s (%s)';

SELECT_DEST_DIR_STR		= 'S�lectionner le r�pertoire de destination pour la sauvegarde';
RECOVERY_SUMMARY_STR	= '%d octet(s) sauv�s sur %d octet(s)';

SKIP_EXISTING_FILE_STR	= 'Le fichier %s existe d�ja, il ne sera pas restaur�.';

SELECT_DRIVE_REPORT_STR	= 'Drive %s - %s selected';
SELECT_PART_REPORT_STR	= 'Partition %s (%s) s�lectionn�e (LBA %d, CHS %d-%d-%d)';

SCAN_PART_ROOT_STR	= 'Scanning directories, starting from Root Directory.';
SCAN_PART_STR		= 'Scanning directories, starting from cluster %d.';
SCAN_REPORT_STR		= '%d Fichier(s) et %d R�pertoire(s) trouv�s (%s)';
SCAN_BLOCKSIZE_STR	= 'Taille r�elle occup�e sur le disque: ';

CANT_CREATE_DIR_ERROR_MSG	= 'ERREUR: Cr�ation du r�pertoire %s impossible';
CLUSTER_CHAIN_ERROR_STR		= 'ERREUR: La chaine de clusters du fichier %s est invalide';
RECOVER_FILE_ERROR_STR		= 'ERREUR: Le fichier %s n''a pas �t� restaur� correctement';

FILE_PROPERTIES_TITLE_STR	= 'Propri�t�s de %s';

UNDEFINED_STR	= 'Undefined';

PARTITION_INFO_STR	= 'Partition Info';
DRIVE_INFO_STR		= 'Drive %s Info';

START_RECOVERY_MSG	= 'Starting Recovery.';

NO_FILE_TO_RECOVER_MSG 	= 'Il n''y a pas de fichier � sauvegarder';

DEST_DRIVE_SPACE_STR	= 'Espace sur disque destination: %s - Taille fichiers: %s';

// -------------------------
ENTER_CLUSTER_TITLE_STR = 'Saisie du cluster racine';
ENTER_ROOT_CLUSTER_MSG	= 'Pour utilisateur exp�riment� !'#13'Veuillez entrer le num�ro de cluster pour le scan (de %d � %d)';

// Strings pour la liste des partitions
DUMP_PART_HEADER_STR	= 'Listes des partitions d�tect�es';
NO_PART_DETECTED_STR	= 'Pas de partition d�tect�e';
HEADER_PART_NUM_STR		= 'Info partition n� %d';
PART_TYPE_INFO_STR		= ' - FileSystem: %s (BootID: %.2x)';
PART_STATUS_STR			= ' - Status : ';
START_SECTOR_STR		= ' - Start Sector: %d (CHS %d-%d-%d)';
SECTOR_COUNT_STR		= ' - Nombre de secteurs: %d';
PART_SIZE_STR			= ' - Taille de la partition: %s';

SCAN_PART_DLG_TITLE		= 'Analyse de la partition, Veuillez patientez...';
SCAN_PART_DLG_TEXT		= 'Analyse en cours';

SEARCH_PART_DLG_TITLE	= 'Recherche des partitions, Veuillez patienter...';
ONPARTITIONINFO_STR	 	= ' - Partition: %s - %s (CHS %d-%d-%d, LBA %d) ';
PARTITIONFOUND_STR		= 'Partitions trouv�es:';
SEARCHPARTENDED_STR		= 'Recherche termin�e.';

RECOVER_DLG_TITLE		= 'Restauration des fichiers...';

// ---------------------------- Hints ---------------------------
//SELECT_DRIVE_HINT	= '';
DRIVE_INFO_HINT		= 'Affiche les informations sur le disque s�lectionn�';

//'Affiche les informations sur la partition s�lectionn�e';
LIST_FILE_HINT		= 'Sauvegarde la liste des fichiers trouv�s lors de l''analyse';
LIST_SEL_FILE_HINT	= 'Sauvegarde la liste des fichiers s�lectionn�s';
//'Restaure tous les fichiers trouv�s';
//'Restaure les fichiers s�lectionn�s';
//'Ouvre compl�tement l''arborescence';
//'Ferme compl�tement l''arborescence';
CLEAR_LOG_HINT		= 'Efface le contenu de la zone de log';
SAVE_LOG_HINT		= 'Sauvegarde le contenu de la zone de log';
SCAN_PART_HINT		= 'Analyse la partition pour trouver les fichiers';


// ----------------------- SaveDialog --------------------------
SAVEDIALOG_TITLE_STR		= 'Selectionner un fichier';
SAVEDIALOG_FILTER_STR		= 'Fichiers texte (*.txt)|*.TXT|Tous les fichiers (*.*)|*.*';
SAVEDIALOG_DEFAULT_EXT_STR	= 'txt';
SAVEDIALOG_NO_FILTER_STR	= 'Tous les fichiers (*.*)|*.*';
//SAVEDIALOG_BIN_FILTER_STR		= 'Fichiers binaire (*.bin)|*.BIN|Tous les fichiers (*.*)|*.*';

// ----------------------- Form DriveDump --------------------------
DRIVE_DUMP_BAD_POSITION_STR	= 'Position non valide';
DRIVE_DUMP_DRIVE_TITLE		= 'Visualisation Disque [Secteur]';
DRIVE_DUMP_PART_TITLE		= 'Visualisation Partition [Secteur/Cluster]';
CLUSTER_NUM_STR				= 'Cluster %d';
SECTOR_NUM_STR				= 'Secteur %d';
SECTOR_ERROR_STR			= 'Erreur Secteur %d';
SECTOR_INTERVAL_STR			= 'Secteur %d � %d';
CLUSTER_INTERVAL_STR 		= 'Cluster %d � %d';

// --------------------------- Form ZAbout ----------------------------
ABOUT_FORM_TITLE_STR	= 'A Propos...';

// --------------------------- Form SelectDrive ----------------------------
DRIVE_SELECT_TITLE_STR	= 'Choisir un disque';

FILE_LIST_SUMMARY_STR	= '%d Fichier(s) dans %s';
DIR_LIST_SUMMARY_STR	= '%d R�pertoire(s)';

RECOVERY_WARNING_MSG	= 'Il ne faut pas sauvegarder les fichiers sur la partition' + #13+
						  'qui est en cours de r�cup�ration.';

// ----------------------- SelectPartiton Form ---------------------------------
EXPERT_MODE_BAD_VALUES	= 'Valeurs non valides.';
COMBO_PART_COUNT_STR	= '%d partition(s) dans la liste';
SECTOR_NUM_TOO_SMALL	= 'Le num�ro de secteur saisi est trop petit.';
INVALID_BOOT_SECTOR_MSG	= 'La partition est corrompue (BootSector non valide).'+#13+#10+
						  'Elle n''est pas r�cup�rable avec la version actuelle du logiciel.';
ATM_NOT_SUPPORTED_PART_MSG	= 'Type de partition non support� actuellement.';

RECALC_PART_PARAM_MSG	= 'Voulez vous recalculer les informations de la partition ?' + #13+
						  'Attention ! Cela n''est valable que si la partition a �t� format�e de mani�re standard.';

// --------------------------- Messages d'erreurs ---------------------------
INVALID_CLUSTER_NUM_ERROR_MSG	= 'Num�ro de cluster non, valide';
SAVE_LOG_ERROR_MSG	= 'Erreur lors de la sauvegarde';

NOT_ENOUGH_SPACE_ON_DEST_MSG	= 'Pas assez de place disponible sur le disque destination.';


//MenuDriveHints: array[0..5] of string = ('jkkjk', 'jkljlkj', '', '5564654', '', 'kjhkjhkjh');


// ---------------------- GetXXXProperties strings -------------------------
NAME_STR			= 'Name';
TYPE_STR			= 'Type';
FILE_STR			= 'File';
SIZE_STR			= 'Size';
DIRECTORY_STR		= 'Directory';
SHORTNAME_STR		= 'Short Name (8.3)';
DOSATTRIBUTES_STR	= 'DOS Attributes';
FILEID_STR			= 'File ID';
PARENTID_DIR_STR	= 'Parent Dir ID';
LASTWRITE_STR		= 'Last Modification';
CREATEDATE_STR		= 'Creation Date';
LASTREAD_STR		= 'Last Access';

PARTITIONTYPE_STR	= 'Partition Type';
PARTSTARTAT_STR		= 'Partition starts at';
VOLUMENAME_STR		= 'Volume Name';
OEMNAME_STR			= 'OEMNAme';
SECTORPERCLUSTER_STR= 'Sectors per Cluster';
SECTORSIZE_STR		= 'Sector Size';
CLUSTERSIZE_STR		= 'Cluster Size';
HIDDENSECTORS_STR	= 'Hidden Sectors';
PARTITIONSIZE_STR	= 'Partition size';
TOTALSECTORS_STR	= 'Total Sectors';
VOLUMEID_STR		= 'Volume ID';
PARTITIONSTATUS_STR	= 'Partition Status';
PRIMARY_STR			= 'Primary';
LOGICAL_STR			= 'Logical';
BOOTABLE_STR		= 'Bootable';

FATCOUNT_STR		= 'Number of FAT(s)';
FATSIZE_STR			= 'FAT Size';
RSVDSECTORCOUNT_STR	= 'Reserved Sector Count';

FILERECORDSIZE_STR	= 'File Record Size';
CLUSPERFILERECORD_STR='Clusters Per File Record';
INDEXBLOCKSIZE_STR	= 'Index Block Size';
CLUSTPERINDXBLCK_STR= 'Clusters Per Index Block';
MFTFIRSTCLUST_STR	= '$MFT first cluster';
MFTMIRFIRSTCLUST_STR= '$MFT Mirror first cluster';
NTFSVOLUMEFLAGS_STR	= 'Volume Flags';
MFTDATE_STR			= 'MFT Change Date';
NTFSPERMATTR_STR	= 'NTFS Attributes';
NTFSSERIAL_STR		= 'NTFS Serial Number';
MFTSIZE_STR			= 'MFT Size';
CHECKSUM_STR		= 'Checksum';


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

Uses
	SysUtils, Math;


Const

// Valeur des unit�s infos (1 Go = 1024 Mo ou 1000 Mo ????)
KBYTE_VALUE		= 1024;
MBYTE_VALUE		= KBYTE_VALUE * KBYTE_VALUE;
GBYTE_VALUE		= MBYTE_VALUE * KBYTE_VALUE;


// -------------------------------------------------------------------------
//							Conversion des dates
// -------------------------------------------------------------------------

//http://msdn.microsoft.com/library/default.asp?url=/library/en-us/sysinfo/base/dosdatetimetofiletime.asp
//FileTimeToSystemTime(fileinfo.WriteDate, syswritedate);
//writedate := SystemTimeToDateTime(syswritedate);
//function DateTimeToStr(DateTime: TDateTime): string;
//procedure DateTimeToString(var Result: string; const Format: string; DateTime: TDateTime);

// TFileTime : Number of 100-nanosecond intervals since January 1, 1601.

//
//
//
function FileTimeToDateTime(const ftime: TFileTime): TDateTime;
var
	systime: TSystemTime;
begin
	FileTimeToSystemTime(ftime, systime);
    Result := SystemTimeToDateTime(systime);
end;


//
// Converti un FileTime en un chaine contenant la date et l'heure
//
function FileTimeToString(const ftime: TFileTime): string;
var
	systime: TSystemTime;
    datetime: TDateTime;
begin
	if FileTimeToSystemTime(ftime, systime) then
    begin
    	datetime := SystemTimeToDateTime(systime);
        //datetimestr := DateTimeToStr(datetime);
        DateTimeToString(Result, 'dd/mm/yyyy hh:nn:ss', datetime);
    end
    else
		Result := '??/??/???? ??:??:??';
end;


//
// Converti un FileTime dans deux chaines, une contenant la date et l'autre l'heure
//
// http://www.swissdelphicenter.ch/torry/showcode.php?id=671
// #Todo2 Pb avec les changements d'heures ??? UTC vs localtime
procedure FileTimeToDateAndTimeString(const ftime: TFileTime; var datestr, timestr: string);
var
	systime: TSystemTime;
    datetime: TDateTime;
begin
	if FileTimeToSystemTime(ftime, systime) then
    begin
    	datetime := SystemTimeToDateTime(systime);
        //datetimestr := DateTimeToStr(datetime);
        DateTimeToString(datestr, 'dd/mm/yyyy', datetime);
        DateTimeToString(timestr, 'hh:nn:ss', datetime);
    end
    else
    begin
        datestr := '??/??/????';
        timestr := '??:??:??';
    end;
end;


//
//
//
function DateTimeToFileTime(const FileTime: TDateTime): TFileTime;
var
  LocalFileTime, Ft: TFileTime;
  SystemTime: TSystemTime;
begin
  Result.dwLowDateTime  := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(FileTime, SystemTime);
  SystemTimeToFileTime(SystemTime, LocalFileTime);
  LocalFileTimeToFileTime(LocalFileTime, Ft);   // necessary ??
  Result := Ft;
end;

// -------------------------------------------------------------------------
//						Conversion Size to String
// -------------------------------------------------------------------------


//
//
//
function SizeToRoundedKiloString(const size : int64) : string;
var
	tmpsize : real;
begin
	// ATTN bug si size > 2 000 Go !!!
	tmpsize := ceil(size / 1024);
    if tmpsize < 1000000 then
    	Result := Format('%.0n %s',[tmpsize, KBYTE_STRING])
    else
    begin
		tmpsize := ceil(tmpsize / 1024);
		    if tmpsize < 1000000 then
		    	Result := Format('%.0n %s',[tmpsize, MBYTE_STRING])
            else
            begin
				tmpsize := ceil(tmpsize / 1024);
		    	Result := Format('%.0n %s',[tmpsize, GBYTE_STRING])
            end;
    end;
end;


//
// Affiche avec l'unit� minimum utilisable (la valeur affich�e < 1024): 
// 1000 => 1000 octet(s), 1269 => 2 Ko, 16985 => 17 Ko, 1000225 => 976 Ko
//
function SizeToRoundedByteString(const value : int64): string;
var
	tmpval : int64;
    unity : string;
begin
    unity := BYTE_STRING;
    if value < KBYTE_VALUE then
        tmpval := value
    else
    	if value < MBYTE_VALUE then
        begin
			// Afficher des Ko
            tmpval := value div KBYTE_VALUE;
            unity := KBYTE_STRING;
            if (value mod KBYTE_VALUE) > 0 then
            	inc(tmpval);
        end
        else
        	if value < GBYTE_VALUE then
            begin
				// Afficher des Mo
                tmpval := value div MBYTE_VALUE;
	            unity := MBYTE_STRING;
                if (value mod MBYTE_VALUE) > 0 then
                    inc(tmpval);
            end
            else
            begin
				// Afficher des Go
                tmpval := value div GBYTE_VALUE;
	            unity := GBYTE_STRING;
                if (value mod GBYTE_VALUE) > 0 then
                    inc(tmpval);
            end;

    Result := Format('%d %s',[tmpval, unity]);
end;


//
// Transforme la taille en octets en chaine avec 2 chiffres apres la virgule
// utilise l'unit� la plus proche
//
function SizeTo2DigitByteString(const value : int64): string;
var
	tmpsize : real;
    unity : string;
begin
    unity := BYTE_STRING;
    
    if value < KBYTE_VALUE then
        tmpsize := value
    else
    	if value < MBYTE_VALUE then
        begin
			// Afficher des Ko
            tmpsize := value / KBYTE_VALUE;
            unity := KBYTE_STRING;
        end
        else
        	if value < GBYTE_VALUE then
            begin
				// Afficher des Mo
                tmpsize := value / MBYTE_VALUE;
	            unity := MBYTE_STRING;
            end
            else
            begin
				// Afficher des Go
                tmpsize := value / GBYTE_VALUE;
	            unity := GBYTE_STRING;
            end;

    Result := Format('%g %s',[SimpleRoundTo(tmpsize), unity]);
end;


//
// Convertit un int64 en une chaine avec un s�parateur de milliers
//
function Int64ToThousandSepString(const valeur: int64): string;
//function SizeToThousandSepString(const valeur: int64): string;
var
	dividende: int64;
begin
	Result := '';

    dividende := valeur;
    repeat
        Result := Format('%.3d', [dividende mod 1000]) + Result;
        dividende := dividende div 1000;
        if dividende > 0 then
        	if ThousandSeparator <> #0 then
            	Result := ThousandSeparator + Result;
    until dividende = 0;

    // Virer les eventuels 0 � gauche
	if Result[1] = '0' then
    	Result := Copy(Result, 2, 1000);
	if Result[1] = '0' then
    	Result := Copy(Result, 2, 1000);
end;

type
TRoundType = (rtFloor, rtCeil, rtInt);


// -------------------------------------------------------------------------
//						Conversion Big Endian to Little Endian
// -------------------------------------------------------------------------

// http://stackoverflow.com/questions/3065335/how-to-convert-big-endian-numbers-to-native-numbers-delphi

//
// 16 bits
//
function SwapEndian16(Value: smallint): smallint; register;
asm
  xchg  al, ah
end;


//
// 32 bits
//
function SwapEndian32(Value: integer): integer; register;
asm
  bswap eax
end;


// http://codeverge.com/embarcadero.delphi.basm/fastest-best-way-to-reverse-byte-orde/1096017

//
// 64 bits
// NOT WORKING
function SwapEndian64(i: Int64): Int64;
//function SwapEndian64old (i: Int64): Int64; register;
asm
	mov eax, dword [i]
	bswap eax
	mov dword [Result+4], eax
	mov eax, dword [i+4]
	bswap eax
	mov dword [Result], eax
end;

//function SwapEndian64(i: Int64): Int64;
//
// [EBP+$10] Valeur de A
// [EBP+$08] Valeur de B
// R�sultat : contenu dans EDX:EAX
//Asm
//  MOV EDX,DWORD PTR i
//  BSWAP EDX
//  MOV EAX,DWORD PTR i[4]
//  BSWAP EAX
//  mov dword PTR Result[4], edx
//  mov dword PTR Result, eax
//end;


//
//
//
function SwapEndian64sss (i: Int64): Int64;
var
	hi, lo: Cardinal;
  pi,pr : array of Byte;
  idx: integer;
begin
	pi := @i;
  pr := @Result;

  for idx := 0 to 7 do
		pr[7-idx] := pi[idx];

	//lo := Cardinal(i and $ffffffff);
  //hi := Cardinal((i shr 32) and $ffffffff);
  //Result := int64(SwapEndian32(lo)) shl 32 or int64(SwapEndian32(hi));
end;

//function SwapEndian64(i: Int64): Int64;
//asm
//        MOV     EDX,i.Int64Rec.Lo
//        BSWAP   EDX
//        MOV     EAX,i.Int64Rec.Hi
//        BSWAP   EAX
//end;

function SwapEndian64zzzz(i: Int64): Int64;
asm
{$IF Defined(CPU386)}
  mov    edx, [ebp+$08]
  mov    eax, [ebp+$0c]
  bswap  edx
  bswap  eax
{$ELSEIF Defined(CPUX64)}
  mov    rax, rcx
  bswap  rax
{$ELSE}
{$Message Fatal 'ByteSwap64 has not been implemented for this architecture.'}
{$IFEND}
end;

//
//
//
{function SizeTo3DigitString(const size: int64; const bin: boolean); string;
var
	tmp, prevtmp: int64;
    unite: Cardinal;

begin
	unite := 0;
	prevtmp := 0;
    tmp := size;

    while (tmp >= 1000) do
    begin
    	prevtmp := tmp;
        tmp := tmp div 1024;	//#Todo2 changer l'unit�
        inc(unite);
    end;

    // Calculer l'arrondi
    prevtmp := prevtmp mod 1024;	//#Todo2 changer l'unit�
    case  of
        rtCeil:
        	if prevtmp > 0 then
		        inc(tmp);
        rtInt:
        	if prevtmp > 500 then
		        inc(tmp);
    end;

	// Transformer en chaine
    //Result := IntToStr(tmp) + ' ' + chaine unite
end;}

// --------------------------- Fin de l'unit� -----------------------------
end.
