{-----------------------------------------------------------------------------
 Unit Name: XFSPartition
 Author:    Chris
 Purpose:   Classe pour gerer objets fichier / repertoire de la partitions XFS
 History:   12/07/2016
-----------------------------------------------------------------------------}
unit XFSFileInfo;

interface

uses
	Classes, SysUtils, CommonFileInfo, XFSCommon;

// http://www.delphibasics.co.uk/RTL.asp?Name=tlist

type

(*
 * Creation des XFSFileInfo, 2 possibilités
 * 1) parcours des blocks contenant les inodes => inode num + buf
 * 2) parcours de la liste des inodes d'un directory => (inode num + filename + 
 *    parent dir)
 *
 * Il faut avoir la possibilité de changer le nom à posteriori (cas 1)
 * ATTENTION FIsDirectory n'est renseigné correctement que si l'on a récupérer
 * les infos de inode_core
 *)

// Class for file and directory for XFS filesystem
TXFSFileInfo = class(TCommonFileInfo)
    private
    protected
      // FFileID = absolute inode number of the file
      // FParentDirID = absolute inode number of the parent directoty
    	FFileName: string;	// remonter à la racince ???
      FIsDirectory: boolean;
      DataExtentsList: TList; 
      XFSAttributes: TStringList;	// Attributes of inode
        // ============================= Methodes =============================
        procedure LoadInfoFromInodeBuffer(buf: Pointer);
        procedure LoadInfoFromInode();
        // Properties
        function GetFileName(): string; override;
        function GetShortName(): string; override;
        function GetDosAttributes(): Byte; override;
        // Data Extents
        function GetDataExtentsCount(): Cardinal;
        function GetBlockOffset(Index: Cardinal): int64;
        function GetStartBlock(Index: Cardinal): int64;
        function GetBlockCount(Index: Cardinal): int64;
    public
        // ============================ Methodes ============================
        constructor Create(const inode: int64; buf: Pointer); overload;  //#Todo add parameter to constructor
        constructor Create(const inode: int64; buf: Pointer; del: TList); overload;  //#Todo add parameter to constructor
        constructor Create(const inode, parentid: int64; inodename: string); overload;
        destructor Destroy; override;
        //
        procedure GetFileProperties(info: TStrings); override;
        function isDirectory(): boolean; override;
        function isDeleted():boolean; override;
        // ============================= Propriétés =============================
        property DECount: Cardinal read GetDataExtentsCount;
        property DESBlockOffset[Index:Cardinal]: int64 read GetBlockOffset;
        property DEStartBlock[Index:Cardinal]: int64 read GetStartBlock;
        property DEBlockCount[Index:Cardinal]: int64 read GetBlockCount;
end;


//
function XFSIsValidInodeCore(buf: Pointer): boolean;
function XFSIsFileOrDirInode(buf: Pointer): boolean;


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation


uses
	Math, DateUtils, ConversionUnit, LinuxPartition;

const
	MAX_DATA_EXTENTS_INFO = 15;	// nombre max de data extents à afficher dans les infos

  
//
//
function XFSTimeStampToDateTime(const ts: xfs_timestamp):TDateTime;
begin
  Result := EncodeDate(1970, 1, 1);
  Result := IncSecond(Result, SwapEndian32(ts.t_sec));
end;


(*
* The on-disk inode record structure has two formats. The original 'full'
* format uses a 4-byte freecount. The 'sparse' format uses a 1-byte freecount
* and replaces the 3 high-order freecount bytes wth the holemask and inode
* count.
*
* The holemask of the sparse record format allows an inode chunk to have holes
* that refer to blocks not owned by the inode record. This facilitates inode
* allocation in the event of severe free space fragmentation.
*)
//type
//	xfs_inobt_rec = record
//		ir_startino: __be32;	    (* starting inode number *)
//        	case Integer of
//			0:(
//
//			);
//			1:(ir_freecount: __be32);
//	end;
//	f = xfs_inobt_rec;
//                 begin
//                        __be16  ir_holemask;(* hole mask for sparse chunks *)
//                        __u8    ir_count;       (* total inode count *)
//                        __u8    ir_freecount;   (* count of free inodes *)
//                 end; sp;
//         end; ir_u;
//        __be64          ir_free;        (* free inode mask *)
// end; xfs_inobt_rec_t;



// --------------------------------------------------------------------------
//							Constructeurs et destructeurs
// --------------------------------------------------------------------------


//
// inode absolute inode
//
constructor TXFSFileInfo.Create(const inode, parentid: int64; inodename: string);
begin
  Self.FFileID := inode;
  Self.FParentDirID := parentid;
  Self.FFileName := inodename;

  Self.XFSAttributes := TStringList.Create;
  // charger les infos à partir
end;


//
// inode: absolute inode
// buf: buffer containing inode
//
constructor TXFSFileInfo.Create(const inode: int64; buf: Pointer);
begin
  Self.FFileID := inode;
  Self.FParentDirID := -1;
	Self.FFileName := IntToStr(inode);

  Self.XFSAttributes := TStringList.Create;

  // charger les infos à partir du buffer
  Self.LoadInfoFromInodeBuffer(buf);
end;


//
//
//
constructor TXFSFileInfo.Create(const inode: int64; buf: Pointer; del: TList);
begin
	Self.Create(inode, buf);

  Self.DataExtentsList := del;
end;



//
//
//
destructor TXFSFileInfo.Destroy;
var
	i: integer;
begin
	inherited;

  // Release Data Extents memory
  if Assigned(Self.DataExtentsList) then
  begin
  	for i := 0 to Self.DataExtentsList.Count - 1 do
    	Dispose(Self.DataExtentsList[i]);

    Self.DataExtentsList.Free;
  end;

	Self.XFSAttributes.Free;
end;


//
//
//
procedure TXFSFileInfo.LoadInfoFromInode;
begin
	// decode l'inode
end;



// --------------------------------------------------------------------------
//								Gestion des inodes
// --------------------------------------------------------------------------


//
// Decode Inode data and retrieve associated data
//
procedure TXFSFileInfo.LoadInfoFromInodeBuffer(buf: Pointer);
var
	pino: ^xfs_dinode_core;
begin
  pino := buf;

  //  Recuperer les infos communes
  Self.FFileSize := SwapEndian64(pino.di_size);

  // retrieve dates
	Self.FCreateDate := DateTimeToFileTime(XFSTimeStampToDateTime(pino.di_ctime));
	Self.FWriteDate := DateTimeToFileTime(XFSTimeStampToDateTime(pino.di_mtime));
	Self.FAccessDate := DateTimeToFileTime(XFSTimeStampToDateTime(pino.di_atime));

	// Traitements spécifiques DIR ou FILE
  case (SwapEndian16(pino.di_mode) and S_IFMT) of
  	S_IFREG:
      begin
      	Self.FIsDirectory := False;

        // data fork (cf p 120 of XFS Algorithms & Data Structures
        case pino.di_format of
          XFS_DINODE_FMT_EXTENTS: ;
          XFS_DINODE_FMT_BTREE : ;
        end;	// case pino.di_format

        // attribure fork
        if pino.di_forkoff <> 0 then
          case pino.di_aformat of
            XFS_DINODE_FMT_LOCAL: ;
            XFS_DINODE_FMT_EXTENTS: ;
            XFS_DINODE_FMT_BTREE : ;
            else;
          end; // case pino.di_aformat
    	end; // S_IFREG
    S_IFDIR:
     	begin
      	Self.FIsDirectory := True;

        // data fork
        case pino.di_format of
          XFS_DINODE_FMT_LOCAL: ;
          XFS_DINODE_FMT_EXTENTS: ;
          XFS_DINODE_FMT_BTREE : ;
        end;

        // attribure fork
        if pino.di_forkoff <> 0 then
          case pino.di_aformat of
            XFS_DINODE_FMT_LOCAL: ;
            XFS_DINODE_FMT_EXTENTS: ;
            XFS_DINODE_FMT_BTREE : ;
            else;
          end; // case pino.di_aformat
     	end;
    else;
  end; // end case
end;


(*
Root Inode


Structure of root block (level = 1)
------------------------------------
   _______________________________
  | xfs_inobt_block_t             |
  |_______________________________|
  | xfs_inobt_rec_t [bb_numrecs]  |
  |_______________________________|

  each xfs_inobt_rec_t reference a chunk of 64 inodes starting at ir_startino
  
*)


//
//
//
function XFSIsValidInodeCore(buf: Pointer): boolean;
var
	pino: ^xfs_dinode_core;
begin
	// Default
  Result := false;

  pino := buf;
  if SwapEndian16(pino.di_magic) = XFS_DINODE_MAGIC then
		Result := true;
end;


//
//
//
function XFSIsFileOrDirInode(buf: Pointer): boolean;
var
	pino: ^xfs_dinode_core;
begin
	// Default
	Result := false;

  pino := buf;
  if XFSIsValidInodeCore(buf) then
  	if ((SwapEndian16(pino.di_mode) and S_IFMT) = S_IFREG) or ((SwapEndian16(pino.di_mode) and S_IFMT) = S_IFDIR) then
			Result := true;
end;


// --------------------------------------------------------------------------
//							Gestion des propriétés
// --------------------------------------------------------------------------


procedure TXFSFileInfo.GetFileProperties(info: TStrings);
var
	i: integer;
begin
	inherited;

  info.Add('Created Time');
  info.Add(FileTimeToString(Self.CreationDate));
  info.Add('Modified Time');
  info.Add(FileTimeToString(Self.WriteDate));
  info.Add('Last Access Time');
  info.Add(FileTimeToString(Self.AccessDate));

  // Data Extents
  if not Self.isDirectory then
  	if Assigned(Self.DataExtentsList) then
    begin
     	info.Add('Data Extents count');
     	info.Add(Inttostr(Self.DECount));

    	for i := 0 to Min(Self.DECount - 1, MAX_DATA_EXTENTS_INFO) do
      begin
      	info.Add(Format('Data Extents %d',[i]));
        // DEBUG Error for directories, ok pour les fichiers (pb test directory ?)
        info.Add(Format('(%d, %d, %d)',[Self.DESBlockOffset[i], Self.DEStartBlock[i], Self.DEBlockCount[i]]));
      end;
    end;
end;


//
//
//
function TXFSFileInfo.GetFileName(): string;
begin
	Result := Self.FFileName;
end;


function TXFSFileInfo.GetShortName(): string;
begin
	Result := Self.GetFileName;
end;


function TXFSFileInfo.GetDosAttributes(): Byte;
begin
	Result := 0;	// not supported in XFS
end;


function TXFSFileInfo.isDirectory(): Boolean;
begin
	Result := Self.FIsDirectory;
end;


function TXFSFileInfo.isDeleted(): Boolean;
begin
	Result := false;
end;


//
//
//
function TXFSFileInfo.GetDataExtentsCount(): Cardinal;
begin
	if Assigned(Self.DataExtentsList) then
  	Result := Self.DataExtentsList.Count
  else
  	Result := 0;
end;


//
// Index should exists in TList
//
function TXFSFileInfo.GetBlockOffset(Index: Cardinal): int64;
begin
	if Index <= (Self.DECount - 1) then
  	Result := PDataExtents(Self.DataExtentsList[Index]).deStartOffset
  else
		Result := 0;	//#Todo2 should raise Exception
end;


//
// Index should exists in TList
//
function TXFSFileInfo.GetStartBlock(Index: Cardinal): int64;
begin
	if Index <= (Self.DECount - 1) then
  	Result := PDataExtents(Self.DataExtentsList[Index]).deStartBlock
  else
		Result := 0;	//#Todo2 should raise Exception
end;


//
// Index should exists in TList
//
function TXFSFileInfo.GetBlockCount(Index: Cardinal): int64;
begin
	if Index <= (Self.DECount - 1) then
  	Result := PDataExtents(Self.DataExtentsList[Index]).deBlockcount
  else
		Result := 0;	//#Todo2 should raise Exception
end;


// --------------------------- Fin de l'unité -----------------------------
end.
