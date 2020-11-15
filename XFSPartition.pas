{-----------------------------------------------------------------------------
 Unit Name: XFSPartition
 Author:    Chris
 Purpose:   Classe pour gerer les partitions XFS
 History:   12/07/2016
-----------------------------------------------------------------------------}
unit XFSPartition;

{
 Each XFS partition is break down in several AG
 Each AG begins with 3 information sectors containing superblock, XAGF, XAGI
   ___________________________________
  |  Allocation Group                 |
  |  _______________________________  |
  | | superblock (sector 0)         | |
  | |_______________________________| |
  | | AGF (sector 1)                | |
  | |_______________________________| |
  | | AGI (sector 2)                | |
  | |_______________________________| |
  | | AGFL (sector 3)               | |
  | |_______________________________| |
  |                                   |

 Info to retreive for each AG
 xfs_agi.agi_count
xfs_agi.agi_root
xfs_agi.agi_level
agi_seqno ??? num AG ??
agf_length agi_length

Bibliography
https://www.dtc.umn.edu/disc/resources/lu.pdf

}

interface

uses
  Classes, SysUtils, Windows,
  GenericPartition, CommonFileInfo, GenericDrive, LinuxPartition, ConversionUnit,
  XFSCommon;

const
	BIGBUFFER_SIZE = 512 * 1024;	// 20201107

type

PClusterInode = ^TClusterInode;
	TClusterInode = record
  	clusterNum : int64;
    bufferOffset : Cardinal;
  end;



//
// Classe spécialisée dans la gestion des partitions XFS
//
TXFSPartition = class(TLinuxPartition)
    private
    	// data from super block
      sb_inodelog: Byte;
      sb_inopblog:	Byte;
      sb_agblklog:  Byte; // log2 value of sb_agblocks (rounded up). Used to generate inode numbers and absolute block numbers defined in extent maps
    protected
      // Infos Superblock
      fpRootDirectory : int64;	// inode number for root directory (sb_rootino)
      fpBlockCount : int64; // remonter dans GenericPartition (ClusterCount)
      fpFreeBlocks : int64; // sb_fdblocks = free data blocks
      fpInodeCount: int64;	// Allocated inodes for filesystem
      fpInodeSize : Cardinal;		// same inode size for all AG ?
      fpDirBlockSize : Cardinal;	// size in bytes of a directory block
			fpFSName : string;
      fpAGCount : integer;
      // array of AGInformation
      fpAGBlockSize : Cardinal;
      fpAGRootInode : Cardinal;
      fpAGInodeCount: Cardinal;
        // ============================= Methodes =============================
      function GetFileSystemName : String; override;

      function GetInfoFromAGF(buf: Pointer): boolean;
      function GetInfoFromAGI(buf: Pointer):boolean;
      function GetInfoFromSuperBlock(buf: Pointer):boolean;
      //
      function AbsBlockNumberToCluster(bn: int64): int64;
			procedure InodeToCluster(const ino: int64; pino_rec: PClusterInode);
      // Data Extents method
      procedure DecodeDataExtents(pde: Pointer; var dde: TDataExtents);
      function GetFileDataExtents(inodebuf: Pointer): TList;
      procedure GetBTreeDataExtents(del: TList; nodecluster: Cardinal);
      // directory methods
      procedure DecodeDirectoryShortForm(pino: Pointer);
      procedure DecodeDirectoryBlock(pbuf: Pointer);
      //
      procedure ParseDirectoryInode(buf: Pointer);
      procedure GetInodeList();
    public
      // ============================= Methodes =============================
      constructor Create(disque: TGenericDrive; const firstsect: Cardinal); override;
      destructor Destroy; override;
      function LoadFromPartInfo(info: PPartitionInfo): boolean; override;
      //
      procedure GetPartitionProperties(info: TStrings); override;
			//
      function ScanRootDirectory(): boolean; override; // Scan depuis la racine
      function ScanRelativeDirectory(const dirid: int64): boolean; override;
      function RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean; override;
      // ============================= Propriétés =============================
      // ----------------- Propriétés de la partition -------------------
      property AGCount 		: integer read fpAGCount;
      property DirectoryBlockSize : Cardinal read fpDirBlockSize;
end;



// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

uses
  XFSFileInfo;

const

XFS_DEFAULT_CLUSTER_SIZE = 4096;
CLUSTERS_BUFFER_SIZE = XFS_DEFAULT_CLUSTER_SIZE * 128; 

// main structures sectors offset from beginning of AG
XFS_SB_SECTOR_OFFSET = 0;
XFS_AGF_SECTOR_OFFSET = 1;
XFS_AGI_SECTOR_OFFSET = 2;

// http://lxr.free-electrons.com/source/fs/xfs/xfs_alloc_btree.h?v=2.6.24
const XFS_ABTB_MAGIC = $41425442;	(* 'ABTB' for bno tree *)
const XFS_ABTC_MAGIC = $41425443;	(* 'ABTC' for cnt tree *)

(*
 * There is a btree for the inode map per allocation group.
 *)
const XFS_IBT_MAGIC = $49414254;	(* 'IABT' *)


// http://lxr.free-electrons.com/source/fs/xfs/xfs_sb.h?v=3.6
const	XFS_SB_MAGIC = $58465342;      (* 'XFSB' *)

// Linux/fs/xfs/uuid.h
type
  uuid_t = record
		__u_bits: array[0..15] of Byte;
end;

(*
 * Superblock - on disk version.  Must match the in core version above.
 * Must be padded to 64 bit alignment.
 *)
	xfs_dsb = record
		sb_magicnum : __be32;     (* magic number == XFS_SB_MAGIC *)
		sb_blocksize : __be32;    (* logical block size, bytes *)
		sb_dblocks  : __be64;     (* number of data blocks *)
		sb_rblocks  : __be64;     (* number of realtime blocks *)
		sb_rextents : __be64;     (* number of realtime extents *)
		sb_uuid     : uuid_t;     (* file system unique id *)
		sb_logstart : __be64;     (* starting block of log if internal *)
		sb_rootino  : __be64;     (* root inode number *)  // root directory inode
		sb_rbmino   : __be64;     (* bitmap inode for realtime extents *)
		sb_rsumino  : __be64;     (* summary inode for rt bitmap *)
		sb_rextsize : __be32;     (* realtime extent size, blocks *)
		sb_agblocks : __be32;     (* size of an allocation group *)
		sb_agcount  : __be32;     (* number of allocation groups *)
		sb_rbmblocks : __be32;    (* number of rt bitmap blocks *)
		sb_logblocks : __be32;    (* number of log blocks *)
		sb_versionnum : __be16;   (* header version == XFS_SB_VERSION *)
		sb_sectsize : __be16;     (* volume sector size, bytes *)
		sb_inodesize : __be16;    (* inode size, bytes *)
		sb_inopblock : __be16;    (* inodes per block *)
		sb_fname    : array[0..11] of char; (* file system name *)
		sb_blocklog : __u8;       (* log2 of sb_blocksize *)
		sb_sectlog  : __u8;       (* log2 of sb_sectsize *)
		sb_inodelog : __u8;       (* log2 of sb_inodesize *)
		sb_inopblog : __u8;       (* log2 of sb_inopblock *)
		sb_agblklog : __u8;       (* log2 of sb_agblocks (rounded up) *)
		sb_rextslog : __u8;       (* log2 of sb_rextents *)
		sb_inprogress : __u8;     (* mkfs is in progress, don't mount *)
		sb_imax_pct : __u8;       (* max % of fs for inode space *)
   (* statistics *)
   (*
    * These fields must remain contiguous.  If you really
    * want to change their layout, make sure you fix the
    * code in xfs_trans_apply_sb_deltas().
    *)
   	sb_icount  : __be64;        (* allocated inodes *)
		sb_ifree   : __be64;        (* free inodes *)
		sb_fdblocks : __be64;       (* free data blocks *)
		sb_frextents : __be64;      (* free realtime extents *)
   (*
    * End contiguous fields.
    *)
		sb_uquotino : __be64;     (* user quota inode *)
		sb_gquotino : __be64;     (* group quota inode *)
		sb_qflags   : __be16;     (* quota flags *)
		sb_flags    : __u8;       (* misc. flags *)
		sb_shared_vn : __u8;      (* shared version number *)
		sb_inoalignmt : __be32;   (* inode chunk alignment, fsblocks *)
		sb_unit     : __be32;     (* stripe or raid unit *)
		sb_width    : __be32;     (* stripe or raid width *)
		sb_dirblklog : __u8;      (* log2 of dir block size (fsbs) *)
		sb_logsectlog : __u8;     (* log2 of the log sector size *)
		sb_logsectsize : __be16;  (* sector size for the log, bytes *)
		sb_logsunit : __be32;     (* stripe unit size for the log *)
		sb_features2 : __be32;    (* additional feature bits *)
   (*
    * bad features2 field as a aResult of failing to pad the sb
    * structure to 64 bits. Some machines will be using this field
    * for features2 bits. Easiest just to mark it bad and not use
    * it for anything else.
    *)
		sb_bad_features2 : __be32;
        (* must be padded to 64 bit alignment *)
 	end;


// http://lxr.free-electrons.com/source/fs/xfs/xfs_ag.h?v=2.6.24
(*
 * Btree number 0 is bno, 1 is cnt.  This value gives the size of the
 * arrays below.
 *)

const XFS_AGF_MAGIC = $58414746;      // 'XAGF'
const XFS_AGF_VERSION = 1;
const XFS_BTNUM_AGF = 2;

type
 (*
 * The second word of agf_levels in the first a.g. overlaps the EFS
 * superblock's magic number.  Since the magic numbers valid for EFS
 * are > 64k, our value cannot be confused for an EFS superblock's.
 *)
xfs_agf = record
  (*
   * Common allocation group header information
   *)
	agf_magicnum   : __be32;                   (* magic number == XFS_AGF_MAGIC *)
	agf_versionnum : __be32;                   (* header version == XFS_AGF_VERSION *)
	agf_seqno      : __be32;                   (* sequence # starting from 0 *)
	agf_length     : __be32;                   (* size in blocks of a.g. *)
  (*
   * Freespace information
   *)
	agf_roots      : array[0..XFS_BTNUM_AGF-1] of __be32;  (* root blocks *)
	agf_spare0     : __be32;                   (* spare field *)
	agf_levels     : array[0..XFS_BTNUM_AGF-1] of __be32;  (* btree levels *)
	agf_spare1     : __be32;                   (* spare field *)
  agf_flfirst    : __be32;                   (* first freelist block's index *)
	agf_fllast     : __be32;                   (* last freelist block's index *)
	agf_flcount    : __be32;                   (* count of blocks in freelist *)
	agf_freeblks   : __be32;                   (* total free blocks *)
	agf_longest    : __be32;                   (* longest free space *)
	agf_btreeblks  : __be32;                   (* # of blocks held in AGF btrees *)
end;
//xfs_agf_t = xfs_agf;


const XFS_AGI_MAGIC = $58414749;      // 'XAGI'
const XFS_AGI_VERSION = 1;
{*
 * Size of the unlinked inode hash table in the agi.
 *}
const XFS_AGI_UNLINKED_BUCKETS = 64;

type
	xfs_agi = record
   (*
    * Common allocation group header information
    *)
		agi_magicnum   : __be32;     (* magic number == XFS_AGI_MAGIC *)
		agi_versionnum : __be32;     (* header version == XFS_AGI_VERSION *)
		agi_seqno      : __be32;     (* sequence # starting from 0 *)
		agi_length     : __be32;     (* size in blocks of a.g. *)
   (*
    * Inode information
    * Inodes are mapped by interpreting the inode number, so no
    * mapping data is needed here.
    *)
		agi_count      : __be32;        (* count of allocated inodes *)
		agi_root       : __be32;        (* root of inode btree, => xfs_inobt_block_t *)
		agi_level      : __be32;        (* levels in inode btree *)
		agi_freecount  : __be32;        (* number of free inodes *)
		agi_newino     : __be32;        (* new inode just allocated *)
		agi_dirino     : __be32;        (* last directory inode chunk *)
   (*
    * Hash table of inodes which have been unlinked but are
    * still being referenced.
    *)
		agi_unlinked   : array[0..XFS_AGI_UNLINKED_BUCKETS-1] of __be32;
	end;
	//xfs_agi_t = xfs_agi;


//union {
//        xfs_bmdr_block_t di_bmbt;       /* btree root block */ // di_format == XFS_DINODE_FMT_BTREE && (di_mode == S_IFREG || di_mode == S_IFDIR)
//        xfs_bmbt_rec_32_t di_bmx[1];    /* extent list */ // di_format == XFS_DINODE_FMT_EXTENTS && (di_mode == S_IFREG || di_mode == S_IFDIR || di_mode == S_IFLNK)
//        xfs_dir2_sf_t   di_dir2sf;      /* shortform directory v2 */
//        char            di_c[1];        /* local contents */
//        __be32          di_dev;         /* device for S_IFCHR/S_IFBLK */
//        uuid_t          di_muuid;       /* mount point value */
//        char            di_symlink[1];  /* local symbolic link */
//} di_u;
//
//union {
//        xfs_bmdr_block_t di_abmbt;      /* btree root block */ XFS_DINODE_FMT_BTREE
//        xfs_bmbt_rec_32_t di_abmx[1];   /* extent list */ XFS_DINODE_FMT_EXTENTS
//        xfs_attr_shortform_t di_attrsf; /* shortform attribute list */ XFS_DINODE_FMT_LOCAL
//} di_a;

///*
// * Entries are packed toward the top as tight as possible.
// */
//typedef struct xfs_attr_shortform {
//        struct xfs_attr_sf_hdr {        /* constant-structure header block */
//                __be16  totsize;        /* total bytes in shortform list */
//                __u8    count;  /* count of active entries */
//        } hdr;
//        struct xfs_attr_sf_entry {
//                __uint8_t namelen;      /* actual length of name (no NULL) */
//                __uint8_t valuelen;     /* actual length of value (no NULL) */
//                __uint8_t flags;        /* flags bits (see xfs_attr_leaf.h) */
//                __uint8_t nameval[1];   /* name & value bytes concatenated */
//        } list[1];                      /* variable sized array */
//} xfs_attr_shortform_t;
//typedef struct xfs_attr_sf_hdr xfs_attr_sf_hdr_t;
//typedef struct xfs_attr_sf_entry xfs_attr_sf_entry_t;


// AG Specific informations
type AGInformations = record
	AGSeqNo : Cardinal; // numéro de l'AG agi_seqno or agf_seqno
	AGLength : Cardinal; // agi_length or agf_length, count of blocks (clusters) for this AG
  AGRootInode : Cardinal; // agi_root for this AG
  AGILevel : Cardinal; // agi_level for this AG
end;


// ----------------------------------------------------------------------
//						Constructeur et Destructeurs
// ----------------------------------------------------------------------


//
// Crée la partitionXFS avec le drive et le numéro de secteur où commence la partition
//
constructor TXFSPartition.Create(disque: TGenericDrive; const firstsect: Cardinal);
begin
	inherited Create(disque, firstsect);
  //
end;


//
//
//
destructor TXFSPartition.Destroy;
begin
	inherited Destroy;
end;


// ----------------------------------------------------------------------
//						Gestion du BootBlock
// ----------------------------------------------------------------------


//
// Récupère les infos en se basant sur TPartitionInfo
//
function TXFSPartition.LoadFromPartInfo(info: PPartitionInfo): boolean;
var
	currentAG : integer;
begin
	inherited LoadFromPartInfo(info);

  Self.fpClusterSize := XFS_DEFAULT_CLUSTER_SIZE;

  // Lire superblock
  // Lire XAGF
  // Lire XAGI
  // Si superblock KO recuperer nb clusters AG depuis XAGF ou XAGI
  // Lire secondary superblock (cluster 0 + nb clusters AG)

	currentAG := 0;	// start with Allocation Group 0

  // Read sector 0 : superblock
  //Result := Self.ReadSector(XFS_SB_SECTOR_OFFSET);
	//if Result then
  //  	Result := Self.GetInfoFromBootSector(Self.fpSectorBuffer);
  // ATTN if primary superblock non avalaible, can't determine cluster size
  //

  // Read sector 1 : XAGF
  Result := Self.ReadSector(XFS_AGF_SECTOR_OFFSET);
  if Result then
  	Self.GetInfoFromAGF(Self.fpSectorBuffer);

    // Read sector 2 : XAGI
  Result := Self.ReadSector(XFS_AGI_SECTOR_OFFSET);
  if Result then
  	Self.GetInfoFromAGI(Self.fpSectorBuffer);

	// Go to secondary super block
  // #Todo try with different cluster size to find secondary super block
  Result := Self.ReadSector(Self.SectorPerCluster * Self.fpAGBlockSize);
  if Result then
  	Self.GetInfoFromSuperBlock(Self.fpSectorBuffer);

  // if no super block => how to determine block count ?
  // Alloue le buffer pour le cluster
  SetLength(Self.fpClusterBuffer, Self.fpClusterSize);

	// Debug
  Self.GetInodeList();

  Result := true;
end;


//
//
//#Todo Declare Filesystem name size in sb as constant
//#Todo Declare UUDI size in sb as constant
//#Todo Modify sb_fname retrieve => Pb with infos form
function TXFSPartition.GetInfoFromSuperBlock(buf: Pointer):boolean;
var
	psb : ^xfs_dsb;
begin
	psb := buf;

	Result := false;
	if SwapEndian32(psb.sb_magicnum) = XFS_SB_MAGIC then
  begin
  	SetString(Self.fpFSName, PAnsiChar(@psb.sb_fname[0]), 12); // gerer les noms de volume < 12
    // UUID

    // TODO : Voir si utilisation des constantes
    // SwapEndian32(sb_blocksize);
    // SwapEndian16(sb_sectsize);
    Self.fpClusterSize := SwapEndian32(psb.sb_blocksize);
    Self.fpSectorSize := SwapEndian16(psb.sb_sectsize);

    Self.fpAGCount := SwapEndian32(psb.sb_agcount);
    Self.fpInodeSize := SwapEndian16(psb.sb_inodesize);
    Self.fpBlockCount := SwapEndian64(psb.sb_dblocks);
    Self.fpFreeBlocks := SwapEndian64(psb.sb_fdblocks);
    Self.fpInodeCount := SwapEndian64(psb.sb_icount);

    Self.fpRootDirectory := SwapEndian64(psb.sb_rootino);
    Self.fpDirBlockSize := SwapEndian32(psb.sb_blocksize) Shl psb.sb_dirblklog;

    // sb_fdblocks
    Self.sb_inopblog := psb.sb_inopblog;
    Self.sb_inodelog := psb.sb_inodelog;
    Self.sb_agblklog := psb.sb_agblklog;
    // sb_inopblog + sb_agblklog => nombre de bits pour les relative inodes => mask + right shift
    // compute mask for relative inode
    // compute relative inode bit size
    
    Self.fpMaxValidClustNum := Self.fpBlockCount - 1; 
  	Result := true;
  end;
end;


//
// Retrieve info from AGF (Allocation Group Free space)
//
function TXFSPartition.GetInfoFromAGF(buf: Pointer):boolean;
var
	pagf: ^xfs_agf;
begin
	pagf := buf;

  if SwapEndian32(pagf.agf_magicnum) = XFS_AGF_MAGIC then
  begin
    Self.fpAGBlockSize := SwapEndian32(pagf.agf_length);
  	Result := true;
  end
  else
  	Result := false;
end;


//
// Retrieve info from AGI (Allocation Group Inode)
//
function TXFSPartition.GetInfoFromAGI(buf: Pointer):boolean;
var
	pagi: ^xfs_agi;
begin
	pagi := buf;

  if SwapEndian32(pagi.agi_magicnum) = XFS_AGI_MAGIC then
  begin
    Self.fpAGRootInode := SwapEndian32(pagi.agi_root);
    Self.fpAGInodeCount := SwapEndian32(pagi.agi_count);
  	Result := true;
  end
  else
  	Result := false;
end;


// ----------------------------------------------------------------------
//										Gestion des repertoires
// ----------------------------------------------------------------------


{* ----------------	Shortform --------------------

	+------------------------------------------+
  | di_core					_core xfs_dinode_core_t  |
  | 	di_format = XFS_DINODE_FMT_LOCAL (1)   |
  | 	di_nblocks = 0												 |
  | 	di nextents = 0											   |
  | di_next_unlinked											   |
  | di_u.di_dir2sf					 	xfs_dir2_sf_t  |
  | +--------------------------------------+ |
  | |hdr								 xfs_dir2_sf_hdr_t | |
  | |		count = number in list[]					 | |
  | |		parent = parent inode number			 | |
  | +--------------------------------------+ |
  | +--------------------------------------+ |
  | |list[]						 xfs_dir2_sf_entry_t | |
  | | +----------------------------------+ | |
  | | +		namelen/offset/name/inumber		 | | |
  | | +----------------------------------+ | |
  | +--------------------------------------+ |
  | +--------------------------------------+ |
  | |di_a.di_attrsf		xfs_attr_shortform_t | |
  | +--------------------------------------+ |

  Inode numbers are stored using 4 or 8 bytes
   i8count <> 0 => inode 8 bytes
   count <> 0  => inode 4 bytes

   WARNING If i8count and count = 0 => inode 4 bytes
 *}

type

xfs_dir2_inou  = packed record
  case Integer of
	  0:		// i8count <> 0
    	(i8: __be64);
	  1:		// count <> 0
    	(i4: __be32);
end;


xfs_dir2_sf_hdr = packed record
	count : __u8;            (* count of 4 bytes inode # entries *)
	i8count : __u8;          (* count of 8-byte inode #s *)
	parent : xfs_dir2_inou;  (* parent dir inode number *)
end;


xfs_dir2_sf_entry = record
	namelen : __u8;       (* actual name length *)
	offset : __be16; 			{* typedef struct xfs_uint8_t i[2];  xfs_dir2_sf_off_t; *}
	name : array of __u8;	{* name, variable size *}
      {*
       * A single byte containing the aFile aType field follows the inode
       * number for version 3 directory entries.
       *
	dwBitField: *;
       * variable offset after the name.
       *}
	inumber: xfs_dir2_inou;	{* Cardinal pour les short inodes can be 4 bytes *}
end;
__arch_packxfs_dir2_sf_entry_t = xfs_dir2_sf_entry;
{$EXTERNALSYM __arch_packxfs_dir2_sf_entry_t}


//
// pino points to inode_core in memory
//
procedure TXFSPartition.DecodeDirectoryShortForm(pino: Pointer);
var
	pbuf : Pointer;
  phdr : ^xfs_dir2_sf_hdr;
	psfentry : ^xfs_dir2_sf_entry;
  longinode : boolean;
  inodecount : Cardinal;
  i : Cardinal;
  inode : int64;
  parentinode : int64;
  name : string;
begin
  // go after inode core and xxx di_next_unlinked
  // #Todo2 Create inode size constant for V3 and V5
  pbuf := PChar(pino) + 100; 	// depends on FS version
  phdr := pbuf;

  if phdr.i8count <> 0 then
  begin
  	longinode := true;
    inodecount := phdr.i8count;
  end
  else
  begin
  	longinode := false;
    inodecount := phdr.count;
  end;

	// #Todo2 recup parent inode
	if longinode then
		parentinode := SwapEndian64(phdr.parent.i8)
	else
		parentinode := SwapEndian32(phdr.parent.i4);

	// aller sur debut liste xfs_dir2_sf_entry (+ sizeof(xfs_dir2_sf_hdr))
	// parcourir liste xfs_dir2_sf_entry et recuperer name and inode
	pbuf := Pchar(pbuf) + sizeof(xfs_dir2_sf_hdr);
	psfentry := Pointer(pbuf);
	for i := 0 to inodecount - 1 do
	begin
	  SetString(name, PAnsiChar(@psfentry.name[0]), psfentry.namelen);
    if longinode then
    	inode := SwapEndian64(psfentry.inumber.i8)
    else
    	inode := SwapEndian32(psfentry.inumber.i4);

		 // next extry #Todo 1 Check offset math
    Inc(psfentry, psfentry.offset * 8);
	end;
end;


{* -------------- Block directory ----------------

	+------------------------------------------+
  | hdr							    xfs_dir2_data_hdr_t  |
  | +--------------------------------------+ |
  | |bestfree[3]			xfs_dir2_data_free_t | |
  | +--------------------------------------+ |
  | +--------------------------------------+ |
  | |u[]						 xfs_dir2_data_union_t | |
  | +--------------------------------------+ |
  | +--------------------------------------+ |
  | |leaf[]					 xfs_dir2_leaf_entry_t | |
  | +--------------------------------------+ |
  | +--------------------------------------+ |
  | |tail 				   xfs_dir2_block_tail_t | |
  | +--------------------------------------+ |
	+------------------------------------------+

 *}

// magic
const
XFS_DIR2_BLOCK_MAGIC = $58443242; {* 'XD2B' v3 *}
//const XFS_DIR2_BLOCK_MAGIC = $58444233; {* 'XDB3' v5 *}
XFS_DIR2_DATA_FREE_TAG = $ffff;

type
//typedef __uint16_t xfs_dir2_data_off_t;
//typedef __uint32_t xfs_dir2_dataptr_t;

{*
 * Describe a free area in the data block.
 * The freespace will be formatted as a xfs_dir2_data_unused_t.
 *}
xfs_dir2_data_free = record
	offset : __be16;		{* start of freespace *}
	length : __be16;		{* ength of freespace *}
end;

{*
 * Header for the data blocks.
 * Always at the beginning of a directory-sized block.
 * The code knows that XFS_DIR2_DATA_FD_COUNT is 3.
 *}
xfs_dir2_data_hdr = record
	magic : __be32;		{* XFS_DIR2_DATA_MAGIC or XFS_DIR2_BLOCK_MAGIC *}
  bestfree : array[0..2] of xfs_dir2_data_free;	{* XFS_DIR2_DATA_FD_COUNT is 3 *}
end;

{*
 * Active entry in a data block.  Aligned to 8 bytes.
 * Tag appears as the last 2 bytes.
 *}
xfs_dir2_data_entry = packed record
	inumber : __be64;		{* inode number *}
  namelen : __u8;			{* name length *}
  name : array[0..0] of __u8; {* name bytes, no null *}
  {* variable offset *}
  tag : __be16;        {* starting offset of us *}
end;

{*
 * Unused entry in a data block.  Aligned to 8 bytes.
 * Tag appears as the last 2 bytes.
 *}
xfs_dir2_data_unused = packed record
	freetag : __be16;		{* 0xffff XFS_DIR2_DATA_FREE_TAG *}
  length : __be16;   {* total free length *}
  {* variable offset *}
  tag : __be16;        {* starting offset of us *}
end;

xfs_dir2_data_union = packed record
	case Integer of
  	0 :
    	(entry : xfs_dir2_data_entry; );
    1 :
    	(unused : xfs_dir2_data_unused;);
end;


//
// buf points on directory block data in memory (xfs_dir2_data_hdr)
//
procedure TXFSPartition.DecodeDirectoryBlock(pbuf : Pointer);
var
	inode : Int64;
  name : string;
  phdr : ^xfs_dir2_data_hdr;
  pdu : ^xfs_dir2_data_union;
  offset : Cardinal;
begin
  phdr := pbuf;
  if phdr.magic = SwapEndian32(XFS_DIR2_BLOCK_MAGIC) then
  begin
		pbuf := PChar(pbuf) + sizeof(xfs_dir2_data_hdr);
	  pdu := pbuf;

    // parse entry data till unused entry
    while pdu.unused.freetag <> XFS_DIR2_DATA_FREE_TAG do
    begin
    	// WARNING In debugging mode, inode value is not correctly showned
      inode := SwapEndian64(pdu.entry.inumber);
      SetString(name, PAnsiChar(@pdu.entry.name[0]), pdu.entry.namelen);

      // compute size of data entry (8 bytes aligned)
      offset := (sizeof(xfs_dir2_data_entry) + pdu.entry.namelen - 1 + 7) and $FFFFFFF8;

      // go to the next data entry
      Inc(Pchar(pbuf), offset);
	    pdu := pbuf;
    end;
  end;
end;


// ----------------------------------------------------------------------
//						Gestion des fichiers et repertoires
// ----------------------------------------------------------------------


//
// Compute
//
//procedure TXFSPartition.GetRelativeInode(const absinode: int64, var AGNumber, relinode: Cardinal);
//begin
//end;

(*
 * Liste des inodes
 *
 * AGI
 * agi_root pointe sur un block commençant par xfs_inobt_block_t
 * lire le block agi_root pour chaque AG
 *
 * bb_level = 0 (leaf)
 * -------------------
 *
 * array of bb_numrecs xfs_inobt_rec_t
 * xfs_inobt_rec_t[i] contient inode
 * ir_free : bit à 1 indique un inode libre (non/plus utilisé)
 *
 * bb_level > 0 (node)
 * -------------------
 *
 *
 *
 *)


type
	xfs_btree_sblock = record
		bb_magic : __be32;         (* magic number for block type *)
		bb_level : __be16;         (* 0 is a leaf *)
		bb_numrecs : __be16;       (* current # of data records *)
		bb_leftsib : __be32;       (* left sibling block or NULLAGBLOCK *)
		bb_rightsib : __be32;      (* right sibling block or NULLAGBLOCK *)
	end;
	xfs_btree_sblock_t = xfs_btree_sblock;
	{$EXTERNALSYM xfs_btree_sblock_t}


(* btree block header type *)
type
	xfs_inobt_block_t = xfs_btree_sblock;

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

type
	xfs_inobt_rec = record
		ir_startino : __be32;     (* starting inode number *)
		ir_freecount : __be32;   (* count of free inodes *)
    ir_free    :    __be64;   (* free inode mask *)
 end;


// ----------------------------------------------------------------------
//						Gestion des Data Extents List
// ----------------------------------------------------------------------


// bit for Data Extents
const BMBT_EXNTFLAG_BITLEN = 1;
const BMBT_STARTOFF_BITLEN = 54;
const BMBT_STARTBLOCK_BITLEN = 52;
const BMBT_BLOCKCOUNT_BITLEN = 21;

// associated bit mask
const BLOCKCOUNT_MASK = $0001fffff;
const START_MASK_L1 = $FFFFFFFFFFE00000;
const START_MASK_L0 = $000001ff;
const BLOCKOFFSET_MASK = $7ffffffffffffe00;

type
xfs_bmbt_rec = record
        l0 : __be64;
        l1 : __be64;
end;
Pxfs_bmbt_rec = ^xfs_bmbt_rec;


//
// Decode on disk data extents structure
//
{-----------------------------------------------------------------------------
  Procedure: TXFSPartition.DecodeDataExtents
  Author:    Chris
  Date:      2016/08/21
  Arguments: [In] pde pointer to Pxfs_bmbt_rec (data extents structure - 128 bits)
             [Out] dde structure for decoded Data Extent 
  Result:    absolute cluster number (not AG dependant)        
-----------------------------------------------------------------------------}
procedure TXFSPartition.DecodeDataExtents(pde: Pointer; var dde: TDataExtents);
var
	bel0,bel1: int64;
begin
	// swap les donnees en Big Endian
  bel0 := SwapEndian64(Pxfs_bmbt_rec(pde).l0);
	bel1 := SwapEndian64(Pxfs_bmbt_rec(pde).l1);

  //
  dde.deStartOffset := (bel0 and BLOCKOFFSET_MASK) shr 9;

  //
  dde.deStartBlock := ((bel1 and START_MASK_L1) Shr BMBT_BLOCKCOUNT_BITLEN) or ((bel0 and START_MASK_L0) shl (43 - 9));
  // Convertion to Absolute Cluster number
  dde.deStartBlock := Self.AbsBlockNumberToCluster(dde.deStartBlock);

  //
  dde.deBlockcount := Cardinal (bel1 and BLOCKCOUNT_MASK);

  // #Todo check for flag values
//  if (bel0 and $8000000000000000) = 0 then
//  	dde.br_state := 0
//  else
//  	dde.br_state := 1;
end;


//
// Converts block number (relative to AG) to absolute cluster number
//
{-----------------------------------------------------------------------------
  Procedure: TXFSPartition.AbsBlockNumberToCluster
  Author:    Chris
  Date:      2016/08/21
  Arguments: bn block number (contains AG number)
  Result:    absolute cluster number (not AG dependant)        
-----------------------------------------------------------------------------}
function TXFSPartition.AbsBlockNumberToCluster(bn: int64): int64;
var
	relblockmask: int64; 
begin
	relblockmask := 1 shl Self.sb_agblklog;
  Result := (bn div relblockmask) * Self.fpAGBlockSize + (bn mod relblockmask);
end;


// http://lxr.free-electrons.com/source/fs/xfs/xfs_bmap_btree.h?v=2.6.24
const XFS_BMAP_MAGIC = $424d4150;	(* 'BMAP' *)

type
xfs_btree_lblock = record
		bb_magic : __be32;         (* magic number for block type *)
		bb_level : __be16;         (* 0 is a leaf *)
		bb_numrecs : __be16;       (* current # of data records *)
		bb_leftsib : __be64;       (* left sibling block or NULLAGBLOCK (-1) *)
		bb_rightsib : __be64;      (* right sibling block or NULLAGBLOCK (-1) *)
end;
// xfs_btree_lblock_t;

(*
 * Bmap root header, on-disk form only.
 *)
xfs_bmdr_block = record
	bb_level :   __be16;       (* 0 is a leaf *)
  bb_numrecs : __be16;       (* current # of data records *)
end;


type
	// Extended inode_code structure for easy access to informations
	xfs_dinode_ext = packed record
  	di_core: xfs_dinode_core;
    di_next_unlinked: __be32;
    //
    case Integer of
    0: // Data Extents part XFS_DINODE_FMT_EXTENTS
    	(
        data_ext: array [0..0] of xfs_bmbt_rec;
    	);
    1: // B+Tree Extents
    	(
    		hdr: xfs_bmdr_block;
        case Integer of
        0: ( bb_keys: array [0..0] of int64; );  // for access to file offset
        1: ( bb_ptrs: array [0..0] of int64; );  // for access BAMP block number.
        // CAUTION bb_ptrs is starting at the middle of remaining inode space
      );
  end;

          // File XFS_DINODE_FMT_BTREE
        // ATTENTION bb_keys[bb_numrecs] et bb_ptrs[bb_numrecs] ne sont pas contigus
        // l'espace disponibles entre et du_a est divisé en 2
        // mais repartis dans 


//
//  Returns a list with all data extents of a file
//   inodebuf : pointer on ondisk inode
function TXFSPartition.GetFileDataExtents(inodebuf: Pointer): TList;
var
	pino: ^xfs_dinode_ext;
  pde: PDataExtents;
  i: integer;
  bbptroffset : integer;
begin
  pino := inodebuf;

	// size > 0 and nb extents > 0
  if (SwapEndian64(pino.di_core.di_size) > 0) and (SwapEndian32(pino.di_core.di_nextents) > 0) then
  begin
  	Result := Tlist.Create;
  	if pino.di_core.di_format = XFS_DINODE_FMT_EXTENTS then
    begin
    	// Data Extents in inode core
      for i := 0 to SwapEndian32(pino.di_core.di_nextents) - 1 do
      begin
      	New(pde);
        DecodeDataExtents(@pino.data_ext[i], pde^);
        Result.Add(pde);
      end
    end
    else
    begin
    	// XFS_DINODE_FMT_BTREE
      // Computer bb_ptrs offset multiple de 16 (8 * 2) => and $fff0
      bbptroffset := (((pino.di_core.di_forkoff * 8) - sizeof(xfs_bmdr_block)) and $fff0) div 16;

      // recuperer le pointer vers le BMAP et recup du BTree
      for i := bbptroffset to bbptroffset + SwapEndian16(pino.hdr.bb_numrecs) - 1 do
      	GetBTreeDataExtents(Result, Self.AbsBlockNumberToCluster(SwapEndian64(pino.bb_ptrs[i])));
    end;
  end
  else
  	Result := nil;
end;


//
type
	// 
	xfs_bmap_ext = record
  	hdr: xfs_btree_lblock;
    Case Integer of
    0: // Node
    	 (
       		Case Integer of
          0: ( bb_keys: array[0..0] of int64);
          1: ( bb_ptrs: array[0..0] of int64);
       );
    1: // Leaf
    	 (
           date_ext: array[0..0] of xfs_bmbt_rec;
       );
  end;


//
//
//
procedure TXFSPartition.GetBTreeDataExtents(del: TList; nodecluster: Cardinal);
var
  i: integer;
	pbmap: ^xfs_bmap_ext;
  pde: PDataExtents;
  bb_ptroffset: integer;
  BMapClusterBuffer : array of Byte;// Cache pour le cluster
begin
	// Allocate buffer for BMAP cluster
  SetLength(BMapClusterBuffer, Self.ClusterSize);

	// Lire le node cluster  // xfs_btree_sblock
  Self.ReadClusterToBuffer(nodecluster, @BMapClusterBuffer[0], caFillInvalid);
   pbmap := @BMapClusterBuffer[0];

  // vérifier XFS_BMAP_MAGIC
  if SwapEndian32(pbmap.hdr.bb_magic) = XFS_BMAP_MAGIC then
  begin
    // Leaf or Node
  	if SwapEndian16(pbmap.hdr.bb_level) > 0 then
    begin
    	// Node
    	// compute bb_ptr offset in cluster All ptr are 64 bits (8 bytes) => must be a multiple of 16
      bb_ptroffset := (Self.ClusterSize - SizeOf(xfs_btree_lblock)) and $fff0 div 16;

      //
      for i:= bb_ptroffset to bb_ptroffset + SwapEndian16(pbmap.hdr.bb_numrecs) - 1 do
      	GetBTreeDataExtents(del, Self.AbsBlockNumberToCluster(SwapEndian64(pbmap.bb_ptrs[i])));;
    end
    else
    begin
    	// Leaf
      // Array of Data Extents starting after header extents[bb_numrecs]
      for i := 0 to SwapEndian16(pbmap.hdr.bb_numrecs) - 1 do
      begin
      	// Allocate a new DataExtents add to list (at the end)
        New(pde);
        del.Add(pde);
        
        // Decode DataExtents
        DecodeDataExtents(@pbmap.date_ext[i], pde^);
      end;
    end;
  end;
end;



//DEBUG
procedure dummyinode(const startino: __be32);
begin

end;

procedure TXFSPartition.InodeToCluster(const ino: int64; pino_rec : PClusterInode);
var
	AGnum: Cardinal;
begin
	// calculer l'AG num de l'inode et l'inode relatif
  AGnum := 0;

	//
	pino_rec.clusterNum := (ino * Self.fpInodeSize) div Self.ClusterSize;
	pino_rec.bufferOffset := (ino * Self.fpInodeSize) mod Self.ClusterSize
end;


//
// Retrieve Inode list
// only level 0 is supported
procedure TXFSPartition.GetInodeList();
var
	piabt: ^xfs_inobt_block_t;
  pinorecar: array of xfs_inobt_rec;
  i,j : integer;
	// DEBUG
  ClustersBuffer: Array[0..CLUSTERS_BUFFER_SIZE-1] of Byte;
begin
	// DEBUG WARNING A SUPPRIMER ???
  Self.fpPhysicalDrive.ReadNTSectors(63 + 8*8, (32 * Self.ClusterSize) div Self.SectorSize, @ClustersBuffer[0]);

	// boucler sur tous les AG
  // read agi_root
	if Self.ReadCluster(self.fpAGRootInode) = Self.GetSectorPerCluster then
	begin
      // pino = first inode in cluster
		  // bb_leftsib/bb_rightsib should be NULLABSxxx
      piabt := @Self.fpClusterBuffer[0];
      if SwapEndian32(piabt.bb_magic) = XFS_IBT_MAGIC then
      	if SwapEndian16(piabt.bb_level) = 0 then
	      begin
        	// parcours les xfs_inobt_rec_t[]
          pinorecar := @Self.fpClusterBuffer[sizeof(xfs_inobt_block_t)];
        	for i := 0 to SwapEndian16(piabt.bb_numrecs) - 1 do
          begin
          	for j := 0 to 63 do
	            if (SwapEndian64(pinorecar[i].ir_free) and (int64(1) shl j)) = 0 then
	            begin
              	// calculer le numero inode à ajouter en fonction du bit
                //
                dummyinode(SwapEndian32(pinorecar[i].ir_startino) + j * 256);   // DEBUG
	            end;
          end;
	      end;
	end
end;


//
// Scan directory from root
//
function TXFSPartition.ScanRootDirectory(): boolean;
var
  pino : ^xfs_dinode_core;
  size: int64;
  i, j : integer;
  offset : Cardinal;
  xfsf: TXFSFileInfo;
  del: TList;
  abortscan: boolean;
	ClusterBuffer : array of Byte;// Cache pour le cluster
begin
	// CAUTION Partition buffer =>
	SetLength(ClusterBuffer, Self.ClusterSize);

  // DEBUG DecodeDirectoryBlock
  if Self.ReadClusterToBuffer(653818, @ClusterBuffer[0], caFillInvalid) = 8 then
  	Self.DecodeDirectoryBlock(@ClusterBuffer[0]);

	// read all clusters
  for i := 0 to 3 do
  begin
	  offset := 0;

	  //
    if Self.ReadClusterToBuffer(8+i, @ClusterBuffer[0], caFillInvalid) = 8 then
  	begin
  	  // 16 inodes par cluster
      for j := 0 to 15 do
	    begin
  	    // pino = first inode in cluster
	      pino := @ClusterBuffer[offset];
  	    if XFSIsFileOrDirInode(@ClusterBuffer[offset]) then
    	  begin
        	// TODO add di_format for the type of data support 20201113
      	  if (SwapEndian16(pino.di_mode) and S_IFMT) = S_IFREG then
	        begin
  	        del := GetFileDataExtents(@ClusterBuffer[offset]);

	  	    	// Create
  	  	    xfsf := TXFSFileInfo.Create(128 + i*16 + j, @ClusterBuffer[offset], del);
	        end
  	      else
	  	      xfsf := TXFSFileInfo.Create(128 + i*16 + j, @ClusterBuffer[offset]);

		      // Found a file.
  		    if assigned(Self.OnFile) then
    	      Self.OnFile(Self, xfsf, abortscan)
    		  else
      			xfsf.Free;

	        // Add to TXFSFileInfo list
  	      // .Add(xfsf)
    	    //xfsf.Free;
	      end;

  	    // next inode
    	  offset := offset + 256;
	    end;	// for j
	  end; // if
  end; // for i

	ClusterBuffer := nil;
	Result := true;
end;


//
//
//
function TXFSPartition.ScanRelativeDirectory(const dirid: int64): boolean;
begin
  // not implemented
end;

(*
    di_mode and S_IFMT = S_IFDIR
    ----------------------------
      di_format = XFS_DINODE_FMT_LOCAL
      --------------------------------
        shortform
        xfs_dir2_sf_hdr (inode can be 4 ou 8 bytes)
        list of xfs_dir2_sf_entry

      di_format = XFS_DINODE_FMT_EXTENTS
      ----------------------------------
        inode contains a pointer of a directory block (1 dataextent) di_u.u_bmx[0]
        directory block size = ?
        directory block starts with XD2B

      di_format = XFS_DINODE_FMT_BTREE
      --------------------------------
*)




//
//  buf = pointer to the inode core structure
//
procedure TXFSPartition.ParseDirectoryInode(buf: Pointer);
var
	pino : ^xfs_dinode_core;
  phdr: ^xfs_dir2_sf_hdr;
  psfentry : ^xfs_dir2_sf_entry;
  pbuf: Pchar;
	pdirext: Pxfs_bmbt_rec;
  //dir_extent: xfs_bmbt_irec;
  inodecount : Cardinal;
  longinode : boolean;
  i: integer;
  name : string;
begin
	pino := buf;

	// go after inode core and xxx di_next_unlinked
  pbuf := buf;
  Inc(pbuf,100);	// depends on FS version

	case pino.di_format of
		XFS_DINODE_FMT_LOCAL:
    	begin
				phdr := Pointer(pbuf);

        if phdr.i8count <> 0 then
        	begin
          	inodecount := phdr.i8count;
            longinode := true;
        	end
	      else
        	begin
          	inodecount := phdr.count;
            longinode := false;
        	end;

        // aller sur debut liste xfs_dir2_sf_entry (+ sizeof(xfs_dir2_sf_hdr))
        // parcourir liste xfs_dir2_sf_entry et recuperer name and inode
        pbuf := pbuf + sizeof(xfs_dir2_sf_hdr);
        psfentry := Pointer(pbuf);
        for i := 0 to inodecount - 1 do
        begin
          SetString(name, PAnsiChar(@psfentry.name[0]), psfentry.namelen);
          // retreive inode number
//          if longinode then
//          else
//          end
          Inc(psfentry, psfentry.offset * 8); //#Todo 1 Check offset math
        end;

        // 
      end;
    XFS_DINODE_FMT_EXTENTS:
    	begin
      	// di_nextents = 1
        // di_u.di_bmx[0]
        // xfs_bmbt_rec_32_t
        // buf + 100 contient xfs_bmbt_rec_32 (128 bits pour l'extents)
        // qui contient le pointeur sur le directory block
        pdirext := Pointer(pbuf);
        //DecodeDataExtents(pdirext, dir_extent);
        //Self.fpBlockCount := dir_extent.br_startblock;
      end;
    XFS_DINODE_FMT_BTREE:;
    else;
  end;
end;


// 20201107
// Optimised RestoreFile with large buffer
//
// TEST FUNCTION
function TXFSPartition.RestoreFile(finfo: TCommonFileInfo; stream: TStream): boolean;
var
	bigbuffer: array of Byte;
	i: Cardinal;
  blocksize, writesize : int64;
  deremaningblocks : Cardinal;	// nombres de clusters restants
  clusterstoread, clusternum : Cardinal;
begin
	// Init
  writesize := 0;
  Result := true;

	// allocate buffer @bigbuffer[0] adress of allocated memory
  SetLength(bigbuffer, BIGBUFFER_SIZE);

  // Parcourir tous les DataExtents
  for i := 0 to TXFSFileInfo(finfo).DECount - 1 do
  begin
    deremaningblocks := TXFSFileInfo(finfo).DEBlockCount[i];
    clusternum := TXFSFileInfo(finfo).DEStartBlock[i];

    // read clusters to buffer in multiple passes if needed
    repeat
    begin
    	// calc clusters to read to buffer
      if (deremaningblocks * Self.ClusterSize) > BIGBUFFER_SIZE then
      	clusterstoread := BIGBUFFER_SIZE div Self.ClusterSize
      else
				clusterstoread := deremaningblocks;

      // Read clusters TODO ADD TEST
      if Self.ReadClustersToBuffer(clusternum, clusterstoread, @bigbuffer[0]) <> clusterstoread * Self.SectorPerCluster then
        begin
            // Gerer les erreurs : Continuer, stopper DISK_READ_ERROR
            //if Assigned(OnScanError) then
            //	Self.OnScanError(Self, error, action);
            // Action = STOP, CONCAT (modif du ReadCluster), USE_PATTERN

            //if (action = ??) then
            Result := false;
        	break;
        end;

      // taille à écrire = clusters lus
      blocksize := clusterstoread * Self.ClusterSize;

      // Vérif fin de fichier
			if writesize + blocksize >= TXFSFileInfo(finfo).Size then
      	blocksize := TXFSFileInfo(finfo).Size - writesize;	// NE doit pas passer qu'une fois

			// on essaie d'ecrire dans le stream
      try
      	stream.WriteBuffer(bigbuffer[0], blocksize);
			except
				on EWriteError do begin
             // Pas d'action, on sort de la procedure
             // mettre un retour d'erreur Erreur fichier FILE_WRITE_ERROR
             Result := false;
             break;
        end;
			end; // try

      // update remaining clusters
      deremaningblocks := deremaningblocks - clusterstoread;
      clusternum := clusternum + clusterstoread;
      writesize := writesize + blocksize;

      if Assigned(OnRestore) then
      	Self.OnRestore(self, TXFSFileInfo(finfo).Size, int64(writesize));
    end;
    until deremaningblocks <= 0;
	end;	// for

  // deallocate memory
  bigbuffer := nil;
end;


//
// Restore a file from its DataExtents
//
{function TXFSPartition.RestoreFileOld(finfo: TCommonFileInfo; stream: TStream): boolean;
var
	i, j: Cardinal;
  blocksize, writesize : int64;
  lastcluster: int64;
begin
	// Init
  writesize := 0;
  blocksize := Self.ClusterSize;
  Result := true;

  // dernier cluster = start + block count du dernier extents
  lastcluster := TXFSFileInfo(finfo).DEStartBlock[TXFSFileInfo(finfo).DECount - 1] + TXFSFileInfo(finfo).DEBlockCount[TXFSFileInfo(finfo).DECount - 1] - 1;

	// Parcourir les DataExtents
  for i := 0 to TXFSFileInfo(finfo).DECount - 1 do
  	for j := TXFSFileInfo(finfo).DEStartBlock[i] to TXFSFileInfo(finfo).DEStartBlock[i] + TXFSFileInfo(finfo).DEBlockCount[i] - 1 do
    begin
    	// j contains cluster number to read
    	if Self.ReadCluster(j) <> (Self.ClusterSize div Self.SectorSize) then
        begin
            // Gerer les erreurs : Continuer, stopper DISK_READ_ERROR
            //if Assigned(OnScanError) then
            //	Self.OnScanError(Self, error, action);
            // Action = STOP, CONCAT (modif du ReadCluster), USE_PATTERN

            //if (action = ??) then
            Result := false;
        	break;
        end;

        // gestion du dernier cluster, ecrire que la partie utile
        if j = lastcluster then
        	blocksize := TXFSFileInfo(finfo).Size mod Self.ClusterSize;

        // en essaie d'ecrire dans le stream
        try
            stream.WriteBuffer(Self.fpClusterBuffer[0], blocksize);
        except
					on EWriteError do begin
                // Pas d'action, on sort de la procedure
                // mettre un retour d'erreur Erreur fichier FILE_WRITE_ERROR
                Result := false;
                break;
            end;
        end; // try

        // Mettre un appel OnProgress ? (sauvegarder le nb octets écris)
        writesize := writesize + blocksize;
        if Assigned(OnRestore) then
        	Self.OnRestore(self, TXFSFileInfo(finfo).Size, int64(writesize));
    end; // for
end;}


// ----------------------------------------------------------------------
//						Gestion des propriétés
// ----------------------------------------------------------------------


//
// Renvoie le nom du File System (type de file system)
//
function TXFSPartition.GetFileSystemName : String;
begin
  Result := 'XFS';
end;


//
// Renseigne le Tstrings avec les infos de la partition XFS
//
procedure TXFSPartition.GetPartitionProperties(info: TStrings);
var
    chs : TCHS;
begin
    inherited;
    Self.fpPhysicalDrive.LBAToCHS(chs, Self.FirstSecOfPart);

    // Verifier si ces info sont disponibles (PartInfo)
    if (Self.FInfoFlag and PART_INFO_AVAIL_FLAG) <> 0 then
    begin
        info.Add(PARTITIONTYPE_STR);
        info.Add(IntToHex(Self.PartitionType, 2));

        info.Add(BOOTABLE_STR);
        if Self.ActivePart then
            info.Add('Yes')
        else
            info.Add('No');

        info.Add(PARTITIONSTATUS_STR);
        if Self.PrimaryPartition then
            info.Add(PRIMARY_STR)
        else
            info.Add(LOGICAL_STR);
    end;

    info.Add(PARTSTARTAT_STR);
    info.Add(Format('%d-%d-%d (CHS)',[chs.Cylinder, chs.Head, chs.Sector]));
    info.Add('');
    info.Add(Format('%d (LBA)',[Self.FirstSecOfPart]));
    info.Add(PARTITIONSIZE_STR);
    info.Add(SizeTo2DigitByteString(int64(Self.TotalSectors) * int64(Self.SectorSize)));
    info.Add(TOTALSECTORS_STR);
    info.Add(IntToStr(Self.TotalSectors));

		info.Add('File System');
		info.Add(Self.FileSysName);
    info.Add(SECTORSIZE_STR);
    info.Add(SizeToRoundedByteString(Self.SectorSize));
    info.Add(CLUSTERSIZE_STR);
    info.Add(SizeToRoundedByteString(Self.ClusterSize));
    //info.Add('VolumeName');
    //info.Add(self.fpFSName);
    info.Add('FS Block count');
    info.Add(IntToStr(Self.fpBlockCount));
    info.Add('FS Free Blocks count');
    info.Add(IntToStr(Self.fpFreeBlocks));
    info.Add('Allocated inodes');
    info.Add(IntToStr(Self.fpInodeCount));
    info.Add('Inode size');
    info.Add(IntToStr(Self.fpInodeSize));
    info.Add('Root directoty inode');
    info.Add(IntToStr(self.fpRootDirectory));
    info.Add('Directoty block size');
    info.Add(IntToStr(self.DirectoryBlockSize));

    // infos AG
    info.Add('AG count');
    info.Add(IntToStr(self.AGCount));
    // loop on AGInformations infos speciques aux AG
    info.Add('AG size in block');
    info.Add(IntToStr(self.fpAGBlockSize));
    info.Add('AG root inode block');
    info.Add(IntToStr(self.fpAGRootInode ));
    info.Add('AG Allocated inodes');
    info.Add(IntToStr(self.fpAGInodeCount));
end;

end.
{
Lire les clusters
Boucler sur la liste des inodes
Si Inode valide
                afficher les infos de l'inode di_core
                afficher les infos di_u
                afficher les infos di_a
Finsi

// ReadSectorBuffer
// Lire les secteurs de 63 à  200
// lire 1er int32
// verifier si magic number
// si oui recuperer les données et dump vers console

//2267500
// starting sector = 63
// #secteurs 312576640
// cluster 4KB (8 secteurs)
// #clusters 39072080 0x2543150

// 18140063 INODE
// Indodes
// secteur 127 à 158 => cluster 8 à 11 => 64 inodes
// secteur 159 : fichier MP4 ?

// AG list (4 de 37GB each)
// sector        63 => cluster 0
// sector  78144223 => cluster 9768020 (0x950C54)
// sector 156288383 => cluster
// sector 234432543 => cluster

(*
AG 0
Root ino 2267500 => sector 18140063
AG 1
Root ino 9589075 => sector 154856823
AG 2
Root ino 4573056 => sector 192872831
AG 3
Root ino 1099324 => sector 243227135
 *)

//http://pages.cs.wisc.edu/~vshree/xfs.pdf

inode => cluster and buffer offset 
}

