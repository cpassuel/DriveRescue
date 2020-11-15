unit XFSCommon;

interface


(*
 * Additional aType declarations for XFS
 *)

  // definition for big endian data type (on disk)
type
	__be16 = Word;
	__be32 = Cardinal;
	__be64 = Int64;
  __u8   = Byte;
  __s8   = ShortInt;


  (*
 * Disk inode structure.
 * This is just the header; the inode is expanded to fill a variable size
 * with the last field expanding.  It is split into the core and 'other'
 * because we only need the core part in the in-core inode.
 *)
type
	xfs_timestamp = record
		t_sec  : __be32;            (* timestamp seconds since 01/01/1970 *)
		t_nsec : __be32;            (* timestamp nanoseconds *)
	end;
  xfs_timestamp_t = xfs_timestamp;

(*
    ===================================================================
    INODE structure on disk
     _______________________________
    |   xfs_dinode_core (96 bytes)  |
    |_______________________________|
    |   di_next_unlinked (4 bytes)  |
    |_______________________________|
    |       di_u                    |
    |_______________________________|
    |       di_a                    |
    |_______________________________|

    di_mode and S_IFMT = S_IFDIR
    ----------------------------
      di_format = XFS_DINODE_FMT_LOCAL
      --------------------------------
        shortform
        array of

      di_format = XFS_DINODE_FMT_EXTENTS
      ----------------------------------
        inode contains a pointer of a directory block (1 dataextent)
        directory block size = ?
        directory block starts with XD2B

      di_format = XFS_DINODE_FMT_BTREE
      --------------------------------

    di_mode and S_IFMT = S_IFREG
    ----------------------------
      di_format = XFS_DINODE_FMT_EXTENTS
      ----------------------------------
	     _______________________________
  	  |   xfs_dinode_core (96 bytes)  |
    	|_______________________________|
	    |   di_next_unlinked (4 bytes)  |
  	  |_______________________________|
	    | di_u.di_bmx[0..di_nextents-1] |
  	  | of xfs_bmbt_rec_32_t          |
  	  |_______________________________|

      di_format = XFS_DINODE_FMT_BTREE
      --------------------------------

         sb_inopblog + sb_agblklog
*)

// http://lxr.free-electrons.com/source/fs/xfs/xfs_dinode.h?v=2.6.24
const
XFS_DINODE_MAGIC = $494e;	(* 'IN' *)

// offset to the data fork du_i from incore start
XFS_INODE_CORE_DF_OFFSET = 100;
XFS_INODE_CORE_DF_OFFSET_V5 = 180;


(*
 * below, the offsets table in xfs_ialloc_log_di() and  xfs_icdinode
 * in xfs_inode.h.
 *)
type
	xfs_dinode_core = packed record
		di_magic     : __be16;              (* inode magic # = XFS_DINODE_MAGIC *)
		di_mode      : __be16;              (* mode and type of file S_Ixxx values defined in stat.h. *)
		di_version   : __u8;                (* inode version *)
		di_format    : __u8;                (* format of di_c data *)
		di_onlink    : __be16;              (* old number of links to file *)
		di_uid       : __be32;              (* owner's user id *)
		di_gid       : __be32;              (* owner's group id *)
		di_nlink     : __be32;              (* number of links to file *)
		di_projid    : __be16;              (* owner's project id *)
    di_pad       : array[0..7] of char; (* unused, zeroed space *)
		di_flushiter : __be16;              (* incremented on flush *)
		di_atime     : xfs_timestamp_t;     (* time last accessed *)
		di_mtime     : xfs_timestamp_t;     (* time last modified *)
		di_ctime     : xfs_timestamp_t;     (* time created/inode modified *)
		di_size      : __be64;              (* number of bytes in file *)
		di_nblocks   : __be64;              (* # of direct & btree blocks used *)
		di_extsize   : __be32;              (* basic/minimum extent size for file *)
		di_nextents  : __be32;              (* number of extents in data fork *)
		di_anextents : __be16;              (* number of extents in attribute fork*)
		di_forkoff   : __u8;                (* attr fork offs, <<3 for 64b align *)
		di_aformat   : __s8;                (* format of attr fork's data (di_format LOCAL EXTENTS BTREE) *)
		di_dmevmask  : __be32;              (* DMIG event mask *)
		di_dmstate   : __be16;              (* DMIG state info *)
		di_flags     : __be16;              (* random flags, XFS_DIFLAG_... *)
		di_gen       : __be32;              (* generation number *)
		//di_next_unlinked :__be32;						{* di_next_unlinked is the only non-core field in the old dinode *}
    //  version 5 filesystem (inode version 3) fields start here
	end;
(*
 * di_forkoff = 0 if no attributes else di_forkoff * 8 = offset 
 *)

(*
 * Values for xfs_dinode_core.di_format (should be enum)
 *)
const XFS_DINODE_FMT_DEV  = 0;             (* xfs_dev_t *) (* CHR, BLK: di_dev *)
const XFS_DINODE_FMT_LOCAL = 1;            (* bulk data *) (* DIR, REG: di_c */ /* LNK: di_symlink *)
const XFS_DINODE_FMT_EXTENTS = 2;          (* struct xfs_bmbt_rec *) (* DIR, REG, LNK: di_bmx *)
const XFS_DINODE_FMT_BTREE = 3;            (* struct xfs_bmdr_block *) (* DIR, REG, LNK: di_bmbt *)
const XFS_DINODE_FMT_UUID = 4;             (* uuid_t *) (* MNT: di_uuid *)


type

// Data Extents decoded
TDataExtents = record
  deStartOffset : int64;    (* starting file offset *)
  deStartBlock : int64;     (* Cluster block number *)
  deBlockcount : Cardinal;  (* number of clusters *)
end;
PDataExtents = ^TDataExtents;


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation

end.
