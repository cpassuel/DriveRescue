{-----------------------------------------------------------------------------
 Unit Name: LinuxPartition
 Author:    Chris
 Purpose:   Classe abstraite pour gerer les partitions Linux
 History:   12/07/2016
-----------------------------------------------------------------------------}
unit LinuxPartition;


interface


uses
  GenericPartition;


const

// http://pubs.opengroup.org/onlinepubs/007908799/xsh/sysstat.h.html
// define type of linux file
S_IFMT = $F000;  // type of file (mask) 00170000
S_IFSOCK = $C000; // 0140000
S_IFLNK = $A000; // symbolic link 0120000
S_IFREG = $8000; // regular 0100000
S_IFBLK = $6000; // block special 0060000
S_IFDIR = $4000; // directory 0040000
S_IFCHR = $2000; // character special 0020000
S_IFIFO = $1000; // FIFO special 0010000
S_ISUID = $0800; // 0004000
S_ISGID = $0400; // 0002000
S_ISVTX = $0200; // 0001000


type

//
// Classe générique pour les partitions Linux
//
TLinuxPartition = class(TGenericPartition)
    private
    protected
    public
end;


// =====================================================================
//	   						IMPLEMENTATION
// =====================================================================
implementation

end.
