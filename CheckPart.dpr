program CheckPart;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  Windows,
  SysUtils,
  Dialogs,
  MainUnit in 'MainUnit.pas' {MainForm},
  FAT32Partition in 'FAT32Partition.pas',
  int13ext in 'int13ext.pas',
  PhysicalDrive in 'PhysicalDrive.pas',
  GenericFat in 'GenericFat.pas',
  DriveDump in 'DriveDump.pas' {FormDump},
  TreeListManager in 'TreeListManager.pas',
  GenericPartition in 'GenericPartition.pas',
  ConversionUnit in 'ConversionUnit.pas',
  FormProperties in 'FormProperties.pas' {PropertyForm},
  SelectDrive in 'SelectDrive.pas' {FormSelectDrive},
  RecoverOptions in 'RecoverOptions.pas' {FormOptions},
  SelectPartition in 'SelectPartition.pas' {FormSelectPart},
  Recovery in 'Recovery.pas',
  FAT16Partition in 'FAT16Partition.pas',
  VirtualDrive in 'VirtualDrive.pas',
  NTFSPartition in 'NTFSPartition.pas',
  ZAboutUnit in 'ZAboutUnit.pas' {FormAbout},
  GenericDrive in 'GenericDrive.pas',
  SHBrowseDialog in 'SHBrowseDialog.pas',
  TreeManager in 'TreeManager.pas',
  ProgressFormUnit in '..\ProgressForm\ProgressFormUnit.pas',
  NTFSAttributes in 'NTFSAttributes.pas',
  NTFSFileInfo in 'NTFSFileInfo.pas',
  NTFSMFTEntry in 'NTFSMFTEntry.pas',
  FATFileInfo in 'FATFileInfo.pas',
  CommonFileInfo in 'CommonFileInfo.pas',
  LinuxPartition in 'LinuxPartition.pas',
  XFSPartition in 'XFSPartition.pas',
  XFSFileInfo in 'XFSFileInfo.pas',
  XFSCommon in 'XFSCommon.pas';

{$R *.res}

begin
    // Interdire lancement plusieurs instances
    SetLastError(NO_ERROR);
    CreateMutex (nil, False, PChar(APPLI_TITLE_STR));
    if GetLastError = ERROR_ALREADY_EXISTS then
        Exit;

    // Vérifier si droits administrateurs
	{if (Win32Platform = VER_PLATFORM_WIN32_NT) and not isAdmin then
	begin
        MessageDlg(NOTANADMIN_MSG, mtError, [mbOK], 0);
        Exit;
    end;}
    
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormDump, FormDump);
  Application.CreateForm(TPropertyForm, PropertyForm);
  Application.CreateForm(TFormSelectDrive, FormSelectDrive);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormSelectPart, FormSelectPart);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.
