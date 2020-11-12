unit ZAboutUnit;

interface

uses
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
	Buttons, ExtCtrls, shellapi;

type
  TFormAbout = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LabelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  
var
  FormAbout: TFormAbout;


// ========================================================================
//								IMPLEMENTATION
// ========================================================================
implementation

uses
	ConversionUnit;
    
{$R *.dfm}


const
WebSiteURL	= 'http://';
MailURL		= 'mailto:';	


procedure TFormAbout.FormCreate(Sender: TObject);
begin
    Self.Caption			:= ABOUT_FORM_TITLE_STR;
	Self.Version.Caption	:= APPLI_VER_STR;
    Self.Copyright.Caption	:= APPLI_COPYRIGHT_STR;
    Self.ProductName.Caption:= APPLI_NAME_STR;
    Self.Comments.Caption	:= APPLI_COMMENT_STR;
end;


//
// Selon le label cliqué va sur la homepage ou envoie email
//
procedure TFormAbout.LabelClick(Sender: TObject);
var
	linkstr: string;
begin
	if sender = Self.ProductName then
		linkstr := WebSiteURL
    else
		linkstr := MailURL;

    Shellexecute(0, 'OPEN', Pchar(linkstr), nil, nil, SW_SHOW);        
end;


// --------------------------- Fin de l'unité -----------------------------
end.

