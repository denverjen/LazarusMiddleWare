program Lazmiddlewareserver;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazmiddleservermain, zcomponent, indylaz, tcpmwclient;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFmtMain, FmtMain);
  Application.CreateForm(Ttcpmclient, tcpmclient);
  Application.Run;
end.

