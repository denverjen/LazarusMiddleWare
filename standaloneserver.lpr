program standaloneserver;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, tcpmwserver, indylaz, zcomponent , LCL   { LCL added to to prevent link Error Exist Code 256 }
  { you can add units after this };

type

  { TSimpleTCPMW }

  TSimpleTCPMW = class(TCustomApplication)
    procedure WriteMsg( AMsg : String ) ;
    procedure ShowHelp ;
    procedure ShowAlias ;
  protected
    procedure DoRun; override;
  public
    MWServer : TTCPMServer ;
    pVersion : String ;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSimpleTCPMW }

procedure TSimpleTCPMW.DoRun;
var
  mOK : Boolean ;
  mKey,mStr : String ;
begin
  MWServer.pLogMessage := @WriteMsg ;
  MWServer.pLogMode := True ;
  MWServer.pLogLevel := 5 ; // All Log
  MWServer.pPort := 6999 ; // Server Port ;
  MWServer.SetAlias( 'testdb','mysqld-5','127.0.0.1','testrun','testpass','horse','utf8','',
  '/usr/lib/x86_64-linux-gnu/libmysqlclient.so.20.3.10','testpath','uusdfsd',3306 ) ;
  // Add any alias here
  MWServer.StartServer ;

  { add your program here }
  mOk := True ;
  while mOk do
   begin
   WriteMsg( 'dgs TCP Simple standalone server version : ' + pVersion ) ;
   WriteMsg( 'Type "STOP" to stop the application or "HELP" to list the other command' ) ;

   ReadLn( mKey ) ;
   mStr := UpperCase( Trim( mKey ) ) ;
   if mStr = 'STOP' then
     mOk := False ;
   if mStr = 'HELP' then
     ShowHelp ;
   if mStr = 'ALIAS' then
     ShowAlias ;
   end;
  // stop program loop
  Terminate;
end;

procedure TSimpleTCPMW.WriteMsg(AMsg: String);
begin
  Writeln( AMsg ) ;
end;

procedure TSimpleTCPMW.ShowHelp;
begin
 WriteMsg( '"ALIAS" to show the list of Alias !' ) ;
 WriteMsg( '---end---' ) ;
end;

procedure TSimpleTCPMW.ShowAlias;
var
  mList : TStringList ;
  mCount, i : Integer ;
begin
  mList := TStringList.Create ;
  Try
    mList.Text := MWServer.AliasList ;
    mCount := mList.Count ;
    for i := 0 to mCount - 1 do
      WriteMsg( mList.Strings[ i ] ) ;
  finally
    mList.Free ;
  end;
end;

constructor TSimpleTCPMW.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  MWServer := TTCPMServer.Create( Self ) ;
  pVersion := MWServer.ServerVersion ;
end;

destructor TSimpleTCPMW.Destroy;
begin
  MWServer.Free ;
  inherited Destroy;
end;

var
  Application: TSimpleTCPMW;
begin
  Application:=TSimpleTCPMW.Create(nil);
  Application.Title:='Standalone TCP Database MW';
  Application.Run;
  Application.Free ;
end.

