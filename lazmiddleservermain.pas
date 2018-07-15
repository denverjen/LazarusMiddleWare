unit lazmiddleservermain;

{$mode objfpc}{$H+}

interface

{
    This is a usage sample for tcpwmserver and tcpmwclient.
}


uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, DBGrids, tcpmwserver, db, tcpmwclient,
  dbcommon, lazutf8 ;

type

  { TFmtMain }

  TFmtMain = class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Button17: TButton;
    Button18: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LogMessage( mMsg : String ) ;
  private
    { private declarations }
    MT : TdgsMemTable ;
  public
    { public declarations }
    mServer : TTCPMServer ;
    mClient : Ttcpmclient ;
  end;

var
  FmtMain: TFmtMain;

implementation

{$R *.lfm}

{ TFmtMain }

procedure TFmtMain.FormCreate(Sender: TObject);
begin
  mServer := TTCPMServer.Create( Self ) ;
  mServer.pExePath := ExtractFilePath(  ParamStr( 0 )  ) ;
  mServer.pLogMessage := @LogMessage ;
  mServer.pLogMode := True ;
  mServer.pLogLevel := 5 ; // All Log
  mServer.pPort := 6999 ; // Server Port ;
  mServer.SetAlias( 'testdb','mysqld-5','127.0.0.1','testrun','testpass','horse','utf8',
  '/usr/lib/x86_64-linux-gnu/libmysqlclient.so.20.3.9','testpath','uusdfsd',3306 ) ;
  mClient := Ttcpmclient.Create( Self ) ;
  MT := TdgsMemTable.Create( Self ) ;
 end;

procedure TFmtMain.Button1Click(Sender: TObject);
begin
  mServer.StartServer ;
end;

procedure TFmtMain.Button10Click(Sender: TObject);
var
  mErr : String ;
begin
  mErr := mClient.ServerDeleteFile( 'usage.txt' ) ;
  if mErr <> '--' then
    ShowMessage( mClient.pLastError );
end;

procedure TFmtMain.Button11Click(Sender: TObject);
var
  mFile : TMemoryStream ;
begin
  mFile := TMemoryStream.Create ;
  Try
    mFile.LoadFromFile( 'usage.txt' ) ;
    mFile.Position := 0 ;
    mClient.ServerSaveFile( 'usage.txt',mFile )  ;
    if mClient.pLastError <> '--' then ShowMessage( mclient.pLastError ) ;
  finally
    mFile.Free ;
  end;
end;

procedure TFmtMain.Button12Click(Sender: TObject);
var
  mFile : TMemoryStream ;
  mTextFile : TStringList ;
begin
  mFile := TMemoryStream.Create ;
  Try
    if mClient.ServerLoadFile( 'usage.txt', mFile ) = '--' then
      begin
      mFile.Position := 0 ;
      mTextFile := TStringList.Create ;
      mTextFile.LoadFromStream( mFile ) ;
      Memo1.Lines.Add( mTextFile.Text ) ;
      mTextFile.Free ;
      end
    else
      ShowMessage( mClient.pLastError ) ;
  finally
    mFile.Free ;
  end;

end;

procedure TFmtMain.Button13Click(Sender: TObject);
var
  mLen,mULen : Integer ;
  mStr : String ;
begin
 mStr := '皑蔼碍爱翱袄' + '謅軸皺晝驟豬諸' ;
 mLen := Length ( mStr ) ;
 mULen := utf8Length( mStr ) ;
 ShowMessage( 'The Length of : ' + mStr + ' : ' + IntToStr( mLen ) ) ;
 ShowMessage( 'The UTF8 Length : ' + mStr + ' : ' + IntToStr( mULen ) ) ;
end;

procedure TFmtMain.Button14Click(Sender: TObject);
begin
  MT.EmtpyTable ;
end;

procedure TFmtMain.Button15Click(Sender: TObject);
var
  mLocalClient : Ttcpmclient ;
begin
  mLocalClient := Ttcpmclient.Create( Self ) ;
  mLocalClient.SetLocalMode( 'mysqld-5','127.0.0.1','testrun','testpass','horse','utf8',
  '/usr/lib/x86_64-linux-gnu/libmysqlclient.so.20.3.9','testpath',3306 ) ;
  MT.Base64AllData := mLocalClient.DBSelect( 'select * from raceday order by id desc' );
  DataSource1.DataSet := MT ;
  mLocalClient.Free ;
end;

procedure TFmtMain.Button16Click(Sender: TObject);
var
  mLocalClient : Ttcpmclient ;
begin
  mLocalClient := Ttcpmclient.Create( Self ) ;
  mLocalClient.SetLocalMode( 'mysqld-5','127.0.0.1','testrun','testpass','horse','utf8','/usr/lib/x86_64-linux-gnu/libmysqlclient.so.20.3.9','testpath',3306 ) ;
  Memo1.Lines.Add( mLocalClient.GetServerFileList('*.*') ) ;
  mLocalClient.Free ;
end;

procedure TFmtMain.Button17Click(Sender: TObject);
var
  mLocalClient : Ttcpmclient ;
begin
  mLocalClient := Ttcpmclient.Create( Self ) ;
  mLocalClient.pAddLog := @LogMessage ;
  mLocalClient.SetLocalMode( 'sqlite-3','','','','horse.db','utf8','/usr/lib/x86_64-linux-gnu/libsqlite3.so.0.8.6','testpath',0 ) ;
  MT.Base64AllData := mLocalClient.DBSelect( 'select * from raceday order by id desc' );
  DataSource1.DataSet := MT ;
  mLocalClient.Free ;
end;

procedure TFmtMain.Button18Click(Sender: TObject);
begin
  mClient.DisConnect ;
end;

procedure TFmtMain.Button2Click(Sender: TObject);
var
  mServerVersion : String ;
  mServerTime : TDateTime ;
begin
  // Point to your server IP
  mClient.pServerHost := '192.168.1.28' ;
  mClient.pServerAlias := 'testdb' ;
  mClient.pServerPort := 6999 ;
  mClient.pServerKey := 'uusdfsd' ;
  mClient.Connect ;
  mServerVersion := mClient.GetServerVersion ;
  mServerTime := mClient.GetServerDateTime ;
  StatusBar1.Panels[ 1 ].Text := 'Server Version : ' + mServerVersion + '   Server Date Time : ' +
  FormatDateTime( 'dd-mm-yyyy  hh:nn:ss', mServerTime ) ;
end;

procedure TFmtMain.Button3Click(Sender: TObject);
begin
  MT.Base64AllData := mClient.DBSelect( 'select id, wincount from raceday order by id desc limit 10' ) ;
  if mClient.pLastError = '--' then
    DataSource1.DataSet := MT
  else
    ShowMessage( 'Server Error : ' + mClient.pLastError ) ;
end;

procedure TFmtMain.Button4Click(Sender: TObject);
begin
   mClient.DBExecSQL( 'update raceday set wincount = 10 where id = 103'  );
   // Refresh the dataset again.
   MT.Base64AllData := mClient.DBSelect( 'select id, wincount from raceday order by id desc limit 10' ) ;
   if mClient.pLastError <> '--' then
     ShowMessage( 'Server Error : ' + mClient.pLastError ) ;
end;

procedure TFmtMain.Button5Click(Sender: TObject);
begin
  mClient.DBExecSQL( 'update raceday set wincount = 1 where id = 103'  );
  // Refresh the dataset again.
  MT.Base64AllData := mClient.DBSelect( 'select id, wincount from raceday order by id desc limit 10' ) ;
  if mClient.pLastError <> '--' then
    ShowMessage( 'Server Error : ' + mClient.pLastError ) ;
end;

procedure TFmtMain.Button6Click(Sender: TObject);
var
  mParams : TdgsParams ;
  mSQL : String ;
begin
  mParams := TdgsParams.Create( Self ) ;
  mSQL := 'select id, wincount from raceday where id > :id order by id limit 10'  ;
  mParams.AddInteger( 'id', 20  );
  MT.Base64AllData := mClient.DBSelect( mSQL, mParams.Base64AllData ) ;
  if mClient.pLastError <> '--' then
    ShowMessage( 'Server Error : ' + mClient.pLastError ) ;
  mParams.Free ;
end;

procedure TFmtMain.Button7Click(Sender: TObject);
var
  mParams : TdgsParams ;
  mSQL : String ;
begin
  mParams := TdgsParams.Create( Self ) ;
  mParams.AddInteger( 'wincount', 5 ) ;
  mSQL := 'update raceday set wincount = :wincount where id = 103' ;
  mClient.DBExecSQL( mSQL, mParams.Base64AllData  );
  mParams.Free ;
   // Refresh the dataset again.
   MT.Base64AllData := mClient.DBSelect( 'select id, wincount from raceday order by id desc limit 10' ) ;
  if mClient.pLastError <> '--' then
    ShowMessage( 'Server Error : ' + mClient.pLastError ) ;
end;

procedure TFmtMain.Button8Click(Sender: TObject);
begin
  Memo1.Lines.Add( mClient.GetServerFileList( '*.*' ) )  ;
end;

procedure TFmtMain.Button9Click(Sender: TObject);
var
  mOk : Boolean ;
begin
  mOk := mClient.ServerFileExist( 'wmain.c' ) ;
  if mOk then ShowMessage( 'wmain.c is found !' ) else ShowMessage( 'wmain.c is not found ' ) ;
  mOk := mClient.ServerFileExist( 'appmain.pas' ) ;
  if mOk then ShowMessage( 'appmain.pas is found !' ) else ShowMessage( 'appmain.pas is not found ' ) ;
end;

procedure TFmtMain.FormDestroy(Sender: TObject);
begin
  MT.Free ;
  mClient.Free ;
  mServer.Free ;
end;

procedure TFmtMain.LogMessage(mMsg: String);
begin
  Memo1.Lines.Add( mMsg ) ;
end;


end.

