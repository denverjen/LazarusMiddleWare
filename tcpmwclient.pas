unit tcpmwclient;

{$mode objfpc}{$H+}

interface

{
 tcpmwclient version : 0.2
 Date : 23/06/2018
}


uses
  Classes, SysUtils, FileUtil, db, dbcommon, Dialogs, ExtCtrls, LResources, ZCompatibility,
  IdTCPClient, IdThreadComponent, Variants, ZConnection, ZDataset,
  ZStoredProcedure ;

Const
  CLIENT_VERSION = '0.2.31' ;


type
  TLogMessage = procedure( mMsg : String ) of Object ;


type

  { Ttcpmclient }

  TTCPmclient = class(TDataModule)
    IdTCPClient1: TIdTCPClient;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    ZStoredProc1: TZStoredProc;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure Connect ;
    procedure DisConnect ;
    procedure Login( AKey : String ) ;
    procedure RequestService( AService, ACName: String; const Args: array of Variant; AResult: TMemoryStream);
    function ErrorBase64AllData( AError : String ) : String ;
    function DBSelect( ASQL : String ) : String ;
    function DBSelect( ASQL, AParaBase64AllData : String ) : String ;
    function DBExecSQL( ASQL : String ) : Integer ;
    function DBExecSQL( ASQL, AParaBase64AllData : String ) : Integer ;
    function DBInsertWithLastID( ASQL, ALastID : String ) : String ;
    function DBInsertWithLastID( ASQL, ALastID, AParaBase64AllData : String ) : String ;
    function GetServerVersion : String ;
    function GetClientVersion : String ;
    function GetServerDateTime : TDateTime ;
    function GetServerFileList( APattern : String ) : String ;
    function ServerFileExist( AFileName : String ) : Boolean  ;
    function ServerDeleteFile( AFileName : String ) : String ;
    function ServerSaveFile( AFileName : String ; var AFileStream : TMemoryStream ) : String ;
    function ServerLoadFile( AFileName : String ; var AFileStream : TMemoryStream ) : String ;
    procedure SetLocalMode( ADBType,AHost,AUser,APassword,ADBName,ACodePage,ASQL,ALibPath,ADataPath : String ; APort : Integer ) ;
  private
    { private declarations }
    function CreateLocalDBConnection( var AConn : TZConnection ) : String ;
    function DBServerSelect( ASQL : String ) : String ;
    function DBServerSelect( ASQL, AParaBase64AllData : String ) : String ;
    function DBServerExecSQL( ASQL : String ) : Integer ;
    function DBServerExecSQL( ASQL, AParaBase64AllData : String ) : Integer ;
    function DBServerInsertWithLastID( ASQL, ALastID  : String ) : String ;
    function DBServerInsertWithLastID( ASQL, ALastID, AParaBase64AllData  : String ) : String ;

    //
    function DBLocalSelect( ASQL : String ) : String ;
    function DBLocalSelect( ASQL, AParaBase64AllData : String ) : String ;
    function DBLocalExecSQL( ASQL : String ) : Integer ;
    function DBLocalExecSQL( ASQL, AParaBase64AllData : String ) : Integer ;
    function DBLocalInsertWithLastID( ASQL, ALastID  : String ) : String ;
    function DBLocalInsertWithLastID( ASQL, ALastID, AParaBase64AllData  : String ) : String ;

    procedure SetQueryParams( var AQuery : TZQuery ; ABase64AllData : String ) ;
    //
    function GetLocalFileList( APattern : String ) : String ;
    function LocalFileExist( AFileName : String ) : Boolean  ;
    function LocalDeleteFile( AFileName : String ) : String ;
    function LocalSaveFile( AFileName : String ; var AFileStream : TMemoryStream ) : String ;
    function LocalLoadFile( AFileName : String ; var AFileStream : TMemoryStream ) : String ;

  public
    { public declarations }
    CS : TRTLCriticalSection;
    pLocalMode : Integer ; // 0 = Server Mode, 1 = LocalMode ;
    pServerHost,pServerAlias,pServerKey : String ;
    pServerPort : Integer ;
    pLastError : String ;
    // Local Mode Data
    pLocalDBType,pLocalDBHost,pLocalDBUser,pLocalDBPass,pLocalDBName,pLocalCodePage,pLocalLibPath,pLocalDataPath : String ;
    pLocalSQL : String ;
    pLocalExePath : String ;
    pLocalDBPort : Integer ;
    pAddLog : TLogMessage ;
  end;

var
  tcpmclient: Ttcpmclient;

implementation

uses
  dgfunc ;

{$R *.lfm}

{ Ttcpmclient }

procedure TTCPmclient.DataModuleCreate(Sender: TObject);
begin
  InitCriticalSection( CS ) ;
  pLocalMode := 0 ;
  pLocalExePath := ExtractFilePath(  ParamStr( 0 )  ) ;
end;

procedure TTCPmclient.DataModuleDestroy(Sender: TObject);
begin
  DoneCriticalsection( CS ) ;
end;

procedure TTCPmclient.Connect;
begin
   IdTCPClient1.Host := pServerHost ;
   IdTCPClient1.Port := pServerPort ;
   IdTCPClient1.Connect ;
   Login( pServerKey ) ;
end;

procedure TTCPmclient.DisConnect;
begin
  IdTCPClient1.Disconnect ;
end;

procedure TTCPmclient.Login(AKey: String);
var
  ms : TMemoryStream ;
  mTime : TDatetime ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    RequestService( 'SYSTEM','LOGIN', [ pServerAlias, AKey ], ms ) ;
    mTime := dgStreamToDateTime( ms ) ;
    ms.Free ;
    end;
end;

procedure TTCPmclient.RequestService(AService, ACName: String;
  const Args: array of Variant; AResult: TMemoryStream);
var
  mInStream,mOutStream : TMemoryStream ;
  mSize : LongInt ;
  mByte,mVarByte : Byte ;
  mNoCompress : Boolean ;
  mDateTime : TDateTime ;
begin
  mInStream := TMemoryStream.Create ;
  mOutStream := TMemoryStream.Create ;
  dgStreamAddLines( AService, mInStream ) ;
  dgStreamAddLines( ACName, mInStream ) ;
  dgArrayOfVariantToStream( Args ,mInStream ) ;
  mInStream.Position := 0 ;
  dgCompressStream( mInStream,mOutStream ) ;
  mSize := mOutStream.Size ;
  mOutStream.Position := 0 ;
  AResult.Position := 0 ;
  EnterCriticalSection( CS ) ;
  Try
       Try
          IdTCPClient1.IOHandler.Write( mSize, False ) ;
          IdTCPClient1.IOHandler.Write( mOutStream, mSize, False ) ;
          mInStream.Clear ;
          mInStream.Position := 0 ;
          mSize := IdTCPClient1.IOHandler.ReadLongInt( False ) ;
          IdTCPClient1.IOHandler.ReadStream( mInStream, mSize, False ) ;
          mInStream.Position := 0 ;
          dgDeCompressStream( mInStream,AResult ) ;
          AResult.Position := 0 ;
        except
          IdTCPClient1.Disconnect ;
        end;
  finally
    LeaveCriticalSection( CS ) ;
    mOutStream.Free ;
    mInStream.Free ;
  end;
end;

function TTCPmclient.ErrorBase64AllData(AError: String): String;
var
  MT : TdgsMemTable ;
begin
  MT := TdgsMemTable.Create( Self ) ;
  MT.FieldDefs.Add( 'error',ftString, 255 ) ;
  MT.CreateDataset ;
  MT.Append ;
  MT.Edit ;
  MT.FieldByName( 'error' ).AsString := AError ;
  MT.Post ;
  Result := MT.Base64AllData ;
  MT.Free ;
end;

function TTCPmclient.DBSelect(ASQL: String): String;
begin
  if pLocalMode = 0 then
    Result := DBServerSelect( ASQL )
  else
    Result := DBLocalSelect( ASQL ) ;
end;

function TTCPmclient.DBSelect(ASQL, AParaBase64AllData: String): String;
begin
  if pLocalMode = 0 then
    Result := DBServerSelect( ASQL, AParaBase64AllData )
  else
    Result := DBLocalSelect( ASQL,AParaBase64AllData ) ;
end;

function TTCPmclient.DBExecSQL(ASQL: String): Integer;
begin
  if pLocalMode = 0 then
    Result := DBServerExecSQL( ASQL )
  else
    Result := DBLocalExecSQL( ASQL ) ;
end;

function TTCPmclient.DBExecSQL(ASQL, AParaBase64AllData: String): Integer;
begin
  if pLocalMode = 0 then
    Result := DBServerExecSQL( ASQL, AParaBase64AllData )
  else
    Result := DBLocalExecSQL( ASQL, AParaBase64AllData ) ;
end;

function TTCPmclient.DBInsertWithLastID(ASQL, ALastID: String): String;
begin
  if pLocalMode = 0 then
    Result := DBServerInsertWithLastID( ASQL, ALastID )
  else
    Result := DBLocalInsertWithLastID( ASQL, ALastID ) ;
end;

function TTCPmclient.DBInsertWithLastID(ASQL, ALastID, AParaBase64AllData: String): String;
begin
  if pLocalMode = 0 then
    Result := DBServerInsertWithLastID( ASQL, ALastID, AParaBase64AllData )
  else
    Result := DBLocalInsertWithLastID( ASQL, ALastID, AParaBase64AllData ) ;
end;

function TTCPmclient.GetServerVersion: String;
var
  ms : TMemoryStream ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    Try
      RequestService( 'SYSTEM','SERVERVERSION',[ 0 ],ms ) ;
      Result := dgStreamToStr( ms ) ;
    finally
      ms.Free ;
    end;
    end
  else
    Result := 'Local Mode' ;
end;

function TTCPmclient.GetClientVersion: String;
begin
  Result := CLIENT_VERSION ;
end;

function TTCPmclient.GetServerDateTime: TDateTime;
var
 ms : TMemoryStream ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    Try
      RequestService( 'SYSTEM','SERVERDATETIME',[ 0 ], ms ) ;
      Result := dgStreamToDateTime( ms ) ;
    finally
      ms.Free ;
    end;
    end
  else
    Result := Now ;
end;

function TTCPmclient.GetServerFileList(APattern: String): String;
var
  ms : TMemoryStream ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    Try
      RequestService( 'SYSTEM','GETDATAPATHFILELIST',[ pServerAlias, APattern ], ms  ) ;
      pLastError := dgStreamToStr( ms ) ;
      if pLastError = '--' then
        Result := dgStreamToStr( ms ) ;
      Finally
        ms.Free ;
      end;
    end
  else
    Result := GetLocalFileList( APattern ) ;
end;

function TTCPmclient.ServerFileExist(AFileName: String): Boolean;
var
  ms : TMemoryStream ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    Try
      RequestService( 'SYSTEM','FILEEXIST',[ pServerAlias,AFileName ], ms  ) ;
      pLastError := dgStreamToStr( ms ) ;
      if pLastError = '--' then
        Result := dgStreamToBoolean( ms ) ;
    Finally
      ms.Free ;
    end;
    end
  else
    Result := LocalFileExist( AFileName ) ;

end;

function TTCPmclient.ServerDeleteFile(AFileName: String): String;
var
  ms : TMemoryStream ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    Try
       RequestService( 'SYSTEM','DELETEFILE',[ pServerAlias, AFileName ], ms  ) ;
       pLastError := dgStreamToStr( ms ) ;
       Result := pLastError ;
    Finally
       ms.Free ;
    end;
    end
  else
    Result := LocalDeleteFile( AFileName ) ;

end;

function TTCPmclient.ServerSaveFile(AFileName: String;
  var AFileStream: TMemoryStream): String;
var
  mBase64String : String ;
  ms : TMemoryStream ;
begin
  if pLocalMode = 0 then
    begin
    ms := TMemoryStream.Create ;
    Try
      mBase64String := Base64StreamToString( AFileStream, AFileStream.Size ) ;
      RequestService( 'SYSTEM','SAVEFILE',[ pServerAlias, AFileName, mBase64String ], ms  ) ;
      pLastError := dgStreamToStr( ms ) ;
      Result := pLastError ;
    finally
      ms.Free ;
    end;
    end
  else
    Result := LocalSaveFile( AFileName, AFileStream ) ;
end;

function TTCPmclient.ServerLoadFile(AFileName: String;
  var AFileStream: TMemoryStream): String;
var
  ms : TMemoryStream ;
begin
   ms := TMemoryStream.Create ;
   Try
     RequestService( 'SYSTEM','LOADFILE',[ pServerAlias, AFileName ], ms  ) ;
     pLastError := dgStreamToStr( ms ) ;
     Result := pLastError ;
     if Result = '--' then
       begin
       Base64StringToStream( dgStreamToStr( ms ) , AFileStream ) ;
       end;
    Finally
      ms.Free ;
    end;
end;

procedure TTCPmclient.SetLocalMode(ADBType, AHost, AUser, APassword, ADBName,
  ACodePage, ASQL, ALibPath, ADataPath: String; APort: Integer);
begin
  pLocalMode := 1 ;
  pLocalDBType := ADBType ;
  pLocalDBHost := AHost ;
  pLocalDBUser := AUser ;
  pLocalDBPass := APassword ;
  pLocalDBName := ADBName ;
  pLocalCodePage := ACodePage ;
  pLocalLibPath := ALibPath ;
  pLocalDataPath := ADataPath ;
  pLocalSQL := ASQL ;
end;

function TTCPmclient.CreateLocalDBConnection(var AConn: TZConnection) : String ;
var
  mLocalSqlite : String ;
  mQuery : TZQuery ;
  mAllSQL,mTemp : TStringList ;
  mCount, i : Integer ;
  mStr, mLast : String ;
begin
   AConn := TZConnection.Create( Self ) ;
   mLocalSqlite :=  IncludeTrailingBackslash( pLocalExePath ) + IncludeTrailingBackslash( pLocalDataPath  ) + pLocalDBName ;
   if pos(  'sqlite', pLocalDBType ) > 0 then
     AConn.Database := mLocalSqlite
   else
     AConn.Database := pLocalDBName ;
//    if Assigned( pAddLog ) then pAddLog( AConn.Database ) ;

   // Check For db lib
   if FileExists( pLocalLibPath ) then
     begin
     Result := '' ;
     AConn.HostName := pLocalDBHost ;
     AConn.User := pLocalDBUser ;
     AConn.Password := pLocalDBPass ;
     AConn.Protocol := pLocalDBType ;
     AConn.Port := pLocalDBPort ;

     if UpperCase( pLocalCodePage)  = 'UTF8' then
       AConn.ControlsCodePage := cCP_UTF8 ;
     if UpperCase( pLocalCodePage)  = 'UTF16' then
       AConn.ControlsCodePage := cCP_UTF16 ;
     AConn.LibraryLocation := pLocalLibPath ;
     AConn.Connect ;
     if Trim( pLocalSQL ) <> '' then
       begin
       mAllSQL := TStringList.Create ;
       mTemp := TStringList.Create ;
       mQuery := TZQuery.Create( Self ) ;
       Try
       mAllSQL.Text := pLocalSQL ;
       mCount := mAllSQL.Count ;
       for i := 0 to mCount - 1 do
         begin
         mStr := Trim( mAllSQL.Strings[ i ] ) ;
         if mStr <> '' then
           mTemp.Add( mStr ) ;
         mLast := Copy( mStr, Length( mStr ) , 1 ) ;
         if mLast = ';' then
           begin
           mQuery.SQL.Text := mTemp.Text ;
           mQuery.ExecSQL ;
           mTemp.Clear ;
           end;
         end;
       if Trim( mTemp.Text ) <> '' then
         begin
         mQuery.SQL.Text := mTemp.Text ;
         mQuery.ExecSQL ;
         end;
       finally
         mQuery.Free ;
         mTemp.Free ;
         mAllSQL.Free ;
       end;
       end
     else
       begin
       Result := 'DB Client Lib : ' + pLocalLibPath + '  not found !' ;
       end;
     end;

end;

function TTCPmclient.DBServerSelect(ASQL: String): String;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Try
    RequestService( 'SQL','NOPARAMSELECTSQL',[ pServerAlias, ASQL ],ms ) ;
    pLastError := dgStreamToStr( ms ) ;
    if pLastError = '--' then
      Result := dgStreamToStr( ms )
    else
      Result := ErrorBase64AllData( pLastError ) ;
  finally
    ms.Free ;
  end;
end;

function TTCPmclient.DBServerSelect(ASQL, AParaBase64AllData: String): String;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Try
    RequestService( 'SQL','PARAMSELECTSQL',[ pServerAlias, ASQL, AParaBase64AllData ],ms ) ;
    pLastError := dgStreamToStr( ms ) ;
    if pLastError = '--' then
      Result := dgStreamToStr( ms )
    else
      Result := ErrorBase64AllData( pLastError ) ;
  finally
    ms.Free ;
  end;
end;

function TTCPmclient.DBServerExecSQL(ASQL: String): Integer;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Try
    RequestService( 'SQL','NOPARAMEXECSQL',[ pServerAlias, ASQL ],ms ) ;
    pLastError := dgStreamToStr( ms ) ;
    if pLastError = '--' then
      Result := dgStreamToInteger( ms )
    else
      Result := 0 ;
  finally
    ms.Free ;
  end;
end;

function TTCPmclient.DBServerExecSQL(ASQL, AParaBase64AllData: String): Integer;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Try
    RequestService( 'SQL','PARAMEXECSQL',[ pServerAlias, ASQL, AParaBase64AllData ],ms ) ;
    pLastError := dgStreamToStr( ms ) ;
    if pLastError = '--' then
      Result := dgStreamToInteger( ms )
    else
      Result := 0 ;
   finally
     ms.Free ;
   end;
end;

function TTCPmclient.DBServerInsertWithLastID(ASQL, ALastID: String): String;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Try
    RequestService( 'SQL','NOPARAMINSERTWITHLASTID',[ pServerAlias, ASQL, ALastID ],ms ) ;
    pLastError := dgStreamToStr( ms ) ;
    if pLastError = '--' then
      Result := dgStreamToStr( ms )
    else
      Result := ErrorBase64AllData( pLastError ) ;
  finally
    ms.Free ;
  end;
end;

function TTCPmclient.DBServerInsertWithLastID(ASQL, ALastID, AParaBase64AllData: String): String;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Try
    RequestService( 'SQL','PARAMINSERTWITHLASTID',[ pServerAlias, ASQL, ALastID, AParaBase64AllData ],ms ) ;
    pLastError := dgStreamToStr( ms ) ;
    if pLastError = '--' then
      Result := dgStreamToStr( ms )
    else
      Result := ErrorBase64AllData( pLastError ) ;
  finally
    ms.Free ;
  end;
end;

function TTCPmclient.DBLocalSelect(ASQL: String): String;
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mData : TdgsMemTable ;
begin
  pLastError := CreateLocalDBConnection( mConn  ) ;
  if pLastError = '' then
    begin
    mData := TdgsMemTable.Create( Self ) ;
    mQuery := TZQuery.Create( Nil ) ;
    Try
      mQuery.Connection := mConn ;
      mQuery.SQL.Text := ASQL ;
      pLastError := '--' ;
      Try
        mQuery.Active := True ;
        CopyStructToMT( TDataSet( mQuery ), mData ) ;
        CopyAllRecord( TDataSet( mQuery ), mData ) ;
        Result := mData.Base64AllData ;
      Except
        On E: Exception do
          begin
          pLastError := E.Message ;
          Result := ErrorBase64AllData( pLastError ) ;
          end;
        end;
    finally
      mQuery.Free ;
      mData.Free ;
      mConn.Free ;
    end;
    end
  else
   Result := ErrorBase64AllData( pLastError ) ;
end;

function TTCPmclient.DBLocalSelect(ASQL, AParaBase64AllData: String): String;
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mData : TdgsMemTable ;
begin
  pLastError := CreateLocalDBConnection( mConn  ) ;
  if pLastError = '' then
    begin
    mQuery := TZQuery.Create( Nil ) ;
    mData := TdgsMemTable.Create( Self ) ;
    Try
      mQuery.Connection := mConn ;
      mQuery.SQL.Text := ASQL ;
      pLastError := '--' ;
      Try
        SetQueryParams( mQuery, AParaBase64AllData ) ;
        mQuery.Active := True ;
        CopyStructToMT( TDataSet( mQuery ), mData ) ;
        CopyAllRecord( TDataSet( mQuery ), mData ) ;
        Result := mData.Base64AllData ;
      Except
        On E: Exception do
          begin
          pLastError := E.Message ;
          Result := ErrorBase64AllData( pLastError ) ;
          end;
        end;
    finally
      mQuery.Free ;
      mData.Free ;
      mConn.Free ;
    end;
    end
  else
    Result := ErrorBase64AllData( pLastError ) ;
end;

function TTCPmclient.DBLocalExecSQL(ASQL: String): Integer;
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
begin
   pLastError := CreateLocalDBConnection( mConn  ) ;
   if pLastError = '' then
     begin
     mQuery := TZQuery.Create( Self ) ;
     mQuery.Connection := mConn ;
     pLastError := '--' ;
     Try
       mQuery.SQL.Text := ASQL ;
       Try
         mQuery.ExecSQL ;
         Result := mQuery.RowsAffected ;
       except
         On E: Exception do
         begin
         pLastError := E.Message ;
         end;
       end;
     finally
       mQuery.Free ;
       mConn.Free ;
     end;
     end
   else
     Result := 0 ;
end;

function TTCPmclient.DBLocalExecSQL(ASQL, AParaBase64AllData: String): Integer;
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
begin
   pLastError := CreateLocalDBConnection( mConn  ) ;
   if pLastError = '' then
     begin
     mQuery := TZQuery.Create( Self ) ;
     mQuery.Connection := mConn ;
     pLastError := '--' ;
     Try
       mQuery.SQL.Text := ASQL ;
       Try
         SetQueryParams( mQuery, AParaBase64AllData ) ;
         mQuery.ExecSQL ;
         Result := mQuery.RowsAffected ;
       except
       On E: Exception do
         begin
         pLastError := E.Message ;
         Result := 0 ;
         end;
       end;
     finally
       mQuery.Free ;
       mConn.Free ;
     end;
     end
   else
     Result := 0 ;
end;

function TTCPmclient.DBLocalInsertWithLastID(ASQL, ALastID: String): String;
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mData : TdgsMemTable ;
begin
  pLastError := CreateLocalDBConnection( mConn ) ;
  if pLastError = '' then
    begin
    mData := TdgsMemTable.Create( Self ) ;
    mQuery := TZQuery.Create( Nil ) ;
    Try
    mQuery.Connection := mConn ;
    mQuery.SQL.Text := ASQL ;
    Try
      mQuery.ExecSQL;
      pLastError := '--' ;
      Try
        mQuery.SQL.Text := ALastID ;
        mQuery.Active := True ;
        CopyStructToMT( TDataSet( mQuery ), mData ) ;
        CopyAllRecord( TDataSet( mQuery ), mData ) ;
        Result := mData.Base64AllData ;
      Except
        On E: Exception do
          begin
          pLastError := E.Message ;
          Result := ErrorBase64AllData( pLastError ) ;
          end;
        end;
      Except
        On E: Exception do
        begin
        pLastError := E.Message ;
        end;
      end;
    finally
     mQuery.Free ;
     mData.Free ;
     mConn.Free ;
    end;
    end
  else
    Result := ErrorBase64AllData( pLastError ) ;
end;

function TTCPmclient.DBLocalInsertWithLastID(ASQL, ALastID, AParaBase64AllData: String): String;
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mData : TdgsMemTable ;
begin
  pLastError := CreateLocalDBConnection( mConn  ) ;
  if pLastError = '' then
    begin
    mData := TdgsMemTable.Create( Self ) ;
    mQuery := TZQuery.Create( Nil ) ;
    Try
      mQuery.Connection := mConn ;
      mQuery.SQL.Text := ASQL ;
      SetQueryParams( mQuery, AParaBase64AllData ) ;
      Try
        mQuery.ExecSQL;
        pLastError := '--' ;
        Try
          mQuery.SQL.Text := ALastID ;
          mQuery.Active := True ;
          CopyStructToMT( TDataSet( mQuery ), mData ) ;
          CopyAllRecord( TDataSet( mQuery ), mData ) ;
          Result := mData.Base64AllData ;
        Except
          On E: Exception do
            begin
            pLastError := E.Message ;
            Result := ErrorBase64AllData( pLastError ) ;
            end;
        end;
      Except
        On E: Exception do
          begin
          pLastError := E.Message ;
          Result := ErrorBase64AllData( pLastError ) ;
          end;
      end;
    finally
      mData.Free ;
      mQuery.Free ;
      mConn.Free ;
    end;
    end
  else
    Result := ErrorBase64AllData( pLastError ) ;

end;

procedure TTCPmclient.SetQueryParams(var AQuery: TZQuery; ABase64AllData: String
  );
var
  mParams : TdgsMemTable ;
  mCount, i : Integer ;
  mFieldType : TFieldType ;
  mParaName : String ;
  mFieldItems : TFieldDef ;
  ms : TMemoryStream ;
  mDBType : String ;
  mMYSQL : Boolean ;
begin
  mDBType := UpperCase( AQuery.Connection.Protocol ) ;
  if pos( 'MYSQL',mDBType ) >= 0 then
     mMYSQL := True
  else
     mMYSQL := False ;
  mParams := TdgsMemTable.Create( Self ) ;
  mParams.Base64AllData := ABase64AllData ;

  Try

  mCount := AQuery.Params.Count ;
  for i := 0 to mCount - 1 do
    begin
    mParaName := AQuery.Params.Items[ i ].Name ;
    mFieldItems := mParams.FieldDefs.Find( mParaName ) ;
    mFieldType := mFieldItems.DataType ;
    if mFieldType = ftString then
      AQuery.Params.Items[ i ].AsString := mParams.FieldByName( mParaName ).AsString ;
    if mFieldType = ftFloat then
      AQuery.Params.Items[ i ].AsFloat := mParams.FieldByName( mParaName ).AsFloat ;
    if mFieldType = ftInteger then
      AQuery.Params.Items[ i ].AsInteger := mParams.FieldByName( mParaName ).AsInteger ;
    if mFieldType = ftDateTime then
      AQuery.Params.Items[ i ].AsDateTime := mParams.FieldByName( mParaName ).AsDateTime ;
    if mFieldType = ftDate then
      AQuery.Params.Items[ i ].AsDate := mParams.FieldByName( mParaName ).AsDateTime ;
    if mFieldType = ftBoolean then
      begin
      if mMYSQL then
        AQuery.Params.Items[ i ].AsInteger := dgBoolToInt( mParams.FieldByName( mParaName ).AsBoolean )
      else
        AQuery.Params.Items[ i ].AsBoolean := mParams.FieldByName( mParaName ).AsBoolean ;
      end;
    if mFieldType = ftCurrency then
      AQuery.Params.Items[ i ].AsFloat := mParams.FieldByName( mParaName ).AsFloat ;
    if mFieldType = ftMemo then
      AQuery.Params.Items[ i ].AsMemo := mParams.FieldByName( mParaName ).AsString  ;
    if mFieldType = ftBlob then
      begin
      ms := TMemoryStream.Create ;
      TBlobField( mParams.FieldByName( mParaName ) ).SaveToStream( ms ) ;
      ms.Position := 0 ;
      AQuery.Params.Items[ i ].LoadFromStream( ms, ftBlob ) ;
      ms.Free ;
      end;
    end;
  finally
    mParams.Free ;
  end;

end;

function TTCPmclient.GetLocalFileList(APattern: String): String;
var
  mfl,mList : TStringList ;
  i,mCount : Integer ;
  mDataPath : String ;
begin
  mDataPath := IncludeTrailingBackslash( pLocalExePath ) + IncludeTrailingBackslash( pLocalDataPath  ) ;
  mList := TStringList.Create ;
  mfl := FindAllFiles( mDataPath,APattern,True ) ;
  mCount := mfl.Count ;
  for i := 0 to mCount - 1 do
    mList.Add( ExtractFileName( mfl.Strings[ i ] ) ) ;
  Result := mList.Text ;
  mfl.Free ;
  mList.Free ;
end;

function TTCPmclient.LocalFileExist(AFileName: String): Boolean;
var
  mDataPath,mFName : String ;
begin
  mDataPath := IncludeTrailingBackslash( pLocalExePath ) + IncludeTrailingBackslash( pLocalDataPath  ) ;
  mFName := mDataPath  +  AFileName ;
  Result := FileExists( mFName ) ;
end;

function TTCPmclient.LocalDeleteFile(AFileName: String): String;
var
  mDataPath,mFName : String ;
begin
  mDataPath := IncludeTrailingBackslash( pLocalExePath ) + IncludeTrailingBackslash( pLocalDataPath  ) ;
  mFName := mDataPath  +  AFileName ;
  pLastError := '--' ;
  if FileExists( mFName ) then
    DeleteFile( mFName )
  else
    pLastError := 'File : ' + AFileName + ' Not found !' ;
end;

function TTCPmclient.LocalSaveFile(AFileName: String;
  var AFileStream: TMemoryStream): String;
var
  mDataPath,mFName : String ;
begin
  mDataPath := IncludeTrailingBackslash( pLocalExePath ) + IncludeTrailingBackslash( pLocalDataPath  ) ;
  mFName := mDataPath  +  AFileName ;
  Try
    if FileExists( mFName ) then
      DeleteFile( mFName ) ;
    AFileStream.Position := 0 ;
    AFileStream.SaveToFile( mFName ) ;
   Except
     pLastError := 'Error Deleting / Saveing file : ' + AFileName ;
   end ;
end;

function TTCPmclient.LocalLoadFile(AFileName: String;
  var AFileStream: TMemoryStream): String;
var
  mDataPath,mFName : String ;
begin
  mDataPath := IncludeTrailingBackslash( pLocalExePath ) + IncludeTrailingBackslash( pLocalDataPath  ) ;
  mFName := mDataPath  +  AFileName ;
  pLastError := '--' ;
  if FileExists( mFName ) then
    begin
    Try
      AFileStream.LoadFromFile( mFName ) ;
      AFileStream.Position := 0 ;
    Except
      On E : Exception do
      begin
      pLastError := E.Message ;
      end;
    end;
    end
  else
    pLastError := 'File not found : ' + pLocalDataPath + ':' + AFileName ;
  Result := pLastError ;
end;

end.

