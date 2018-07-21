unit tcpmwserver;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, FileUtil, ZConnection, ZDataset, ZCompatibility ,
  IdTCPServer, dbcommon, IdGlobal, db , IdCustomTCPServer, IdContext;


Const
  SERVER_VERSION = '0.2.3' ;

type
  TMyContext = class
    UserID : Integer ;
    Login : Word ;
    LastTime : DWord ;
    ClientIP : String ;
    ClientPort : Integer ;
  end;

  TLogMessage = procedure( mMsg : String ) of Object ;

type

  { TTCPMServer }

  TTCPMServer = class(TDataModule)
    IdTCPServer1: TIdTCPServer;
    ZConnection1: TZConnection;
    ZQuery1: TZQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure StartServer ;
    procedure ClearAlias ;
    function AliasList : String ;
    procedure SetAlias( AAliasName,ADBType,AHost,AUser,APassword,ADBName,ACodePage,ASQL,ALibPath,ADataPath,AKey : String ; APort : Integer ) ;
    procedure AddLog( mMessage : String ; mLevel : Integer ) ;
  private
    { private declarations }
    DBCS,LOGCS,PROCCS : TRTLCriticalSection ;
    function GetAliasKey( mAlias : String ) : String ;
    function GetAliasPath( mAlias : String ) : String ;
    procedure InternalServices(mServiceName, mCName : String; mArgsStream, mResultStream: TMemoryStream) ;
    procedure __systemcall(mCName : String; mArgsStream, mResultStream: TMemoryStream);
    procedure __sqlcall(mCName : String; mArgsStream, mResultStream: TMemoryStream);
    function ErrorData(mMsg: String): String ;
    procedure CreateDBConnection( var mConn : TZConnection ; mAlias : String  ) ;
    procedure SetQueryParams( var mQuery : TZQuery ; var mParams : TdgsMemTable ) ;
    function GetDataPathFileList( mDataPath, mPattern : String ) : String ;
    // __sqlcall
    procedure NoParamSelectSQL( mArgsStream, mResultStream : TMemoryStream ) ;
    procedure NoParamExecSQL( mArgsStream, mResultStream : TMemoryStream ) ;
    procedure NoParamInsertWithLastID( mArgsStream, mResultStream : TMemoryStream ) ;
    procedure ParamSelectSQL( mArgsStream, mResultStream : TMemoryStream ) ;
    procedure ParamExecSQL( mArgsStream, mResultStream : TMemoryStream ) ;
    procedure ParamInsertWithLastID( mArgsStream, mResultStream : TMemoryStream ) ;

  public
    { public declarations }
    pLogMode : Boolean ; //  True = use pLogMessage to send lon
    pLogLevel : Integer ; // Level 1,2,3
    pPort : Integer ;
    pExePath : String ;
    pLogMessage : TLogMessage ;
    WAlias : TdgsMemTable ;
  end;

var
  TCPMServer: TTCPMServer;

implementation

uses dgfunc ;

{$R *.lfm}

{ TTCPMServer }

procedure TTCPMServer.DataModuleCreate(Sender: TObject);
begin
    pExePath := ParamStr( 0 ) ;
    IdTCPServer1.Bindings.Add ;
    IdTCPServer1.Bindings.Items[ 0 ].IPVersion := Id_IPv4 ;
    InitCriticalSection( DBCS ) ;
    InitCriticalSection( LOGCS ) ;
    InitCriticalSection( PROCCS ) ;
    pLogMode  := False ;
    pLogLevel := 5 ;
    WAlias := TdgsMemTable.Create( Self ) ;
    WAlias.FieldDefs.Add( 'alias',ftString,50 ) ;
    WAlias.FieldDefs.Add( 'database',ftString,50 ) ;
    WAlias.FieldDefs.Add( 'host',ftString,50 ) ;
    WAlias.FieldDefs.Add( 'user',ftString,50 ) ;
    WAlias.FieldDefs.Add( 'password',ftString,50 ) ;
    WAlias.FieldDefs.Add( 'dbtype',ftString,50 ) ;
    WAlias.FieldDefs.Add( 'libpath',ftString,300 ) ;
    WAlias.FieldDefs.Add( 'datapath',ftString,300 ) ;
    WAlias.FieldDefs.Add( 'codepage',ftString,10 ) ;
    WAlias.FieldDefs.Add( 'port',ftInteger ) ;
    WAlias.FieldDefs.Add( 'sql',ftMemo) ;
    WAlias.FieldDefs.Add( 'key',ftString,30 ) ;
    WAlias.CreateDataset ;
    WAlias.Active := True ;
end;

procedure TTCPMServer.DataModuleDestroy(Sender: TObject);
begin
  WAlias.Free ;
  DoneCriticalsection( PROCCS ) ;
  DoneCriticalsection( LOGCS ) ;
  DoneCriticalsection( DBCS ) ;
end;

procedure TTCPMServer.IdTCPServer1Connect(AContext: TIdContext);
var
  mFrom : String ;
  mPort : Integer ;
begin
  mFrom := AContext.Connection.Socket.Binding.PeerIP;
  mPort := AContext.Connection.Socket.Binding.PeerPort ;
  AddLog( 'Connected from : ' + mFrom + ':' + IntTostr( mPort ),  3 ) ;
  AContext.Data := TMyContext.Create ;
  TMyContext( AContext.Data ).LastTime := IdGlobal.Ticks();
  TMyContext( AContext.Data ).Login := 0 ;
  TMyContext( AContext.Data ).ClientIP := mFrom ;
  TMyContext( AContext.Data ).ClientPort := mPort ;
end;

procedure TTCPMServer.IdTCPServer1Disconnect(AContext: TIdContext);
var
  mFrom : String ;
begin
  mFrom := AContext.Binding.IP ;
  TMyContext( AContext.Data ).Free ;
  AddLog( 'Disconnected from : ' + mFrom, 3 ) ;
  AContext.Data := nil ;
end;

procedure TTCPMServer.IdTCPServer1Execute(AContext: TIdContext);
var
  mInStream,mOutStream,mArgsStream,mResultStream : TMemoryStream ;
  mSize : Integer ;
  mCommand, mCName, mKey, mAlias, mAKey : String ;
  mByte : Byte ;
  mNow : TDateTime ;
begin
  // Only Handle a few Type of Commands
  // 1. System Command e.g ServerDateTime, ServerVersion,  ServerPing .....
  // 2. SQL Services
  mInStream := TMemoryStream.Create ;
  mOutStream := TMemoryStream.Create ;
  Try
    mSize := AContext.Connection.IOHandler.ReadInt32( False ) ;
    AContext.Connection.IOHandler.ReadStream( mInStream, mSize, False ) ;
    mInStream.Position := 0 ;
    // Client will Compress the stream, so Decompress Stream here !

    dgDeCompressStream( mInStream,mOutStream ) ;
    mOutStream.Position := 0 ;
    mCommand := UpperCase( dgStreamReadLine( mOutStream ) ) ;
    mCName := UpperCase( dgStreamReadLine( mOutStream ) ) ;

    mArgsStream := TMemoryStream.Create ;
    Try
      dgCopyStream( mOutStream, mArgsStream ) ;
      mArgsStream.Position := 0 ;
      AddLog( 'Command : ' + mCommand + '  CNAME : ' + mCName, 2 ) ;
      if ( mCommand = 'SYSTEM' ) and ( mCName = 'LOGIN' ) then
        begin
        mArgsStream.Read( mByte, 1 ) ;
        mArgsStream.Read( mSize, SizeOf( mSize ) ) ;
        mAlias := UpperCase( dgStreamToStr( mArgsStream ) ) ;
        mKey := dgStreamToStr( mArgsStream ) ;
        AddLog( 'Client Key ( ' + mAlias + ' ) : ' + mKey, 2 ) ;
        mAKey := GetAliasKey( mAlias ) ;
        AddLog( 'Alias  Key ( ' + mAlias + ' ) : ' + mAKey, 2 ) ;
        if mKey = mAKey then
          begin
          TMyContext( AContext.Data ).Login := 1 ;
          mInStream.Clear ;
          mOutStream.Clear ;
          mNow := Now ;
          dgDateTimeToStream( mNow , mInStream ) ;
          mInStream.Position := 0 ;
          dgCompressStream( mInStream, mOutStream ) ;
          mSize := mOutStream.Size ;
          mOutStream.Position := 0 ;
          AContext.Connection.IOHandler.Write( mSize , False ) ;
          AContext.Connection.IOHandler.Write( mOutStream, mSize, False ) ;
          AContext.Connection.IOHandler.WriteBufferClear ;
          AddLog( 'Login OK ! and ' + IntToStr( mSize ) + ' bytes of data back to client !' ,2  ) ;
          end
        else
          begin
          TMyContext( AContext.Data ).Login := 0 ;
          AddLog( 'Login Fail ! Key InValid !', 2 ) ;
          AContext.Connection.Disconnect ;
          end ;
        end
      else
        begin
        if TMyContext( AContext.Data ).Login = 1 then
          begin
          mOutStream.Clear ;
          mResultStream := TMemoryStream.Create ;
          Try
            InternalServices( mCommand, mCName, mArgsStream, mResultStream ) ;
            mResultStream.Position := 0 ;
            dgCompressStream( mResultStream ,mOutStream ) ;
            mOutStream.Position := 0 ;
            mSize := mOutStream.Size ;
            AContext.Connection.IOHandler.Write( mSize,False ) ;
            AContext.Connection.IOHandler.Write( mOutStream, mSize, False ) ;
            AContext.Connection.IOHandler.WriteBufferClear ;
          finally
            mResultStream.Free ;
          end;
          end
        else
          begin
          AddLog( 'Not Valid Login !', 2 ) ;
          AContext.Connection.DisConnect ;
          end;
        end;
      finally
        mArgsStream.Free ;
      end;
    finally
      mOutStream.Free ;
      mInStream.Free ;
    end;

end;

procedure TTCPMServer.StartServer;
begin
  if not IdTCPServer1.Active then
    begin
    IdTCPServer1.Bindings.Items[ 0 ].Port := pPort ;
    IdTCPserver1.Active := True ;
    // This binary has no thread support compiled in.
    // Recompile the application with a thread-driver in the program uses
    // clause before other units using thread.
    // Runtime error 232
    AddLog( 'Server Started !' , 1 ) ;
    end;
end;

procedure TTCPMServer.ClearAlias;
begin
  WAlias.EmtpyTable ;
end;

function TTCPMServer.AliasList: String;
var
  mList : TStringList ;
begin
  mList := TStringList.Create ;
  EnterCriticalSection( DBCS ) ;
  Try
    WAlias.First ;
    While not WAlias.Eof do
      begin
      mList.Add( WAlias.FieldByName( 'alias' ).AsString + '/' +
                 WAlias.FieldByName( 'dbtype' ).AsString + '/' +
                 WAlias.FieldByName( 'database' ).AsString + '/' +
                 WAlias.FieldByName( 'datapath' ).AsString ) ;
      WAlias.Next ;
      end;
  finally
    LeaveCriticalSection( DBCS ) ;
  end;
  Result := mList.Text ;
  mList.Free ;

end;

procedure TTCPMServer.SetAlias(AAliasName, ADBType, AHost, AUser, APassword,
  ADBName, ACodePage, ASQL, ALibPath, ADataPath, AKey : String; APort: Integer);
begin
  WAlias.Append ;
  WAlias.Edit ;
  WAlias.FieldByName( 'alias' ).AsString := UpperCase( AAliasName ) ;
  WAlias.FieldByName( 'database' ).AsString := ADBName ;
  WAlias.FieldByName( 'host' ).AsString := AHost ;
  WAlias.FieldByName( 'dbtype' ).AsString := ADBType ;
  WAlias.FieldByName( 'libpath' ).AsString := ALibPath ;
  WAlias.FieldByName( 'datapath' ).AsString := ADataPath ;
  WAlias.FieldByName( 'user' ).AsString := AUser ;
  WAlias.FieldByName( 'password' ).AsString := APassword ;
  WAlias.FieldByName( 'key' ).AsString := AKey ;
  WAlias.FieldByName( 'port' ).AsInteger := APort ;
  WAlias.FieldByName( 'codepage' ).AsString := ACodePage ;
  WAlias.FieldByName( 'sql' ).AsString := ASQL ;
  WAlias.Post ;
end;

function TTCPMServer.GetAliasKey(mAlias: String): String;
var
  mKey : String ;
begin
  mKey := FloatToStr( Now ) ;
  EnterCriticalSection( DBCS ) ;
  Try
    if WAlias.Locate( 'alias',UpperCase( mAlias ), [] ) then
      mKey := WAlias.FieldByName( 'key' ).AsString ;
  finally
   LeaveCriticalSection( DBCS ) ;
  end;
  Result := mKey ;
end;

function TTCPMServer.GetAliasPath(mAlias: String): String;
begin
  EnterCriticalSection( DBCS ) ;
   Try
     if not WAlias.Locate( 'alias', UpperCase( mAlias ) , [] ) then AddLog( 'Locate Error : ' + mAlias, 3 ) ;
     Result := IncludeTrailingBackslash( pExePath)  + IncludeTrailingBackslash( WAlias.FieldByName( 'datapath' ).AsString ) ;
   finally
    LeaveCriticalSection( DBCS ) ;
   end;
end;

procedure TTCPMServer.InternalServices(mServiceName, mCName: String;  mArgsStream, mResultStream: TMemoryStream);
begin
  if mServiceName = 'SYSTEM' then
    __systemcall( mCName, mArgsStream, mResultStream )
  else if mServiceName = 'SQL' then
    __sqlCall( mCName, mArgsStream, mResultStream ) ;
end;

procedure TTCPMServer.__systemcall(mCName : String; mArgsStream, mResultStream: TMemoryStream);
var
  mDateTime : TDateTime ;
  mAlias,mFileName,lError,mName,mStr,mFilePattern : String ;
  mSize : LongInt ;
  mByte : Byte ;
  mText : TStringList ;
  mOk : Boolean ;
  mFileStream : TMemoryStream ;
begin
  AddLog( 'SYSTEM CALL --- ' + mCName, 3 ) ;
  if mCName = 'SERVERDATETIME' then
    begin
    mDateTime := Now ;
    dgDateTimeToStream( mDateTime, mResultStream ) ;
    end
  else if mCName = 'SERVERVERSION' then
    begin
    mStr := SERVER_VERSION ;
    dgStrToStream( mStr , mResultStream ) ;
    end
  else if mCName = 'GETDATAPATHFILELIST' then
    begin
    mArgsStream.Read( mByte, 1 ) ;  // 88
    mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // 2
    mAlias := dgStreamToStr( mArgsStream ) ;
    mFilePattern := dgStreamToStr( mArgsStream ) ;
    mText := TStringList.Create ;
    AddLog( GetAliasPath( mAlias ) , 3 ) ;
    mText.Text := GetDataPathFileList( GetAliasPath( mAlias ), mFilePattern ) ;
    lError := '--' ;
    dgStrToStream( lError, mResultStream ) ;
    dgStrToStream( mText.Text, mResultStream ) ;
    mText.Free ;
    end
   else if mCName = 'FILEEXIST' then
    begin
    lError := '--' ;
    mArgsStream.Read( mByte, 1 ) ;  // 88
    mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // 2
    mAlias := dgStreamToStr( mArgsStream ) ;
    mName := dgStreamToStr( mArgsStream ) ;
    mFileName := GetAliasPath( mAlias ) +  mName ;
    mOk := FileExists( mFileName ) ;
    dgStrToStream( lError, mResultStream ) ;
    dgBooleanToStream( mOk, mResultStream ) ;
    end
  else if mCName = 'DELETEFILE' then
    begin
    lError := '--' ;
    mArgsStream.Read( mByte, 1 ) ;  // 88
    mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // 1
    mAlias := dgStreamToStr( mArgsStream ) ;
    mName := dgStreamToStr( mArgsStream ) ;
    mFileName := GetAliasPath( mAlias ) +  mName ;
    AddLog( 'Delete file : ' + mFileName, 2 ) ;
   if FileExists( mFileName ) then
     DeleteFile( mFileName )
   else
     lError := 'File : ' + mName + ' Not found !' ;
     dgStrToStream( lError, mResultStream ) ;
    end
  else if mCName = 'SAVEFILE' then
    begin
    lError := '--' ;
    mArgsStream.Read( mByte, 1 ) ;  // 88
    mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // 3
    mAlias := dgStreamToStr( mArgsStream ) ;
    mName := dgStreamToStr( mArgsStream ) ;
    mFileStream := TMemoryStream.Create ;
    Try
      Base64StringToStream( dgStreamToStr( mArgsStream ), mFileStream ) ;
      mFileStream.Position := 0 ;
      mFileName := GetAliasPath( mAlias ) +  mName ;
        Try
          if FileExists( mFileName ) then
            DeleteFile( mFileName ) ;
          mFileStream.SaveToFile( mFileName ) ;
         Except
            lError := 'Error Deleting / Saveing file : ' + mName ;
         end ;
      Finally
        mFileStream.Free ;
        dgStrToStream( lError, mResultStream ) ;
      end;
    end
  else if mCName = 'LOADFILE' then
    begin
    lError := '--' ;
    mArgsStream.Read( mByte, 1 ) ;  // 88
    mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // 1
    mAlias := dgStreamToStr( mArgsStream ) ;
    mName := dgStreamToStr( mArgsStream ) ;
    mFileName := GetAliasPath( mAlias ) +  mName ;
    AddLog( 'Get Configure file : ' + mFileName, 3 ) ;
    mFileStream := TMemoryStream.Create ;
    Try
      if FileExists( mFileName ) then
        begin
        mFileStream.LoadFromFile( mFileName ) ;
        mFileStream.Position := 0 ;
        end
      else
        lError := 'File not found : ' + mAlias + ':' + mName ;
      dgStrToStream( lError, mResultStream ) ;
      dgStrToStream( Base64StreamToString( mFileStream, mFileStream.Size ), mResultStream ) ;
     Finally
       mFileStream.Free;
     end
    end
  else
    begin
    // pAddLog( 'SYSTEM-' + mCName + ' Not Found !' ) ;
    lError := 'error : ' + mCName + ' Not Found 1'  ;
    dgStrToStream( lError , mResultStream ) ;
    end;
end;

procedure TTCPMServer.__sqlcall(mCName: String; mArgsStream, mResultStream: TMemoryStream);
var
  v : Variant ;
begin
  AddLog( 'SQL : ' + mCName, 2 ) ;
  if mCName = 'NOPARAMSELECTSQL' then
    NoParamSelectSQL( mArgsStream , mResultStream )
  else if mCName = 'NOPARAMEXECSQL' then
    NoParamExecSQL(mArgsStream , mResultStream )
  else if mCName = 'PARAMSELECTSQL' then
    ParamSelectSQL( mArgsStream , mResultStream )
  else if mCName = 'PARAMEXECSQL' then
    ParamExecSQL(mArgsStream , mResultStream )
  else if mCName = 'NOPARAMINSERTWITHLASTID' then
    NoParamInsertWithLastID(mArgsStream , mResultStream )
  else if mCNAME = 'PARAMINSERTWITHLASTID' then
    NoParamInsertWithLastID(mArgsStream , mResultStream )
  else
    begin
    // pAddLog( 'SQL-' + mCName + ' Not Found !' ) ;
    dgStrToStream(  ErrorData( 'SQL-' + mCName + ' Not Found !' ), mResultStream ) ;
    end;
end;

procedure TTCPMServer.NoParamSelectSQL(mArgsStream, mResultStream: TMemoryStream );
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mError,mAlias : String ;
  mByte : Byte ;
  mSize : LongInt ;
  mSQL : String ;
  mData : TdgsMemTable ;
begin
  mArgsStream.Read( mByte, 1 ) ;  // 88  <- Array
  mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // Size of Array = 2
  mAlias := dgStreamToStr( mArgsStream ) ;  // The first parameter is string, Alias
  mSQL := dgStreamToStr( mArgsStream ) ; //
  mData := TdgsMemTable.Create( Nil ) ;
  CreateDBConnection( mConn, mAlias ) ;
  mQuery := TZQuery.Create( Nil ) ;
  mQuery.Connection := mConn ;
  mError := '--' ;
  dgStrToStream( mError, mResultStream ) ;
  Try
    Try
     mQuery.SQL.Text := mSQL ;
     AddLog( 'Client SQL : ' + mSQL, 1 ) ;
     mQuery.Active := True ;
     CopyStructToMT( TDataSet( mQuery ), mData ) ;
     CopyAllRecord( TDataSet( mQuery ), mData ) ;
     dgStrToStream(  mData.Base64AllData, mResultStream )  ;
    Except
      On E: Exception do
        begin
         mError := 'Server Error : ' + E.Message ;
         mResultStream.clear ;
         dgStrToStream( mError, mResultStream ) ;
        end;
    end;
  finally
    mConn.Free ;
    mQuery.Free ;
    mData.Free ;
  end;
end;

procedure TTCPMServer.NoParamExecSQL(mArgsStream, mResultStream: TMemoryStream);
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mError,mAlias : String ;
  mByte : Byte ;
  mSize : LongInt ;
  mSQL : String ;
  mNoOfRowaffected : Integer ;
begin
  mArgsStream.Read( mByte, 1 ) ;  // 88  <- Array
  mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // Size of Array = 2
  mAlias := dgStreamToStr( mArgsStream ) ;  // The first parameter is string, Alias
  mSQL := dgStreamToStr( mArgsStream ) ; //
  AddLog( 'Client SQL : ' + mSQL, 1 ) ;
  CreateDBConnection( mConn, mAlias ) ;
  mQuery := TZQuery.Create( Nil ) ;
  mQuery.Connection := mConn ;
  mError := '--' ;
  mNoOfRowaffected := 0 ;
  dgStrToStream( mError, mResultStream ) ;
  Try
    Try
     mQuery.SQL.Text := mSQL ;
     mQuery.ExecSQL ;
     mNoOfRowaffected := mQuery.RowsAffected;
    Except
      On E: Exception do
        begin
         mError := 'Server Error : ' + E.Message ;
         mResultStream.clear ;
         dgStrToStream( mError, mResultStream ) ;
        end;
    end;
  finally
    dgIntegerToStream( mNoOfRowaffected,mResultStream ) ;
    mConn.Free ;
    mQuery.Free ;
  end;
end;

procedure TTCPMServer.NoParamInsertWithLastID(mArgsStream, mResultStream: TMemoryStream);
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mError,mAlias : String ;
  mByte : Byte ;
  mSize : LongInt ;
  mSQL,mLastID : String ;
  mData : TdgsMemTable ;
begin
  mArgsStream.Read( mByte, 1 ) ;  // 88  <- Array
  mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // Size of Array = 3
  mAlias := dgStreamToStr( mArgsStream ) ;  // The first parameter is string, Alias
  mSQL := dgStreamToStr( mArgsStream ) ; //
  mLastID := dgStreamToStr( mArgsStream ) ; //
  AddLog( 'Client SQL : ' + mSQL, 1 ) ;
  AddLog( 'Clinet LastID : ' + mLastID, 1 ) ;
  CreateDBConnection( mConn, mAlias ) ;
  mQuery := TZQuery.Create( Nil ) ;
  mQuery.Connection := mConn ;
  mData := TdgsMemTable.Create( Nil ) ;
  mError := '--' ;
  dgStrToStream( mError, mResultStream ) ;
  Try
    Try
     mQuery.SQL.Text := mSQL ;
     mQuery.ExecSQL ;
     // MySQL : select last_insert_id() as id
     mQuery.SQL.Text := mLastID ;
     mQuery.Active := True ;
     CopyStructToMT( TDataSet( mQuery ), mData ) ;
     CopyAllRecord( TDataSet( mQuery ), mData ) ;
     dgStrToStream(  mData.Base64AllData, mResultStream )  ;
    Except
      On E: Exception do
        begin
         mError := 'Server Error : ' + E.Message ;
         mResultStream.clear ;
         dgStrToStream( mError, mResultStream ) ;
        end;
    end;
  finally
    mData.Free ;
    mConn.Free ;
    mQuery.Free ;
  end;
end;

procedure TTCPMServer.ParamSelectSQL(mArgsStream, mResultStream: TMemoryStream);
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mError,mAlias : String ;
  mByte : Byte ;
  mSize : LongInt ;
  mSQL : String ;
  mData,mParams : TdgsMemTable ;
begin
  mArgsStream.Read( mByte, 1 ) ;  // 88  <- Array
  mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // Size of Array = 3
  mAlias := dgStreamToStr( mArgsStream ) ;  // The first parameter is string, Alias
  mSQL := dgStreamToStr( mArgsStream ) ; // The secord parameter is the SQL
  mParams := TdgsMemTable.Create( nil ) ;
  mParams.Base64AllData := dgStreamToStr( mArgsStream ) ; // The third Paramenter is Parameters
  mData := TdgsMemTable.Create( Nil ) ;
  CreateDBConnection( mConn, mAlias ) ;
  mQuery := TZQuery.Create( Nil ) ;
  mQuery.Connection := mConn ;
  mError := '--' ;
  dgStrToStream( mError, mResultStream ) ;
  Try
    Try
     mQuery.SQL.Text := mSQL ;
     SetQueryParams( mQuery, mParams ) ;
     mQuery.Active := True ;
     CopyStructToMT( TDataSet( mQuery ), mData ) ;
     CopyAllRecord( TDataSet( mQuery ), mData ) ;
     dgStrToStream(  mData.Base64AllData, mResultStream )  ;
    Except
      On E: Exception do
        begin
         mError := 'Server Error : ' + E.Message ;
         mResultStream.clear ;
         dgStrToStream( mError, mResultStream ) ;
        end;
    end;
  finally
    mConn.Free ;
    mQuery.Free ;
    mData.Free ;
    mParams.Free ;
  end;
end;

procedure TTCPMServer.ParamExecSQL(mArgsStream, mResultStream: TMemoryStream);
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mError,mAlias : String ;
  mByte : Byte ;
  mSize : LongInt ;
  mSQL : String ;
  mParams : TdgsMemTable ;
  mNoOfRowaffected  : Integer ;
begin
  mArgsStream.Read( mByte, 1 ) ;  // 88  <- Array
  mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // Size of Array = 3
  mAlias := dgStreamToStr( mArgsStream ) ;  // The first parameter is string, Alias
  mSQL := dgStreamToStr( mArgsStream ) ; // The secord parameter is the SQL
  mParams := TdgsMemTable.Create( nil ) ;
  mParams.Base64AllData := dgStreamToStr( mArgsStream ) ; // The third Base64AllData, Params
  CreateDBConnection( mConn, mAlias ) ;
  mQuery := TZQuery.Create( Nil ) ;
  mQuery.Connection := mConn ;
  mError := '--' ;
  mNoOfRowaffected := 0 ;
  dgStrToStream( mError, mResultStream ) ;
    Try
      Try
       mQuery.SQL.Text := mSQL ;
       SetQueryParams( mQuery, mParams ) ;
       mQuery.ExecSQL ;
       mNoOfRowaffected := mQuery.RowsAffected ;
      Except
        On E: Exception do
          begin
           mError := 'Server Error : ' + E.Message ;
           mResultStream.clear ;
           dgStrToStream( mError, mResultStream ) ;
          end;
      end;
    finally
      dgIntegerToStream( mNoOfRowaffected,mResultStream ) ;
      mConn.Free ;
      mQuery.Free ;
      mParams.Free ;
    end;
end;

procedure TTCPMServer.ParamInsertWithLastID(mArgsStream, mResultStream: TMemoryStream);
var
  mConn : TZConnection ;
  mQuery : TZQuery ;
  mError,mAlias : String ;
  mByte : Byte ;
  mSize : LongInt ;
  mSQL,mLastID : String ;
  mParams,mData : TdgsMemTable ;
begin
  mArgsStream.Read( mByte, 1 ) ;  // 88  <- Array
  mArgsStream.Read( mSize, SizeOf( mSize ) ) ;  // Size of Array = 4
  mAlias := dgStreamToStr( mArgsStream ) ;  // The first parameter is string, Alias
  mSQL := dgStreamToStr( mArgsStream ) ; // The secord parameter is the SQL
  mLastID := dgStreamToStr( mArgsStream ) ;
  mParams := TdgsMemTable.Create( nil ) ;
  mParams.Base64AllData := dgStreamToStr( mArgsStream ) ; // The 4th Base64AllData, Params
  CreateDBConnection( mConn, mAlias ) ;
  mQuery := TZQuery.Create( Nil ) ;
  mQuery.Connection := mConn ;
  mData := TdgsMemTable.Create( Self ) ;
  mError := '--' ;
  dgStrToStream( mError, mResultStream ) ;
    Try
      Try
       mQuery.SQL.Text := mSQL ;
       SetQueryParams( mQuery, mParams ) ;
       mQuery.ExecSQL ;
       mQuery.SQL.Text := mLastID ;
       mQuery.Active := True ;
       CopyStructToMT( TDataSet( mQuery ), mData ) ;
       CopyAllRecord( TDataSet( mQuery ), mData ) ;
       dgStrToStream(  mData.Base64AllData, mResultStream )  ;
     Except
        On E: Exception do
          begin
           mError := 'Server Error : ' + E.Message ;
           mResultStream.clear ;
           dgStrToStream( mError, mResultStream ) ;
          end;
      end;
    finally
      mData.Free ;
      mConn.Free ;
      mQuery.Free ;
      mParams.Free ;
    end;
end;

function TTCPMServer.ErrorData(mMsg: String): String;
var
  MT : TdgsMemTable ;
begin
  MT := TdgsMemTable.Create( Self ) ;
  MT.FieldDefs.Add( 'error',ftString,100 ) ;
  MT.Active := True ;
  MT.Append ;
  MT.Edit ;
  MT.FieldByName( 'error' ).AsString := mMsg ;
  MT.Post ;
  Result := MT.Base64AllData ;
  MT.Free ;
end;

procedure TTCPMServer.CreateDBConnection(var mConn: TZConnection; mAlias: String );
var
  mDB,mHost,mUser,mPass,mType,mStr,mLast,mLibPath,mCodePage : String ;
  mPort : Integer ;
  mQuery : TZQuery ;
  mAllSQL,mTemp : TStringList ;
  mCount, i : Integer ;
begin
  mAllSQL := TStringList.Create ;
  mTemp := TStringList.Create ;
  EnterCriticalSection( DBCS ) ;
  Try
    if not WAlias.Locate( 'alias',UpperCase( mAlias ),[] ) then AddLog( 'CreateDBConnection: pAlias.Locate Error : ' + mAlias, 3 ) ;
    mDB := WAlias.FieldByName( 'database' ).AsString ;
    mHost := WAlias.FieldByName( 'host' ).AsString ;
    mUser := WAlias.FieldByName( 'user' ).AsString ;
    mPass := WAlias.FieldByName( 'password' ).AsString ;
    mType := WAlias.FieldByName( 'dbtype' ).AsString ;
    mPort := WAlias.FieldByName( 'port' ).AsInteger ;
    mCodePage := WAlias.FieldByName( 'codepage' ).AsString ;
    mAllSQL.Text := WAlias.FieldByName( 'sql' ).AsString ;
    mLibPath := WAlias.FieldByName( 'libpath' ).AsString ;
    AddLog( mAllSQL.Text, 3 ) ;
  finally
    LeaveCriticalSection( DBCS ) ;
  end;
  mConn := TZConnection.Create( Self ) ;
  mConn.Database := mDB ;
  mConn.HostName := mHost ;
  mConn.User := mUser ;
  mConn.Password := mPass ;
  mConn.Protocol := mType ;
  mConn.Port := mPort ;
  mConn.LibraryLocation := mLibPath ;
  if UpperCase( mCodePage ) = 'UTF8' then
    mConn.ControlsCodePage := cCP_UTF8 ;
  if UpperCase( mCodePage ) = 'UTF16' then
    mConn.ControlsCodePage := cCP_UTF16 ;
  mConn.Connect ;
  mQuery := TZQuery.Create( nil ) ;
  mQuery.Connection := mConn ;
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
  mQuery.Free ;
  mTemp.Free ;
  mAllSQL.Free ;
end;


procedure TTCPMServer.SetQueryParams(var mQuery: TZQuery; var mParams: TdgsMemTable);
var
  mCount, i : Integer ;
  mFieldType : TFieldType ;
  mParaName : String ;
  mFieldItems : TFieldDef ;
  ms : TMemoryStream ;
  mDBType : String ;
  mMYSQL : Boolean ;
begin
  mDBType := UpperCase( mQuery.Connection.Protocol ) ;
  if pos( 'MYSQL',mDBType ) >= 0 then
     mMYSQL := True
  else
     mMYSQL := False ;
  mCount := mQuery.Params.Count ;
  for i := 0 to mCount - 1 do
    begin
    mParaName := mQuery.Params.Items[ i ].Name ;
    mFieldItems := mParams.FieldDefs.Find( mParaName ) ;
    mFieldType := mFieldItems.DataType ;
    if mFieldType = ftString then
      mQuery.Params.Items[ i ].AsString := mParams.FieldByName( mParaName ).AsString ;
    if mFieldType = ftFloat then
      mQuery.Params.Items[ i ].AsFloat := mParams.FieldByName( mParaName ).AsFloat ;
    if mFieldType = ftInteger then
      mQuery.Params.Items[ i ].AsInteger := mParams.FieldByName( mParaName ).AsInteger ;
    if mFieldType = ftDateTime then
      mQuery.Params.Items[ i ].AsDateTime := mParams.FieldByName( mParaName ).AsDateTime ;
    if mFieldType = ftDate then
      mQuery.Params.Items[ i ].AsDate := mParams.FieldByName( mParaName ).AsDateTime ;
    if mFieldType = ftBoolean then
      begin
      if mMYSQL then
        mQuery.Params.Items[ i ].AsInteger := dgBoolToInt( mParams.FieldByName( mParaName ).AsBoolean )
      else
        mQuery.Params.Items[ i ].AsBoolean := mParams.FieldByName( mParaName ).AsBoolean ;
      end;
    if mFieldType = ftCurrency then
      mQuery.Params.Items[ i ].AsFloat := mParams.FieldByName( mParaName ).AsFloat ;
    if mFieldType = ftMemo then
      mQuery.Params.Items[ i ].AsMemo := mParams.FieldByName( mParaName ).AsString  ;
    if mFieldType = ftBlob then
      begin
      ms := TMemoryStream.Create ;
      TBlobField( mParams.FieldByName( mParaName ) ).SaveToStream( ms ) ;
      ms.Position := 0 ;
      mQuery.Params.Items[ i ].LoadFromStream( ms, ftBlob ) ;
      ms.Free ;
      end;
    end;
end;

function TTCPMServer.GetDataPathFileList(mDataPath, mPattern: String) : String ;
var
  mfl,mList : TStringList ;
  i,mCount : Integer ;
begin
  mList := TStringList.Create ;
  mfl := FindAllFiles( mDataPath,mPattern,True ) ;
  mCount := mfl.Count ;
  for i := 0 to mCount - 1 do
    mList.Add( ExtractFileName( mfl.Strings[ i ] )  ) ;
  Result := mList.Text ;
  mfl.Free ;
  mList.Free ;
end;

procedure TTCPMServer.AddLog(mMessage: String ; mLevel : Integer );
begin
  if pLogMode then
    begin
    if pLogLevel >= mLevel then
      pLogMessage( mMessage ) ;
    end;
end;

end.

