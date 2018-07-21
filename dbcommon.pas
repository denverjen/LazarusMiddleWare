unit dbcommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, BufDataset, Base64 ;

Type

   { TdgsMemTable }

   TdgsMemTable = class( TBufDataSet )
     procedure LoadFromDataSet( mDataSet : TDataSet ) ;
     function SaveToBase64String : String ;
     procedure LoadFromBase64String( mBase64 : String ) ;
     procedure EmtpyTable ;
     published
      property Base64AllData : String read SaveToBase64String write LoadFromBase64String ;
      Constructor Create(AOwner:TComponent); override;
      Destructor Destroy; override;
   end;

Type

    { TdgsParams }

    TdgsParams = Class( TComponent )
     procedure AddString( AName, AValue : String ) ;
     procedure AddInteger( AName : String ; AValue : Integer ) ;
     procedure AddLongInteger( AName : String ; AValue : LongInt ) ;
     procedure AddFloat( AName : String ; AValue : Real ) ;
     procedure AddCurrency( AName : String ; AValue : Currency ) ;
     procedure AddBlobFromStream( AName : String ; AStream : TMemoryStream ) ;
     procedure AddBlobFromStream( AName : String ; AStream : TMemoryStream ; ASize : LongInt ) ;
     procedure AddDateTime( AName : String ; AValue : TDateTime ) ;
     procedure AddBoolean( AName : String ; AValue : Boolean ) ;
     procedure AddMemo( AName : String ; AValue : String ) ;
     function Base64AllData : String ;
   private
     DataStream : TMemoryStream ;
     DataName : TdgsMemTable ;
     NoOfParams : Integer ;
     procedure AddNameType( AName, AType : String ; ALen : Integer ) ;
   published
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
   end;

procedure CopyStructToMT( var mDataSet: TDataSet; var mMem: TdgsMemtable);
procedure CopyOneRecord(var mDataSet: TDataSet; var mMem: TdgsMemtable);
procedure CopyAllRecord(var mDataSet: TDataSet; var mMem: TdgsMemtable);
procedure CopyOneMemRecord(var mDataSet, mMem: TdgsMemtable);
procedure CopyAllMemRecord(var mDataSet, mMem: TdgsMemtable);
procedure CopyMemTableStructure( var mDest , mSource : TdgsMemtable ; mList : String ) ;
procedure CopyMemTableFullStructure( var mDest , mSource : TdgsMemtable ) ;
function Base64StreamToString( AStream : TMemoryStream ; ASize : LongInt ) : String ;
procedure Base64StringToStream( ABase64String : String ; AStream : TMemoryStream ) ;

implementation

uses
  dgfunc ;

procedure CopyStructToMT(var mDataSet: TDataSet; var mMem: TdgsMemtable);
var
  lCount,i,mLen : Integer ;
  mFieldType : TFieldType ;
  mFieldName : String ;
begin
  lCount := mDataSet.FieldCount ;
  for i := 0 to lCount - 1 do
    begin
    mFieldType := mDataSet.FieldDefs[ i ].DataType ;
    mFieldName := mDataSet.Fields[ i ].FieldName ;
    if mFieldType = ftLargeInt	then
      mMem.FieldDefs.Add( mFieldName,ftLargeInt ) ;
    if mFieldType = ftString then
      begin
      mLen := mDataSet.FieldDefs.Items[ i ].Size ;
      mMem.FieldDefs.Add( mFieldName,ftString,mLen ) ;
      end ;
    if mFieldType = ftInteger then
      mMem.FieldDefs.Add( mFieldName,ftInteger ) ;
    if mFieldType = ftSmallint	 then
      mMem.FieldDefs.Add( mFieldName,ftSmallint	) ;
    if mFieldType = ftWord then
      mMem.FieldDefs.Add( mFieldName,ftWord ) ;
    if mFieldType = ftDate then
      mMem.FieldDefs.Add( mFieldName,ftDate ) ;
    if mFieldType = ftTime then
      mMem.FieldDefs.Add( mFieldName,ftTime ) ;
    if mFieldType = ftDateTime then
      mMem.FieldDefs.Add( mFieldName,ftDateTime ) ;
    if mFieldType = ftBCD then
      mMem.FieldDefs.Add( mFieldName,ftFloat ) ;
    if mFieldType = ftFloat then
      mMem.FieldDefs.Add( mFieldName,ftFloat ) ;
    if mFieldType = ftCurrency then
      mMem.FieldDefs.Add( mFieldName,ftCurrency	 ) ;
    if mFieldType = ftAutoInc	then
      mMem.FieldDefs.Add( mFieldName,ftInteger ) ;
    if mFieldType = ftMemo	then
      mMem.FieldDefs.Add( mFieldName,ftMemo ) ;
    if mFieldType = ftBlob then
      mMem.FieldDefs.Add( mFieldName,ftBlob ) ;
    if mFieldType = ftFMTBcd then
      mMem.FieldDefs.Add( mFieldName,ftFloat ) ;
    end ;
  mMem.CreateDataset ;
end;

procedure CopyOneRecord(var mDataSet: TDataSet; var mMem: TdgsMemtable);
var
  i,lCount : Integer ;
  mFieldName : String ;
  mFieldType : TFieldType ;
begin
 lCount := mMem.FieldCount ;
  for i := 0 to lCount - 1 do
    begin
    mFieldName := mMem.Fields[ i ].FieldName ;
    mFieldType := mMem.FieldDefs[ i ].DataType ;
    if mFieldType = ftFloat then
      mMem.FieldByName( mFieldName ).AsFloat := mDataSet.FieldByName( mFieldName ).AsFloat
    else
      mMem.FieldByName( mFieldName ).Assign( mDataSet.FieldByName( mFieldName ) ) ;
    end ;
end;

procedure CopyAllRecord(var mDataSet: TDataSet; var mMem: TdgsMemtable);
var
  lCount : Integer ;
begin
  lCount := mDataSet.RecordCount ;
  mDataSet.First ;
  while not mDataSet.Eof do
    begin
    mMem.Append ;
    mMem.Edit ;
    CopyOneRecord( mDataSet,mMem ) ;
    mMem.Post ;
    mDataSet.Next ;
    end ;
end;

procedure CopyOneMemRecord(var mDataSet, mMem: TdgsMemtable);
var
  i,lCount : Integer ;
  mFieldName : String ;
  mFieldType : TFieldType ;
begin
 lCount := mMem.FieldCount ;
  for i := 0 to lCount - 1 do
    begin
    mFieldName := mMem.Fields[ i ].FieldName ;
    mFieldType := mMem.FieldDefs[ i ].DataType ;
    if mFieldType = ftFloat then
      mMem.FieldByName( mFieldName ).AsFloat := mDataSet.FieldByName( mFieldName ).AsFloat
    else
      mMem.FieldByName( mFieldName ).Assign( mDataSet.FieldByName( mFieldName ) ) ;
    end ;
end;

procedure CopyAllMemRecord(var mDataSet, mMem: TdgsMemtable);
var
  lCount : Integer ;
begin
  lCount := mDataSet.RecordCount ;
  mDataSet.First ;
  while not mDataSet.Eof do
    begin
    mMem.Append ;
    mMem.Edit ;
    CopyOneMemRecord( mDataSet,mMem ) ;
    mMem.Post ;
    mDataSet.Next ;
    end ;
end;

procedure CopyMemTableStructure(var mDest, mSource: TdgsMemtable; mList: String );
var
  mFldList : TStringList ;
  mFieldName : String ;
  mFldCount, i,mLen,mIndex : Integer ;
  mFieldType : TFieldType ;
begin
  mFldList := TStringList.Create ;
  mFldList.Text := mList ;
  mFldCount := mFldList.Count ;
  for i := 0 to mFldCount - 1 do
    begin
    mFieldName := mFldList.Strings[ i ] ;
    mIndex := mSource.FieldDefs.IndexOf( mFieldName ) ;
    if mIndex >= 0 then
      begin
      mFieldType := mSource.FieldDefs.Items[ mIndex ].DataType ;
      if mFieldType = ftLargeInt	then
        mDest.FieldDefs.Add( mFieldName,ftLargeInt ) ;
      if mFieldType = ftString then
        begin
        mLen := mSource.FieldDefs.Items[ mIndex ].Size ;
        mDest.FieldDefs.Add( mFieldName,ftString,mLen ) ;
        end ;
      if mFieldType = ftInteger then
        mDest.FieldDefs.Add( mFieldName,ftInteger ) ;
      if mFieldType = ftSmallint	 then
        mDest.FieldDefs.Add( mFieldName,ftSmallint	) ;
      if mFieldType = ftWord then
        mDest.FieldDefs.Add( mFieldName,ftWord ) ;
      if mFieldType = ftDate then
        mDest.FieldDefs.Add( mFieldName,ftDate ) ;
      if mFieldType = ftTime then
        mDest.FieldDefs.Add( mFieldName,ftTime ) ;
      if mFieldType = ftDateTime then
        mDest.FieldDefs.Add( mFieldName,ftDateTime ) ;
      if mFieldType = ftFloat then
        mDest.FieldDefs.Add( mFieldName,ftFloat ) ;
      if mFieldType = ftCurrency then
        mDest.FieldDefs.Add( mFieldName,ftCurrency	 ) ;
      if mFieldType = ftAutoInc	then
        mDest.FieldDefs.Add( mFieldName,ftInteger ) ;
      if mFieldType = ftMemo	then
        mDest.FieldDefs.Add( mFieldName,ftMemo ) ;
      if mFieldType = ftFmtMemo	then
        mDest.FieldDefs.Add( mFieldName,ftMemo ) ;
      if mFieldType = ftBlob then
        mDest.FieldDefs.Add( mFieldName,ftBlob ) ;
      if mFieldType = ftBCD then
        mDest.FieldDefs.Add( mFieldName,ftFloat ) ;
      end;
    end ;
  mFldList.Free ;
end;

procedure CopyMemTableFullStructure(var mDest, mSource: TdgsMemtable);
var
  mFieldName : String ;
  mFldCount, i,mLen,mIndex : Integer ;
  mFieldType : TFieldType ;
begin
  mFldCount := mSource.FieldDefs.Count ;
  for i := 0 to mFldCount - 1 do
    begin
    mFieldName := mSource.FieldDefs.Items[ i ].Name ;
    mIndex := mSource.FieldDefs.IndexOf( mFieldName ) ;
    if mIndex >= 0 then
      begin
      mFieldType := mSource.FieldDefs.Items[ mIndex ].DataType ;
      if mFieldType = ftLargeInt	then
        mDest.FieldDefs.Add( mFieldName,ftLargeInt ) ;
      if mFieldType = ftString then
        begin
        mLen := mSource.FieldDefs.Items[ mIndex ].Size ;
        mDest.FieldDefs.Add( mFieldName,ftString,mLen ) ;
        end ;
      if mFieldType = ftInteger then
        mDest.FieldDefs.Add( mFieldName,ftInteger ) ;
      if mFieldType = ftSmallint	 then
        mDest.FieldDefs.Add( mFieldName,ftSmallint	) ;
      if mFieldType = ftWord then
        mDest.FieldDefs.Add( mFieldName,ftWord ) ;
      if mFieldType = ftDate then
        mDest.FieldDefs.Add( mFieldName,ftDate ) ;
      if mFieldType = ftTime then
        mDest.FieldDefs.Add( mFieldName,ftTime ) ;
      if mFieldType = ftDateTime then
        mDest.FieldDefs.Add( mFieldName,ftDateTime ) ;
      if mFieldType = ftFloat then
        mDest.FieldDefs.Add( mFieldName,ftFloat ) ;
      if mFieldType = ftCurrency then
        mDest.FieldDefs.Add( mFieldName,ftCurrency	 ) ;
      if mFieldType = ftAutoInc	then
        mDest.FieldDefs.Add( mFieldName,ftInteger ) ;
      if mFieldType = ftMemo	then
        mDest.FieldDefs.Add( mFieldName,ftMemo ) ;
      if mFieldType = ftFmtMemo	then
        mDest.FieldDefs.Add( mFieldName,ftMemo ) ;
      if mFieldType = ftBlob then
        mDest.FieldDefs.Add( mFieldName,ftBlob ) ;
      if mFieldType = ftBoolean then
        mDest.FieldDefs.Add( mFieldName,ftBoolean ) ;
      if mFieldType = ftFMTBcd then
        mDest.FieldDefs.Add( mFieldName,ftFloat ) ;
      end;
    end ;
end;

function Base64StreamToString(AStream: TMemoryStream; ASize: LongInt): String;
var
  EncodedStream : TStringStream ;
  Encoder : TBase64EncodingStream ;
begin
  EnCodedStream := TStringStream.Create( '' ) ;
  Encoder := TBase64EncodingStream.Create( EncodedStream ) ;
  Try
    Encoder.CopyFrom( AStream, ASize );
    Encoder.Flush ;
    Result := EncodedStream.DataString ;
  finally
    Encoder.Free ;
    EnCodedStream.Free ;
  end;

end;

procedure Base64StringToStream(ABase64String: String; AStream: TMemoryStream);
var
  EncodedStream : TStringStream ;
  Decoder : TBase64DecodingStream ;
begin
  EncodedStream := TStringStream.Create( ABase64String ) ;
  Try
    Try
      Decoder := TBase64DecodingStream.Create( EncodedStream ) ;
      AStream.CopyFrom(Decoder, Decoder.Size);
    finally
      Decoder.Free ;
    end;
  finally
    EncodedStream.Free ;
  end;
end;

{ TdgsParams }

procedure TdgsParams.AddString(AName, AValue: String );
var
  mLen : Integer ;
begin
  mLen := Length( AValue ) ;
  Inc( NoOfParams ) ;
  AddNameType( AName, 'string', mLen + 2 ) ;
  dgStrToStream( AValue, DataStream ) ;
end;

procedure TdgsParams.AddInteger(AName: String; AValue: Integer);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'integer', 0 ) ;
  dgIntegerToStream( AValue, DataStream ) ;
end;

procedure TdgsParams.AddLongInteger(AName: String; AValue: LongInt);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'longint', 0 ) ;
  dgIntegerToStream( AValue, DataStream ) ;
end;

procedure TdgsParams.AddFloat(AName: String; AValue: Real);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'float', 0 ) ;
  dgFloatToStream( AValue, DataStream ) ;
end;

procedure TdgsParams.AddCurrency(AName: String; AValue: Currency);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'currency', 0 ) ;
  dgCurrencyToStream( AValue , DataStream ) ;
end;

procedure TdgsParams.AddBlobFromStream(AName: String; AStream: TMemoryStream);
var
  mSize : LongInt ;
begin
  mSize := AStream.Size ;
  Inc( NoOfParams ) ;
  AddNameType( AName, 'blob', mSize ) ;
  dgStreamCopyToStream( AStream, DataStream, mSize ) ;
end;

procedure TdgsParams.AddBlobFromStream(AName: String; AStream: TMemoryStream;  ASize: LongInt);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'blob', ASize ) ;
  dgStreamCopyToStream( AStream, DataStream, ASize ) ;
end;

procedure TdgsParams.AddDateTime(AName: String; AValue: TDateTime);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'datetime', 0 ) ;
  dgDateTimeToStream( AValue, DataStream ) ;
end;

procedure TdgsParams.AddBoolean(AName: String; AValue: Boolean);
begin
  Inc( NoOfParams ) ;
  AddNameType( AName, 'boolean', 0 ) ;
  dgBooleanToStream( AValue, DataStream ) ;
end;

procedure TdgsParams.AddMemo(AName: String; AValue: String);
var
  mLen : Integer ;
begin
  mLen := Length( AValue ) ;
  Inc( NoOfParams ) ;
  AddNameType( AName, 'memo', mLen ) ;
  dgStrToStream( AValue, DataStream ) ;
end;

function TdgsParams.Base64AllData: String;
var
  MT : TdgsMemTable ;
  mName,mType : String ;
  mLen : LongInt ;
  mStream : TMemoryStream ;
begin
  MT := TdgsMemTable.Create( Self ) ;
  DataName.First ;
  while not DataName.Eof do
    begin
    mName := DataName.FieldByName( 'pname' ).AsString ;
    mType := DataName.FieldByName( 'ptype' ).AsString ;
    mLen := DataName.FieldByName( 'psize' ).AsLargeInt ;
    if mType = 'string' then
      MT.FieldDefs.Add( mName, ftString, mLen ) ;
    if mType = 'integer' then
      MT.FieldDefs.Add( mName, ftInteger ) ;
    if mType = 'longint' then
      MT.FieldDefs.Add( mName, ftLargeInt ) ;
    if mType = 'float' then
      MT.FieldDefs.Add( mName, ftFloat ) ;
    if mType = 'currency' then
      MT.FieldDefs.Add( mName, ftCurrency ) ;
    if mType = 'blob' then
      MT.FieldDefs.Add( mName, ftBlob ) ;
    if mType = 'datetime' then
      MT.FieldDefs.Add( mName, ftDateTime ) ;
    if mType = 'boolean' then
      MT.FieldDefs.Add( mName,ftBoolean ) ;
    if mType = 'memo' then
      MT.FieldDefs.Add( mName, ftMemo ) ;
    DataName.Next ;
    end;
  MT.CreateDataset ;
  MT.Active := True ;
  MT.Append ;
  MT.Edit ;
  DataStream.Position := 0 ;
  DataName.First ;
  while not DataName.Eof do
    begin
    mName := DataName.FieldByName( 'pname' ).AsString ;
    mType := DataName.FieldByName( 'ptype' ).AsString ;
    mLen := DataName.FieldByName( 'psize' ).AsLargeInt ;
    if mType = 'string' then
      MT.FieldByName( mName ).AsString := dgStreamtoStr( DataStream ) ;
    if mType = 'integer' then
      MT.FieldByName( mName ).AsInteger := dgStreamToInteger( DataStream ) ;
    if mType = 'longint' then
      MT.FieldByName( mName ).AsLargeInt := dgStreamToLongInt( DataStream ) ;
    if mType = 'float' then
      MT.FieldByName( mName ).AsFloat := dgStreamToFloat( DataStream ) ;
    if mType = 'currency' then
      MT.FieldByName( mName ).AsCurrency := dgStreamToCurrency( DataStream ) ;
    if mType = 'blob' then
      begin
      mStream := TMemoryStream.Create ;
      dgStreamLoadFromStream( DataStream, mStream ) ;
      mStream.Position := 0 ;
      TBlobField( MT.FieldByName( mName ) ).LoadFromStream( mStream ) ;
      mStream.Free ;
      end;
    if mType = 'datetime' then
      MT.FieldByName( mName ).AsDateTime := dgStreamToDateTime( DataStream ) ;
    if mType = 'boolean' then
      MT.FieldByName( mName ).AsBoolean := dgStreamToBoolean( DataStream ) ;
    if mType = 'memo' then
      MT.FieldByName( mName ).AsString := dgStreamToStr( DataStream ) ;
    DataName.Next ;
    end;
  MT.Post ;
  Result := MT.Base64AllData ;
  MT.Free ;
end;

procedure TdgsParams.AddNameType( AName, AType: String ; ALen : Integer );
begin
  DataName.Append ;
  DataName.Edit ;
  DataName.FieldByName( 'pname' ).AsString := AName ;
  DataName.FieldByName( 'ptype' ).AsString := AType ;
  DataName.FieldByName( 'psize' ).AsInteger := ALen ;
  DataName.Post ;
end;

constructor TdgsParams.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  NoOfParams := 0 ;
  DataStream := TMemoryStream.Create ;
  DataName := TdgsMemTable.Create( Self ) ;
  DataName.FieldDefs.Add( 'pname',ftString,80 ) ;
  DataName.FieldDefs.Add( 'ptype',ftString,20 ) ;
  DataName.FieldDefs.Add( 'psize',ftLargeInt ) ;
  DataName.CreateDataset ;
end;

destructor TdgsParams.Destroy;
begin
  DataName.Free ;
  DataStream.Free ;
  inherited Destroy;
end;

{ TdgsMemTable }

procedure TdgsMemTable.LoadFromDataSet(mDataSet: TDataSet);
begin
  CopyStructToMT( mDataSet, Self ) ;
  CopyAllRecord( mDataSet, Self ) ;
end;

function TdgsMemTable.SaveToBase64String : String ;
var
  AStream : TMemoryStream ;
  ASize : LongInt ;
begin
  AStream := TMemoryStream.Create ;
  Self.SaveToStream( AStream ) ;
  ASize := AStream.Size ;
  AStream.Position := 0 ;
  Try
    Result := Base64StreamToString( AStream, ASize ) ;
  finally
    AStream.Free ;
  end;
end;

procedure TdgsMemTable.LoadFromBase64String( mBase64 : String ) ;
var
  ms : TMemoryStream ;
begin
  ms := TMemoryStream.Create ;
  Base64StringToStream( mBase64, ms ) ;
  ms.Position := 0 ;
  Clear ;
  Self.LoadFromStream( ms ) ;
  ms.Free ;
end;

procedure TdgsMemTable.EmtpyTable;
var
  MT : TdgsMemTable ;
begin
  MT := TdgsMemTable.Create( Nil ) ;
  CopyMemTableFullStructure( MT, Self ) ;
  MT.CreateDataset ;
  Clear ;
  CopyMemTableFullStructure( Self, MT ) ;
  CreateDataset ;
  MT.Free ;
end;


constructor TdgsMemTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TdgsMemTable.Destroy;
begin
  inherited Destroy;
end;

end.

