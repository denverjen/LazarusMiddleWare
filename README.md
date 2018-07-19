# LazarusMiddleWare

Version 0.2.2

Simple Middle Ware for Lazarus. Read and Update Dataset via TCP/IP 
Inlcude Server Side Component and Client Side Component.
This project using Lazarus 1.8.4 or above.

Note : Use Lazarus Online Package Manager to install Indy10 and ZeosDBO. 

Server Side Usage : 
  mServer := TTCPMServer.Create( Self ) ;
  mServer.pExePath := ExtractFilePath(  ParamStr( 0 )  ) ;
  mServer.pLogMessage := @LogMessage ;
  mServer.pLogMode := True ;
  mServer.pLogLevel := 5 ; // All Log
  mServer.pPort := 6999 ; // Server Port ;
  mServer.SetAlias( 'testdb','mysqld-5','127.0.0.1','testrun','testpass','horse','utf8','',
  '/usr/lib/x86_64-linux-gnu/libmysqlclient.so.20.3.9','uusdfsd',3306 ) ; 
  mServer.StartServer

  
Client Side Usage :
  mClient.pServerHost := '127.0.0.1' ;
  mClient.pServerAlias := 'testdb' ;
  mClient.pServerPort := 6999 ;
  mClient.pServerKey := 'uusdfsd' ;
  mClient.Connect ;   
  MT.Base64AllData := mClient.DBSelect( 'select id, wincount from raceday order by id desc limit 10' ) ; 
  
   // or

   
  mClient.DBExecSQL( 'update raceday set wincount = 10 where id = 103'  ); 
  lazmiddleservermain.pas shows the usage. 
  
  This sample is using MYSQL as database backend. 
  
  
