unit RESTRequest4D;

interface

uses
  {$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_NETHTTP))}
    REST.Types,
  {$ENDIF}
  RESTRequest4D.Request.Contract, RESTRequest4D.Response.Contract, RESTRequest4D.Request.Adapter.Contract;

type
  IRequest = RESTRequest4D.Request.Contract.IRequest;
  IRequestAdapter = RESTRequest4D.Request.Adapter.Contract.IRequestAdapter;
  IResponse = RESTRequest4D.Response.Contract.IResponse;

  TRequest = class
  public
    class function New: IRequest;
  end;

{$IF NOT (DEFINED(RR4D_INDY) or DEFINED(FPC) or DEFINED(RR4D_NETHTTP))}
const
  poDoNotEncode = REST.Types.poDoNotEncode;
  poTransient = REST.Types.poTransient;
  poAutoCreated = REST.Types.poAutoCreated;
  {$IF COMPILERVERSION >= 33}
    poFlatArray = REST.Types.poFlatArray;
    poPHPArray = REST.Types.poPHPArray;
    poListArray = REST.Types.poListArray;
  {$ENDIF}

  pkCOOKIE = REST.Types.pkCOOKIE;
  pkGETorPOST = REST.Types.pkGETorPOST;
  pkURLSEGMENT = REST.Types.pkURLSEGMENT;
  pkHTTPHEADER = REST.Types.pkHTTPHEADER;
  pkREQUESTBODY = REST.Types.pkREQUESTBODY;
  {$IF COMPILERVERSION >= 32}
    pkFILE = REST.Types.pkFILE;
  {$ENDIF}
  {$IF COMPILERVERSION >= 33}
    pkQUERY = REST.Types.pkQUERY;
  {$ENDIF}
{$ENDIF}

implementation

uses
  {$IF DEFINED(FPC) and (not DEFINED(RR4D_INDY)) and (not DEFINED(RR4D_SYNAPSE))}
    RESTRequest4D.Request.FPHTTPClient
  {$ELSEIF DEFINED(RR4D_INDY)}
    RESTRequest4D.Request.Indy
  {$ELSEIF DEFINED(RR4D_NETHTTP)}
    RESTRequest4D.Request.NetHTTP
  {$ELSEIF DEFINED(RR4D_SYNAPSE)}
    RESTRequest4D.Request.Synapse
  {$ELSEIF DEFINED(RR4D_ICS)}
    RESTRequest4D.Request.ICS
  {$ELSE}
    {$IF DEFINED(RR4D_CLIENT)}
      RESTRequest4D.Request.Client
    {$ELSEIF DEFINED(RR4D_PIPES)}
      RESTRequest4D.Request.PipesClient
    {$ENDIF}
  {$ENDIF}
  ;

class function TRequest.New: IRequest;
begin
  {$IF DEFINED(FPC) and (not DEFINED(RR4D_INDY)) and (not DEFINED(RR4D_SYNAPSE))}
    Result := TRequestFPHTTPClient.New;
  {$ELSEIF DEFINED(RR4D_INDY)}
    Result := TRequestIndy.New;
  {$ELSEIF DEFINED(RR4D_NETHTTP)}
    Result := TRequestNetHTTP.New;
  {$ELSEIF DEFINED(RR4D_SYNAPSE)}
    Result := TRequestSynapse.New;
  {$ELSEIF DEFINED(RR4D_ICS)}
    Result := TRequestICS.New;
  {$ELSE}
    {$IF DEFINED(RR4D_CLIENT)}
    Result := TRequestClient.New;
    {$ELSEIF DEFINED(RR4D_PIPES)}
     Result := TRequestPipes.New;
     {$ENDIF}
  {$ENDIF}
end;

end.