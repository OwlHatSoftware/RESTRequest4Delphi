unit upiperestserverwrapper.iface;

interface

uses
  System.JSON;

type
  TOnGetJSONEvent = procedure(Sender: TObject; const AEndPoint: string; var AJSONObject: TJSONObject) of object;
  TOnPostJSONEvent = procedure(Sender: TObject; const AEndPoint, AJSON: String; var ASuccess: boolean) of object;
  IPipeRESTServerWrapper = interface
    function Start(const APipeName: string): boolean;
    function Stop: boolean;
    procedure BroadCastMessage(const AMessage: string); overload;
    procedure BroadCastMessage(const AJSONObject: TJSONObject); overload;
    function GetServerName: string;
    function GetOnGetJSON: TOnGetJSONEvent;
    procedure SetOnGetJSON(const Value: TOnGetJSONEvent);
    function GetOnPostJSON: TOnPostJSONEvent;
    procedure SetOnPostJSON(const Value: TOnPostJSONEvent);
    property OnGetJSON: TOnGetJSONEvent read GetOnGetJSON write SetOnGetJSON;
    property OnPostJSON: TOnPostJSONEvent read GetOnPostJSON write SetOnPostJSON;
  end;

var
  PipeRESTServer: IPipeRESTServerWrapper;

implementation

uses
  upiperestserverwrapper.impl;

function CreatePipeRESTServer: IPipeRESTServerWrapper;
begin
  result := TPipeRESTServerWrapper.Create;
end;

initialization

PipeRestServer := CreatePipeRESTServer;

end.
