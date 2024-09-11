unit RESTRequest4D.Response.PipesClient;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses Classes, SysUtils, RESTRequest4D.Response.Contract,
  {$IFDEF FPC}
    fpjson, jsonparser;
  {$ELSE}
    System.Json;
  {$ENDIF}

type
  TResponsePipes = class(TInterfacedObject, IResponse)
  private
    FJSONValue: {$IFDEF FPC}TJSONData;{$ELSE}TJSONValue;{$ENDIF}
    FStreamResult: TStringStream;
    function Content: string;
    function ContentLength: Cardinal;
    function ContentType: string;
    function ContentEncoding: string;
    function ContentStream: TStream;
    function StatusCode: Integer;
    function StatusText: string;
    function RawBytes: TBytes;
    function Headers: TStrings;
    {$IFDEF FPC}
      function JSONValue: TJSONData;
    {$ELSE}
      function JSONValue: TJSONValue; overload;
      function JSONValue(const AEncoding: TEncoding): TJSONValue; overload;
    {$ENDIF}
    function GetCookie(const ACookieName: string): string;
  public
    constructor Create(AData: string);
    destructor Destroy; override;
  end;

implementation

function TResponsePipes.Content: string;
begin
  Result := FStreamResult.DataString;
end;

function TResponsePipes.ContentLength: Cardinal;
begin
  Result := 0;
end;

function TResponsePipes.ContentType: string;
begin
  Result := '';
end;

function TResponsePipes.ContentEncoding: string;
begin
  Result := '';
end;

function TResponsePipes.ContentStream: TStream;
begin
  Result := FStreamResult;
  Result.Position := 0;
end;

function TResponsePipes.StatusCode: Integer;
begin
  Result := 0;
end;

function TResponsePipes.StatusText: string;
begin
  Result := '';
end;

function TResponsePipes.RawBytes: TBytes;
begin
  Result := FStreamResult.Bytes;
end;

{$IFDEF FPC}
function TResponsePipes.JSONValue: TJSONData;
var
  LContent: string;
  LJSONParser: TJSONParser;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
    LJSONParser := TJSONParser.Create(LContent, False);
    try
      if LContent.StartsWith('{') then
        FJSONValue := LJSONParser.Parse as TJSONObject
      else if LContent.StartsWith('[') then
        FJSONValue := LJSONParser.Parse as TJSONArray
      else
        raise Exception.Create('The return content is not a valid JSON value.');
    finally
      LJSONParser.Free;
    end;
  end;
  Result := FJSONValue;
end;
{$ELSE}
function TResponsePipes.JSONValue: TJSONValue;
begin
  Result := Self.JSONValue(TEncoding.UTF8);
end;

function TResponsePipes.JSONValue(const AEncoding: TEncoding): TJSONValue;
var
  LContent: string;
begin
  if not(Assigned(FJSONValue)) then
  begin
    LContent := Content.Trim;
    if LContent.StartsWith('{') then
      FJSONValue := (TJSONObject.ParseJSONValue(AEncoding.GetBytes(LContent), 0) as TJSONObject)
    else if LContent.StartsWith('[') then
      FJSONValue := (TJSONObject.ParseJSONValue(AEncoding.GetBytes(LContent), 0) as TJSONArray)
    else
      raise Exception.Create('The return content is not a valid JSON value.');
  end;
  Result := FJSONValue;
end;
{$ENDIF}

function TResponsePipes.Headers: TStrings;
begin
  Result := TStrings.Create;
end;

constructor TResponsePipes.Create(AData: string);
begin
  FStreamResult := TStringStream.Create(AData);
end;

destructor TResponsePipes.Destroy;
begin
  FreeAndNil(FStreamResult);
  if Assigned(FJSONValue) then
    FJSONValue.Free;
  inherited Destroy;
end;

function TResponsePipes.GetCookie(const ACookieName: string): string;
begin
  Result := 'not implemented!';
end;

end.
