{*
 * Channels - Linked Lists used to store data about Channels/Clans
 * Copyright (C) 2005 Chris "MD87" Smith, Shane "Dataforce" Mc Cormack
 * For conditions of distribution and use, see copyright notice in license.txt
 *}
unit Channels;

interface

uses SysUtils, {$ifdef win32}WSocket{$else}LSocket{$endif}, UserSocket;

type
  {$ifdef win32}
    TSocket = TWSocket;
  {$else}
    TSocket = TLSocket;
  {$endif}

  TWOL_ChannelClient =  class(tobject)// Channel Clients LinkedList Type
    User: TWOL_Socket;          // The client
    Mode: Array [0..1] of char;
    GameOpt: string;

    Prev: TWOL_ChannelClient;     // Pointer to previous channel (If not first)
    Next: TWOL_ChannelClient;     // Pointer to next channel (If not last)
  end;

  TWOL_ChannelData =  class(tobject) // Clients LinkedList Type
    Name: String;
    Topic: String;
    Password: String;
    IsGame: Boolean;
    Hidden: boolean;
    Locked: boolean;
    UnLockRequested: Boolean;
    IsTournament: boolean;
    GameOwner: TWOL_Socket;
    GameOptions: Word;
    JoinInfo: String;
    ChanMode: Integer;

    Admin: Boolean;

    Prev: TWOL_ChannelData;          // Pointer to previous client (If not first)
    Next: TWOL_ChannelData;          // Pointer to next client (If not last)


    ChannelClientTotal: integer;         // Total number of ChannelClients
    FFirstChannelClient: TWOL_ChannelClient;     // Pointer to first ChannelClient in the list
    FLastChannelClient: TWOL_ChannelClient;      // Pointer to last ChannelClient in the list

//    procedure Send(Line: String);

    procedure InitChannelClients();
    function AddChannelClient(AddFirst: Boolean = True): TWOL_ChannelClient;  // Add a ChannelClient
    procedure DeleteChannelClient(ChannelClient: TWOL_ChannelClient); // Delete a ChannelClient
    function FindChannelClient(Nickname: String): TWOL_ChannelClient; // Find a ChannelClient

    procedure SendToChannel(Line: String; IgnoredClient: TWOL_Socket = nil);
  end;

  TWOL_ClanData =  class(tobject) // Clients LinkedList Type
    Number: Word;
    Name: String;
    Tag: String;

    Prev: TWOL_ClanData;          // Pointer to previous client (If not first)
    Next: TWOL_ClanData;          // Pointer to next client (If not last)
  end;


Var
//  AdminChannel: TWOL_ChannelData;

  ChannelDataTotal: integer;         // Total number of clients
  FirstChannelData: TWOL_ChannelData;     // Pointer to first client in the list
  LastChannelData: TWOL_ChannelData;      // Pointer to last client in the list

  ClanDataTotal: integer;         // Total number of clients
  FirstClanData: TWOL_ClanData;     // Pointer to first client in the list
  LastClanData: TWOL_ClanData;      // Pointer to last client in the list

procedure LL_InitClanChan();        // Initiate the lists

function LL_AddChannelData(AddFirst: Boolean = True): TWOL_ChannelData;  // Add a client
procedure LL_DeleteChannelData(ChannelData: TWOL_ChannelData); // Delete a client
function LL_FindChannelData(Name: String): TWOL_ChannelData; // Find a client

function LL_AddClanData(AddFirst: Boolean = True): TWOL_ClanData;  // Add a client
procedure LL_DeleteClanData(ClanData: TWOL_ClanData); // Delete a client
function LL_FindClanData(Number: Word): TWOL_ClanData; // Find a client

implementation

procedure LL_InitChannelDatas();
var
  ptr: TWOL_ChannelData;
begin
  if FirstChannelData <> nil then begin
    ptr := FirstChannelData;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        ptr.prev.free;
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        ptr.free;
        break;
      end;
    end;
  end;

  LastChannelData := nil;
  FirstChannelData := nil;
  ChannelDataTotal := 0;
end;

procedure LL_InitClanDatas();
var
  ptr: TWOL_ClanData;
begin
  if FirstClanData <> nil then begin
    ptr := FirstClanData;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        ptr.prev.free;
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        ptr.free;
        break;
      end;
    end;
  end;

  LastClanData := nil;
  FirstClanData := nil;
  ClanDataTotal := 0;
end;

procedure LL_InitClanChan();
begin
  LL_InitChannelDatas();
  LL_InitClanDatas();
end;

//----------------------------------------------------------------------------
// Add a new ChannelData object
// AddFirst is used to decide if it is added at the start or end
//----------------------------------------------------------------------------
function LL_AddChannelData(AddFirst: Boolean = True): TWOL_ChannelData;
var
  NewChannelData: TWOL_ChannelData;
begin
  NewChannelData := TWOL_ChannelData.Create;

  if AddFirst then begin
    NewChannelData.Next := FirstChannelData;
    NewChannelData.Prev := nil;
    if ChannelDataTotal = 0 then begin
       LastChannelData := NewChannelData;
    end else begin
       FirstChannelData.Prev := NewChannelData;
    end;
    FirstChannelData := NewChannelData;
  end else begin
    NewChannelData.Prev := LastChannelData;
    NewChannelData.Next := nil;
      if ChannelDataTotal = 0 then begin
        FirstChannelData := NewChannelData;
      end else begin
        LastChannelData.Next := NewChannelData;
      end;
    LastChannelData := NewChannelData;
  end;

  Inc(ChannelDataTotal);
  Result := NewChannelData;
end;

//----------------------------------------------------------------------------
// Delete a ChannelData object
//----------------------------------------------------------------------------
procedure LL_DeleteChannelData(ChannelData: TWOL_ChannelData);
var
  ptr: TWOL_ChannelClient;
begin
  if ChannelData = nil then exit;

  if not assigned(ChannelData.Prev) then begin
    // i.e., the ChannelData is first
    FirstChannelData := ChannelData.Next;
  end;

  if not assigned(ChannelData.next) then begin
    // i.e., the ChannelData is last
    LastChannelData := ChannelData.Prev;
  end;

  if ChannelData.Prev <> nil then begin
    ChannelData.Prev.Next := ChannelData.Next;
  end;

  if ChannelData.Next <> nil then begin
    ChannelData.Next.Prev := ChannelData.Prev;
  end;

  if ChannelData.FFirstChannelClient <> nil then begin
    ptr := ChannelData.FFirstChannelClient;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        ptr.prev.free;
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        ptr.free;
        break;
      end;
    end;
  end;  

  ChannelData.free;
  Dec(ChannelDataTotal);
end;

//----------------------------------------------------------------------------
// Find a ChannelData object
// Nickname is matched as lowercase
// First match is returned
//----------------------------------------------------------------------------
function LL_FindChannelData(Name: String): TWOL_ChannelData;
var
  ptr: TWOL_ChannelData;
begin
  Result := nil;
  ptr := FirstChannelData;
  while ptr <> nil do begin
    if lowercase(ptr.Name) = lowercase(Name) then begin
      Result := ptr;
      break;
    end;
    ptr := ptr.next;
  end;
end;

{procedure TWOL_ChannelData.Send(Line: String);
var
  ChannelClient: TWOL_ChannelClient;
begin
  ChannelClient := FFirstChannelClient;
  while ChannelClient <> nil do begin
    if User <> IgnoreSock then ChannelClient.User.Send(Line);
    ChannelClient := ChannelClient.next;
  end;
end;}


procedure TWOL_ChannelData.InitChannelClients();
var
  ptr: TWOL_ChannelClient;
begin
  if FFirstChannelClient <> nil then begin
    ptr := FFirstChannelClient;
    while ptr <> nil do begin
      if ptr.prev <> nil then begin
        ptr.prev.free;
      end;
      if ptr.next <> nil then begin
        ptr := ptr.next
      end else begin
        ptr.free;
        break;
      end;
    end;
  end;

  FLastChannelClient := nil;
  FFirstChannelClient := nil;
  ChannelClientTotal := 0;
end;


//----------------------------------------------------------------------------
// Add a new ChannelClient object
// AddFirst is used to decide if it is added at the start or end
//----------------------------------------------------------------------------
function TWOL_ChannelData.AddChannelClient(AddFirst: Boolean = True): TWOL_ChannelClient;
var
  NewChannelClient: TWOL_ChannelClient;
begin
  NewChannelClient := TWOL_ChannelClient.Create;

  if AddFirst then begin
    NewChannelClient.Next := FFirstChannelClient;
    NewChannelClient.Prev := nil;
    if ChannelClientTotal = 0 then begin
       FLastChannelClient := NewChannelClient;
    end else begin
       FFirstChannelClient.Prev := NewChannelClient;
    end;
    FFirstChannelClient := NewChannelClient;
  end else begin
    NewChannelClient.Prev := FLastChannelClient;
    NewChannelClient.Next := nil;
      if ChannelClientTotal = 0 then begin
        FFirstChannelClient := NewChannelClient;
      end else begin
        FLastChannelClient.Next := NewChannelClient;
      end;
    FLastChannelClient := NewChannelClient;
  end;

  Inc(ChannelClientTotal);
  Result := NewChannelClient;
end;

//----------------------------------------------------------------------------
// Delete a ChannelClient object
//----------------------------------------------------------------------------
procedure TWOL_ChannelData.DeleteChannelClient(ChannelClient: TWOL_ChannelClient);
begin
  if ChannelClient = nil then exit;

  if not assigned(ChannelClient.Prev) then begin
    // i.e., the ChannelClient is first
    FFirstChannelClient := ChannelClient.Next;
  end;

  if not assigned(ChannelClient.next) then begin
    // i.e., the ChannelClient is last
    FLastChannelClient := ChannelClient.Prev;
  end;

  if ChannelClient.Prev <> nil then begin
    ChannelClient.Prev.Next := ChannelClient.Next;
  end;

  if ChannelClient.Next <> nil then begin
    ChannelClient.Next.Prev := ChannelClient.Prev;
  end;

  ChannelClient.free;
  Dec(ChannelClientTotal);
end;

//----------------------------------------------------------------------------
// Find a ChannelClient object
// Nickname is matched as lowercase
// First match is returned
//----------------------------------------------------------------------------
function TWOL_ChannelData.FindChannelClient(Nickname: String): TWOL_ChannelClient;
var
  ptr: TWOL_ChannelClient;
begin
  Result := nil;
  ptr := FFirstChannelClient;
  while ptr <> nil do begin
    if lowercase(ptr.user.info.Username) = lowercase(NickName) then begin
      Result := ptr;
      break;
    end;
    ptr := ptr.next;
  end;
end;

//----------------------------------------------------------------------------
// Add a new ClanData object
// AddFirst is used to decide if it is added at the start or end
//----------------------------------------------------------------------------
function LL_AddClanData(AddFirst: Boolean = True): TWOL_ClanData;
var
  NewClanData: TWOL_ClanData;
begin
  NewClanData := TWOL_ClanData.Create;

  if AddFirst then begin
    NewClanData.Next := FirstClanData;
    NewClanData.Prev := nil;
    if ClanDataTotal = 0 then begin
       LastClanData := NewClanData;
    end else begin
       FirstClanData.Prev := NewClanData;
    end;
    FirstClanData := NewClanData;
  end else begin
    NewClanData.Prev := LastClanData;
    NewClanData.Next := nil;
      if ClanDataTotal = 0 then begin
        FirstClanData := NewClanData;
      end else begin
        LastClanData.Next := NewClanData;
      end;
    LastClanData := NewClanData;
  end;

  Inc(ClanDataTotal);
  Result := NewClanData;
end;

//----------------------------------------------------------------------------
// Delete a ClanData object
//----------------------------------------------------------------------------
procedure LL_DeleteClanData(ClanData: TWOL_ClanData);
begin
  if ClanData = nil then exit;

  if not assigned(ClanData.Prev) then begin
    // i.e., the ClanData is first
    FirstClanData := ClanData.Next;
  end;

  if not assigned(ClanData.next) then begin
    // i.e., the ClanData is last
    LastClanData := ClanData.Prev;
  end;

  if ClanData.Prev <> nil then begin
    ClanData.Prev.Next := ClanData.Next;
  end;

  if ClanData.Next <> nil then begin
    ClanData.Next.Prev := ClanData.Prev;
  end;

  ClanData.free;
  Dec(ClanDataTotal);
end;

//----------------------------------------------------------------------------
// Find a ClanData object
// Nickname is matched as lowercase
// First match is returned
//----------------------------------------------------------------------------
function LL_FindClanData(Number: Word): TWOL_ClanData;
var
  ptr: TWOL_ClanData;
begin
  Result := nil;
  ptr := FirstClanData;
  while ptr <> nil do begin
    if ptr.number = number then begin
      Result := ptr;
      break;
    end;
    ptr := ptr.next;
  end;
end;

procedure TWOL_ChannelData.SendToChannel(Line: String; IgnoredClient: TWOL_Socket = nil);
var
  ChannelClient: TWOL_ChannelClient;
begin
  ChannelClient := FFirstChannelClient;
  while ChannelClient <> nil do begin
    if (ChannelClient.User = nil) or (ChannelClient.User = IgnoredClient) then begin
      ChannelClient := ChannelClient.next;
      continue;
    end;
    if ChannelClient.User.Sock = nil then begin
      ChannelClient := ChannelClient.next;
      continue;
    end;
    ChannelClient.User.Send(Line);
    ChannelClient := ChannelClient.next;
  end;
end;

end.

