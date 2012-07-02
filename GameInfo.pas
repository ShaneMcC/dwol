unit GameInfo;

interface

  function GetLocaleName(nLocale: Byte): String;
  function GetSKUGame(nSKU: Integer): String;
  function GetChanModeGame(nChanMode: Integer): String;
implementation

function GetLocaleName(nLocale: Byte): String;
begin
  Case nLocale of
    4: Result := 'United Kingdom';
  else Result := 'Unknown';
  end;
end;

function GetSKUGame(nSKU: Integer): String;
begin
  Case nSKU of
      1000: Result := 'Westwood Chat';
      // 1000

      1003: Result := 'Command & Conquer';
      // 1003

      1005,
      1006,
      1007,
      1008: Result := 'Red Alert 1 v2.02';
      // 1005 1006 1007 1008

      1040: Result := 'C&C Sole Survivor';
      // 1040

      3072,
      3074,
      3075,
      3078,
      3081,
      3082: Result := 'C&C Renegade';
      // 3072 3074 3075 3078 3081 3082

      3584,
      3586,
      3587,
      3589,
      3591: Result := 'Dune 2000';
      // 3584 3586 3587 3589 5391

      4096,
      4098,
      4099,
      4101,
      4102,
      4105: Result := 'Nox';
      // 4096 4098 4099 4101 4102 4105

      4608,
      4610,
      4611,
      4615: Result := 'Tiberian Sun';
      // 4608 4610 4611 4615

      5376,
      5378,
      5379: Result := 'Red Alert 1 v3.03';
      // 5376 5378 5479

      7168,
      7170,
      7171,
      7175,
      7424,
      7426,
      7427,
      7431: Result := 'Tiberian Sun: Firestorm';
      // 7168 7170 7171 7175 7424 7426 7427 7431

      7936,
      7938,
      7939,
      7945,
      7946: Result := 'Emperor: Battle for Dune';
      // 7936 7938 7939 7945 7946

      8448,
      8450,
      8451,
      8457,
      8458,
      8960,
      8962,
      8963,
      8969,
      8970: Result := 'Red Alert 2';
      // 8448 8450 8451 8457 8458 8960 8962 8963 8969 8970

      9472,
      9474,
      9475,
      9477,
      9478,
      9481: Result := 'Nox Quest';
      // 9472 9474 9475 9477 9478 9481

      10496,
      10498,
      10499,
      10505,
      10506: Result := 'Yuri''s Revenge';
      // 10496 10498 10499 10505 10506

      12288: Result := 'Renegade - Free Dedicated Server';
      // 12288

      32512: Result := 'WOL API';
      // 32512
  else Result := 'Unknown';
  end;

  // All TS/RA2
  // 4608 4610 4611 4615 7168 7170 7171 7175 7424 7426 7427
  // 7431 8448 8450 8451 8457 8458 8960 8962 8963 8969 8970
  // 10496 10498 10499 10505 10506
end;

function GetChanModeGame(nChanMode: Integer): String;
begin
  Case nChanMode of
    12: Result := '';
    14: Result := '';
    16: Result := '';
    18: Result := 'Tiberian Sun';
    21: Result := '';
    31: Result := '';
    33: Result := '';
    37: Result := '';
    41: Result := 'Yuri''s Revenge';
  else Result := 'Unknown';
  end;
end;

end.
