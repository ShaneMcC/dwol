// SVN: $Id:$

//-----------------------------------------------------------------------------
// Main Application
//-----------------------------------------------------------------------------

// Dont show the console when running in windows? (Can be run hidden on startup)
// {$DEFINE NO_CONSOLE}

// Dont use an icon for windows compiles?
// {$DEFINE NO_ICON}

// Use wcore/lcore for timers rather than extctrls
{$DEFINE UseCore}

// If we are using FPC, we _have_ to use *core because extctrls doesn't exist
{$IFDEF FPC}{$DEFINE UseCore}{$ENDIF}
