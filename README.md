#Untorque

Untorque is a command line based Torque3D Script DSO decompiler. 

> **Note:**
> This tool was developed quick and is not intended to be a perfect decompiler and trying to effectively work on simple use cases that cover most features.
>
> It was compiled with Torque3D 3.5.1 and will likely break with any other build if not manually updated.


> **Warning:**
> Untorque does not try to create intermediate folders or protect the user from overwriting files so please use at your own risk and backup files before using this tool.


## Installation

 1. Download the latest release from my github page [Untorque](https://github.com/figment/Untorque/releases) 
 2. Unzip untorque.exe to the root game folder of the Torque engine required  
	 * It is not strictly required as there are no dependencies but examples use this convention

----------

## Example

The following are simple usage examples for decompling files and outputing to a location.

      Untorque decompile scripts\main.cs.dso scripts\main.cs
      Untorque decompile --dump art\gui\StartupGui.gui.dso art\gui\StartupGui.gui

The following are simple usage examples for compling files and outputing to a specific location.
    
      Untorque compile scripts\main.cs scripts\main.cs.dso
      Untorque compile --dump art\gui\StartupGui.gui art\gui\StartupGui.gui.dso

### Automation Examples

#### **DumpScripts.bat**
This batch file assumes Untorque is in the game folder and this batch file is in the same folder.  It will decompile all dso files to source and may overwrite existing files if they are present.

    @echo off
    pushd "%~dp0"
    for /f "usebackq delims=|" %%i in (`dir /b /s "*.dso"`) do (
    	echo %%~dpni
    	Untorque decompile "%%i" "%%~dpni" 	
    )
    popd

----------

## Uninstall

 * Delete untorque.exe from the root game folder of the Torque engine

----------

## Development

### Requirements

 1. Torque3D 3.5.1 was used in development
 2. DirectX SDK June 2010 is a Torque3D Requirement

Please use my [Torque3D](https://github.com/figment/Torque3D) development-3.5.2 branch as there are several fixes to the string and compiler that are required for the Torque code not to crash and fail repeatedly.

> **Note:**
> Edit the *Untorque\buildFiles\VisualStudio 2010\projects\Environment.vcprops* file to fix the DirectX SDK install location.  You can use the Property Manager to change the User Macros which holds the environment variable.  
> 
> Sorry Visual Studio does not make it easy to handle project level environment settings without modifying global environment variables which I do not like so I use this technique but it confuses most developers.

### Compiling
Check out [Untorque](https://github.com/figment/Untorque) project to the *My Projects* folder in your Torque3D checkout.  
> **Note:**
> Git and github do not like projects within projects.  Git has submodules but github does not seem to properly use them. 
>
> I used Junction from sysinternals.com to create symbolic links between the real Untorque and the Torque3D folder but is not what I would generally recommend.  However Git allows me to manage the Untorque in this mode with fewer issues.



