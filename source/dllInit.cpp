#include "dllInit.h"

#include "platform/platformTimer.h"
#include "platform/platformVolume.h"
#include "platform/platformMemory.h"
#include "platform/platformTimer.h"
#include "platform/platformNet.h"
#include "platform/nativeDialogs/fileDialog.h"
#include "platform/threads/thread.h"

#include "core/module.h"
#include "core/threadStatic.h"
#include "core/iTickable.h"
#include "core/stream/fileStream.h"
#include "console/compiler.h"
#include "windowManager/platformWindowMgr.h"

#include "core/util/journal/process.h"
#include "util/fpsTracker.h"

#include "console/debugOutputConsumer.h"
#include "console/consoleTypes.h"
#include "console/engineAPI.h"

#include "util/sampler.h"
#include "platform/threads/threadPool.h"

#ifdef TORQUE_DEMO_PURCHASE
#include "demo/pestTimer/pestTimer.h"
#endif

#ifdef TORQUE_ENABLE_VFS
#include "platform/platformVFS.h"
#endif

DITTS( F32, gTimeScale, 1.0 );
DITTS( U32, gTimeAdvance, 0 );
DITTS( U32, gFrameSkip, 0 );

extern S32 sgBackgroundProcessSleepTime;
extern S32 sgTimeManagerProcessInterval;

extern FPSTracker gFPS;

TimeManager* tm = NULL;

static bool gRequiresRestart = false;

#ifdef TORQUE_DEBUG

/// Temporary timer used to time startup times.
static PlatformTimer* gStartupTimer;

#endif

#if defined( TORQUE_MINIDUMP ) && defined( TORQUE_RELEASE )
StringTableEntry gMiniDumpDir;
StringTableEntry gMiniDumpExec;
StringTableEntry gMiniDumpParams;
StringTableEntry gMiniDumpExecDir;
#endif


namespace engineAPI
{
   // This is the magic switch for deciding which interop the engine
   // should use.  It will go away when we drop the console system
   // entirely but for now it is necessary for several behaviors that
   // differ between the interops to decide what to do.
   bool gUseConsoleInterop = true;
   
   bool gIsInitialized = false;
}

const char* Platform::getClipboard() {
	return NULL;
}
bool Platform::setClipboard(const char *text) {
	return false;
}

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include "platform/platformTimer.h"
#include "platformWin32/platformWin32.h"

//F32 Platform::getRandom() { return 4.0f; }

//Win32PlatState winState;
Platform::SystemInfo_struct Platform::SystemInfo;

#include "app/mainLoop.h"
void StandardMainLoop::setRestart(bool) {}
int SimDataBlock::sNextModifiedKey = 0;

void Input::init() {}
void Input::destroy(){}

#include <gfx\gfxDevice.h>
bool GFXDevice::destroy(){ return false; }
void installRedBookDevices(){}

bool Platform::closeSplashWindow() { return false; }
bool Platform::displaySplashWindow(String path) { return false; }
bool _fndisplaySplashWidnowimpl(char const*){ return false; }

extern "C"
{
	bool torque_engineinit(S32 argc, const char **argv) { return false; }
	S32  torque_enginetick() { return 0; }
	bool torque_engineshutdown() { return false; }
};




#ifndef __DLL__

#include <stdio.h>
void logCallback(U32 level, const char *consoleLine)
{
	TORQUE_UNUSED(level);
	fwrite(consoleLine, strlen(consoleLine), 1, stdout);
	fwrite("\n", 1, 1, stdout);
}
void errCallback(U32 level, const char *consoleLine)
{
	TORQUE_UNUSED(level);
	fwrite(consoleLine, strlen(consoleLine), 1, stderr);
	fwrite("\n", 1, 1, stderr);
}

extern String Decompile(CodeBlock& cb);


#include "core/volume.h"
#include "platformWin32/WinConsole.h"
#include "core/fileio.h"
//#include <console/codeblock.h>
//int PASCAL WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, int nCommandShow)
int main(int argc, const char **argv)
{
	if (argc <= 2)
		return 1;

	FrameAllocator::init(1024);
	Platform::FS::InstallFileSystems();
	StringTable->create();
	Con::init();
	Con::addConsumer(logCallback);

	String dsoFileName;

	if (0 == stricmp(argv[1], "-c")) {
		String inFileName(argv[2]);
		Torque::Path   filePath(inFileName);
		StringTableEntry filenameEntry = StringTable->insert(Torque::PathToPlatform(filePath), false);
		String filename(filenameEntry);
		File input;
		if (File::Ok == input.open(Torque::PathToOS(filename), File::AccessMode::Read))
		{
			U32 len = input.getSize();
			char* script = (char*)malloc(len + 1);
			U32 read = 0;
			if (File::Ok == input.read(len, script, &read))
			{
				script[read] = 0;
				Torque::Path dsoName(inFileName);
				dsoName.setExtension("cs.dso");
				CodeBlock block;
				if (block.compile(dsoName.getFullPath(), filenameEntry, script, true)){
					dsoFileName = dsoName.getFullPath();
				}
			}
			free(script);
		}
	} else if (0 == stricmp(argv[1], "-d")) {
		String inFileName(argv[2]);
		Torque::Path   filePath(inFileName);
		StringTableEntry filenameEntry = StringTable->insert(Torque::PathToPlatform(filePath), false);
		dsoFileName = filenameEntry;
	}
	if (dsoFileName.isNotEmpty())
	{
		FileStream stream;
		if (stream.open(dsoFileName, Torque::FS::File::AccessMode::Read))
		{
			U32 version;
			stream.read(&version);
			if (version == Con::DSOVersion) {
				CodeBlock input;
				if (input.read(dsoFileName, stream)){
					Con::printf("/*");
					input.dumpInstructions();
					Con::printf("*/");
					Con::printf("//---------------------------------------------------------------");
					String str = Decompile(input);
					fwrite(str.c_str(), 1, str.length(), stdout);
					Con::printf("//---------------------------------------------------------------");
					String csFileName2 = "D:\\tmp\\tmp.cs";
					Torque::Path dsoName2(csFileName2);
					dsoName2.setExtension("cs.dso");

					Con::removeConsumer(logCallback);
					Con::addConsumer(errCallback);
					

					String dsoFileName2 = dsoName2.getFullPath();
					CodeBlock block2;
					StringTableEntry filenameEntry2 = StringTable->insert(Torque::PathToPlatform(csFileName2), false);
					block2.compile(dsoName2.getFullPath(), filenameEntry2, str.c_str(), true);
					U32 filesize1 = Platform::getFileSize(dsoFileName);
					U32 filesize2 = Platform::getFileSize(dsoName2.getFullPath());
					if (filesize1 != filesize2)
					{
						fprintf(stderr, "Compile sizes do not match: %s  (%d != %d)\n", Torque::PathToOS(dsoFileName).c_str(), filesize1, filesize2);
					}
				}
			}
		}
	}
	return 0;
}
#endif