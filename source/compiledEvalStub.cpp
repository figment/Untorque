//-----------------------------------------------------------------------------
// Copyright (c) 2012 GarageGames, LLC
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to
// deal in the Software without restriction, including without limitation the
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
// IN THE SOFTWARE.
//-----------------------------------------------------------------------------

#include "platform/platform.h"
#include "console/console.h"

#include "console/ast.h"
#include "core/tAlgorithm.h"

#include "core/strings/findMatch.h"
#include "core/strings/stringUnit.h"
#include "console/consoleInternal.h"
#include "core/stream/fileStream.h"
#include "console/compiler.h"

#include "console/simBase.h"
#include "console/telnetDebugger.h"
#include "sim/netStringTable.h"
#include "console/ICallMethod.h"
#include "console/stringStack.h"
#include "util/messaging/message.h"
#include "core/frameAllocator.h"

#ifndef TORQUE_TGB_ONLY
#include "materials/materialDefinition.h"
#include "materials/materialManager.h"
#endif

using namespace Compiler;

enum EvalConstants {
	MaxStackSize = 1024,
	MethodOnComponent = -2
};

namespace Con
{
	// Current script file name and root, these are registered as
	// console variables.
	extern StringTableEntry gCurrentFile;
	extern StringTableEntry gCurrentRoot;
	extern S32 gObjectCopyFailures;
}

/// Frame data for a foreach/foreach$ loop.
struct IterStackRecord
{
	/// If true, this is a foreach$ loop; if not, it's a foreach loop.
	bool mIsStringIter;

	/// The iterator variable.
	Dictionary::Entry* mVariable;

	/// Information for an object iterator loop.
	struct ObjectPos
	{
		/// The set being iterated over.
		SimSet* mSet;

		/// Current index in the set.
		U32 mIndex;
	};

	/// Information for a string iterator loop.
	struct StringPos
	{
		/// The raw string data on the string stack.
		const char* mString;

		/// Current parsing position.
		U32 mIndex;
	};

	union
	{
		ObjectPos mObj;
		StringPos mStr;
	} mData;
};

IterStackRecord iterStack[MaxStackSize];

F64 floatStack[MaxStackSize];
S64 intStack[MaxStackSize];

StringStack STR;

U32 _FLT = 0;     ///< Stack pointer for floatStack.
U32 _UINT = 0;    ///< Stack pointer for intStack.
U32 _ITER = 0;    ///< Stack pointer for iterStack.

namespace Con
{
	const char *getNamespaceList(Namespace *ns)
	{
		U32 size = 1;
		Namespace * walk;
		for (walk = ns; walk; walk = walk->mParent)
			size += dStrlen(walk->mName) + 4;
		char *ret = Con::getReturnBuffer(size);
		ret[0] = 0;
		for (walk = ns; walk; walk = walk->mParent)
		{
			dStrcat(ret, walk->mName);
			if (walk->mParent)
				dStrcat(ret, " -> ");
		}
		return ret;
	}
}

//------------------------------------------------------------

F64 consoleStringToNumber(const char *str, StringTableEntry file, U32 line)
{
	F64 val = dAtof(str);
	if (val != 0)
		return val;
	else if (!dStricmp(str, "true"))
		return 1;
	else if (!dStricmp(str, "false"))
		return 0;
	else if (file)
	{
		Con::warnf(ConsoleLogEntry::General, "%s (%d): string always evaluates to 0.", file, line);
		return 0;
	}
	return 0;
}

//------------------------------------------------------------

namespace Con
{

	char *getReturnBuffer(U32 bufferSize)

	{
		return STR.getReturnBuffer(bufferSize);
	}

	char *getReturnBuffer(const char *stringToCopy)
	{
		U32 len = dStrlen(stringToCopy) + 1;
		char *ret = STR.getReturnBuffer(len);
		dMemcpy(ret, stringToCopy, len);
		return ret;
	}

	char* getReturnBuffer(const String& str)
	{
		const U32 size = str.size();
		char* ret = STR.getReturnBuffer(size);
		dMemcpy(ret, str.c_str(), size);
		return ret;
	}

	char* getReturnBuffer(const StringBuilder& str)
	{
		char* buffer = Con::getReturnBuffer(str.length() + 1);
		str.copy(buffer);
		buffer[str.length()] = '\0';

		return buffer;
	}

	char *getArgBuffer(U32 bufferSize)
	{
		return STR.getArgBuffer(bufferSize);
	}

	char *getFloatArg(F64 arg)
	{
		char *ret = STR.getArgBuffer(32);
		dSprintf(ret, 32, "%g", arg);
		return ret;
	}

	char *getIntArg(S32 arg)
	{
		char *ret = STR.getArgBuffer(32);
		dSprintf(ret, 32, "%d", arg);
		return ret;
	}

	char *getStringArg(const char *arg)
	{
		U32 len = dStrlen(arg) + 1;
		char *ret = STR.getArgBuffer(len);
		dMemcpy(ret, arg, len);
		return ret;
	}

	char* getStringArg(const String& arg)
	{
		const U32 size = arg.size();
		char* ret = STR.getArgBuffer(size);
		dMemcpy(ret, arg.c_str(), size);
		return ret;
	}
}

//------------------------------------------------------------

inline void ExprEvalState::setCurVarName(StringTableEntry name)
{
	if (name[0] == '$')
		currentVariable = globalVars.lookup(name);
	else if (getStackDepth() > 0)
		currentVariable = getCurrentFrame().lookup(name);
	if (!currentVariable && gWarnUndefinedScriptVariables)
		Con::warnf(ConsoleLogEntry::Script, "Variable referenced before assignment: %s", name);
}

inline void ExprEvalState::setCurVarNameCreate(StringTableEntry name)
{
	if (name[0] == '$')
		currentVariable = globalVars.add(name);
	else if (getStackDepth() > 0)
		currentVariable = getCurrentFrame().add(name);
	else
	{
		currentVariable = NULL;
		Con::warnf(ConsoleLogEntry::Script, "Accessing local variable in global scope... failed: %s", name);
	}
}

//------------------------------------------------------------

inline S32 ExprEvalState::getIntVariable()
{
	return currentVariable ? currentVariable->getIntValue() : 0;
}

inline F64 ExprEvalState::getFloatVariable()
{
	return currentVariable ? currentVariable->getFloatValue() : 0;
}

inline const char *ExprEvalState::getStringVariable()
{
	return currentVariable ? currentVariable->getStringValue() : "";
}

//------------------------------------------------------------

inline void ExprEvalState::setIntVariable(S32 val)
{
	AssertFatal(currentVariable != NULL, "Invalid evaluator state - trying to set null variable!");
	currentVariable->setIntValue(val);
}

inline void ExprEvalState::setFloatVariable(F64 val)
{
	AssertFatal(currentVariable != NULL, "Invalid evaluator state - trying to set null variable!");
	currentVariable->setFloatValue(val);
}

inline void ExprEvalState::setStringVariable(const char *val)
{
	AssertFatal(currentVariable != NULL, "Invalid evaluator state - trying to set null variable!");
	currentVariable->setStringValue(val);
}

//------------------------------------------------------------

// Gets a component of an object's field value or a variable and returns it
// in val.
static void getFieldComponent(SimObject* object, StringTableEntry field, const char* array, StringTableEntry subField, char val[])
{
	const char* prevVal = NULL;

	// Grab value from object.
	if (object && field)
		prevVal = object->getDataField(field, array);

	// Otherwise, grab from the string stack. The value coming in will always
	// be a string because that is how multicomponent variables are handled.
	else
		prevVal = STR.getStringValue();

	// Make sure we got a value.
	if (prevVal && *prevVal)
	{
		static const StringTableEntry xyzw[] =
		{
			StringTable->insert("x"),
			StringTable->insert("y"),
			StringTable->insert("z"),
			StringTable->insert("w")
		};

		static const StringTableEntry rgba[] =
		{
			StringTable->insert("r"),
			StringTable->insert("g"),
			StringTable->insert("b"),
			StringTable->insert("a")
		};

		// Translate xyzw and rgba into the indexed component 
		// of the variable or field.
		if (subField == xyzw[0] || subField == rgba[0])
			dStrcpy(val, StringUnit::getUnit(prevVal, 0, " \t\n"));

		else if (subField == xyzw[1] || subField == rgba[1])
			dStrcpy(val, StringUnit::getUnit(prevVal, 1, " \t\n"));

		else if (subField == xyzw[2] || subField == rgba[2])
			dStrcpy(val, StringUnit::getUnit(prevVal, 2, " \t\n"));

		else if (subField == xyzw[3] || subField == rgba[3])
			dStrcpy(val, StringUnit::getUnit(prevVal, 3, " \t\n"));

		else
			val[0] = 0;
	}
	else
		val[0] = 0;
}

// Sets a component of an object's field value based on the sub field. 'x' will
// set the first field, 'y' the second, and 'z' the third.
static void setFieldComponent(SimObject* object, StringTableEntry field, const char* array, StringTableEntry subField)
{
	// Copy the current string value
	char strValue[1024];
	dStrncpy(strValue, STR.getStringValue(), 1024);

	char val[1024] = "";
	const char* prevVal = NULL;

	// Set the value on an object field.
	if (object && field)
		prevVal = object->getDataField(field, array);

	// Set the value on a variable.
	else if (gEvalState.currentVariable)
		prevVal = gEvalState.getStringVariable();

	// Ensure that the variable has a value
	if (!prevVal)
		return;

	static const StringTableEntry xyzw[] =
	{
		StringTable->insert("x"),
		StringTable->insert("y"),
		StringTable->insert("z"),
		StringTable->insert("w")
	};

	static const StringTableEntry rgba[] =
	{
		StringTable->insert("r"),
		StringTable->insert("g"),
		StringTable->insert("b"),
		StringTable->insert("a")
	};

	// Insert the value into the specified 
	// component of the string.
	if (subField == xyzw[0] || subField == rgba[0])
		dStrcpy(val, StringUnit::setUnit(prevVal, 0, strValue, " \t\n"));

	else if (subField == xyzw[1] || subField == rgba[1])
		dStrcpy(val, StringUnit::setUnit(prevVal, 1, strValue, " \t\n"));

	else if (subField == xyzw[2] || subField == rgba[2])
		dStrcpy(val, StringUnit::setUnit(prevVal, 2, strValue, " \t\n"));

	else if (subField == xyzw[3] || subField == rgba[3])
		dStrcpy(val, StringUnit::setUnit(prevVal, 3, strValue, " \t\n"));

	if (val[0] != 0)
	{
		// Update the field or variable.
		if (object && field)
			object->setDataField(field, 0, val);
		else if (gEvalState.currentVariable)
			gEvalState.setStringVariable(val);
	}
}

const char *CodeBlock::exec(U32 ip, const char *functionName, Namespace *thisNamespace, U32 argc, const char **argv, bool noCalls, StringTableEntry packageName, S32 setFrame)
{
	return NULL;
}