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

#define ASSERT(x, msg) { if (x) { fprintf(stderr,"%s\n",msg); if(IsDebuggerPresent()) __asm{ int 3 }; } }
#define ASSERT1(x, msg, arg1) { if (x) { fprintf(stderr,msg,arg1); if(IsDebuggerPresent()) __asm{ int 3 }; } }
#define ASSERT2(x, msg, arg1, arg2) { if (x) { fprintf(stderr,msg,arg1,arg2); if(IsDebuggerPresent()) __asm{ int 3 }; } }

using namespace Compiler;

extern "C" int __stdcall IsDebuggerPresent(void);

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
		Con::warnf(ConsoleLogEntry::Script, "Accessing local variable in bbal scope... failed: %s", name);
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


struct Expression
{
	enum ExpressionType : char
	{
		NONE,
		UNARYOP,
		BINARYOP,
		STRINGOP,
		ARRAYOP,
		GENERALOP,
		JUMPOP,
		FLOATLITERAL,
		UINTLITERAL,
		STRINGLITERAL,
		STRINGALLOC,
		SPECIALOP,
	};
	ExpressionType type;
	union
	{
		struct
		{
			Compiler::CompiledInstructions val;
			U32 arg;
		} op;
		StringTableEntry strVal;
		F64 floatVal;
		S64 intVal;
		void *memory;
	};

	Expression() : type(NONE), intVal(0) { reset(); }

	void reset() {
		if (type == STRINGALLOC) free(memory);
		type = NONE;
		intVal = 0;
	}
	void set(ExpressionType t, U32 val){
		reset();
		type = t;
		intVal = val;
	}
	void set(U32 val){
		reset();
		type = UINTLITERAL;
		intVal = val;
	}
	void set(U64 val){
		reset();
		type = UINTLITERAL;
		intVal = val;
	}
	void set(F32 val){
		reset();
		type = FLOATLITERAL;
		floatVal = val;
	}
	void set(F64 val){
		reset();
		type = FLOATLITERAL;
		floatVal = val;
	}
	void set(StringTableEntry val, bool copy = false){
		reset();
		if (copy && val && val[0]) {
			type = STRINGALLOC;
			memory = malloc(strlen(val) + 1);
			strVal = strcpy((char*)memory, val);
		}
		else {
			type = STRINGLITERAL;
			strVal = val;
		}
	}
	void setOp(Compiler::CompiledInstructions value, U32 arg = 0) {
		switch (value)
		{
		case OP_NEG:
		case OP_NOT:
		case OP_NOTF:
		case OP_ONESCOMPLEMENT:
			type = UNARYOP;
			break;
		case OP_ADVANCE_STR:
		case OP_ADVANCE_STR_COMMA:
		case OP_ADVANCE_STR_NUL:
		case OP_ADVANCE_STR_APPENDCHAR:
		case OP_REWIND_STR:
		case OP_TERMINATE_REWIND_STR:
		case OP_COMPARE_STR:
			type = STRINGOP;
			break;
		case OP_SETCURFIELD:
		case OP_SETCURVAR_ARRAY:
		case OP_SETCURVAR_ARRAY_CREATE:
		case OP_SETCURFIELD_ARRAY:
		case OP_SAVEFIELD_UINT:
		case OP_SAVEFIELD_FLT:
		case OP_SAVEFIELD_STR:
		case OP_LOADFIELD_UINT:
		case OP_LOADFIELD_FLT:
		case OP_LOADFIELD_STR:
			type = ARRAYOP;
			break;
		case OP_STR_TO_NONE:
		case OP_FLT_TO_NONE:
		case OP_UINT_TO_NONE:
		case OP_SETCUROBJECT_INTERNAL:
			type = GENERALOP;
			break;
		case OP_JMPIF:
		case OP_JMPIFF:
		case OP_JMPIF_NP:
		case OP_JMPIFNOT:
		case OP_JMPIFNOT_NP:
		case OP_JMPIFFNOT:
		case OP_JMP:
			type = JUMPOP;
			break;
		default:
			type = BINARYOP;
			break;
		}
		op.val = value;
		op.arg = arg;
	}
};

struct Frame
{
	enum FrameType : char {
		NONE,
		IF,
		INLINEIF, 
		IFCOND,
		ELSE,
		SWITCH,
		WHILE,
		ITER,
		FUNC,
		OBJECT,
	};
	FrameType type;
	bool written;
	bool placeAtRoot;
	U16 expr;
	U32 start;
	U32 end;

	Frame() : type(NONE), start(0), end(0), written(false), expr(0) {}
	//operator U32() { return end; }
	void reset() { type = NONE, start = 0, end = 0, written = false, expr = 0; }
	void set(FrameType t, U32 s, U32 e, bool w=false, U16 x=0) { type = t, start = s, end = e, written = w, expr = x; }
};

class CodeWriter
{
	String::StrFormat mFormat;
public:
	U32 mIndent;
	bool needIndent;
	bool needEnd;
	bool needReturn;

	CodeWriter() : mIndent(0), needIndent(false), needEnd(false), needReturn(false) {}

	U32 length() const
	{
		return mFormat.length();
	}

	void indent() { 
		++mIndent; 
	}

	void unindent() { 
		if (mIndent == 0) 
			return;
		--mIndent; 
	}

	CodeWriter& startBlock(bool sameline = false) {
		append("{");
		if (!sameline) {
			appendline();
		}
		indent();
		return *this;
	}

	CodeWriter& endBlock(bool endStatement = false)
	{
		unindent();
		if (mIndent >= 0) {
			append("}");
			needEnd |= endStatement;
		}
		return *this;
	}

	void ensureIndent() {
		flush();
		if (needIndent) {
			needIndent = false;
			for (U32 i = 0; i < mIndent; ++i)
				mFormat.append("    ");
		}
	}

	void copy(char* buffer) const
	{
		mFormat.copy(buffer);
	}

	const char* data() const
	{
		return mFormat.c_str();
	}
	void reset()
	{
		mFormat.reset();
		needIndent = false;
		needEnd = false;
		needReturn = false;
		mIndent = 0;
	}

	CodeWriter& flush()
	{
		if (needEnd) {
			needEnd = false;
			needReturn = false;
			appendline(";");
		}
		if (needReturn) {
			appendline();
		}
		return *this;
	}

	String end()
	{
		return mFormat.getString();
	}
	operator String()
	{
		return mFormat.getString();
	}

	CodeWriter& append(char ch)
	{
		ensureIndent();
		char str[2];
		str[0] = ch;
		str[1] = '\0';
		mFormat.append(str);
		return *this;
	}
	CodeWriter& append(const char* str)
	{
		ensureIndent();
		mFormat.append(str);
		return *this;
	}
	CodeWriter& append(const String& str)
	{
		ensureIndent();
		mFormat.append(str.c_str(), str.length());
		return *this;
	}
	CodeWriter& append(const char* str, U32 length)
	{
		ensureIndent();
		mFormat.append(str, length);
		return *this;
	}
	CodeWriter& format(const char* fmt, ...)
	{
		ensureIndent();
		va_list args;
		va_start(args, fmt);
		mFormat.formatAppend(fmt, &args);
		va_end(args);
		return *this;
	}
	CodeWriter& appendline()
	{
		needReturn = false;
		append("\n");
		needIndent = true;
		return *this;
	}
	CodeWriter& appendline(char ch) {
		append(ch);
		return appendline();
	}
	CodeWriter& appendline(const char* str) {
		append(str);
		return appendline();
	}
	CodeWriter& appendline(const String& str){
		append(str.c_str(), str.length());
		return appendline();
	}
	CodeWriter& appendline(const char* str, U32 length){
		append(str, length);
		return appendline();
	}
	CodeWriter& formatline(const char* fmt, ...)
	{
		ensureIndent();
		va_list args;
		va_start(args, fmt);
		mFormat.formatAppend(fmt, &args);
		va_end(args);
		return appendline();
	}

	bool isNullOrEmpty(const char * string) {
		U32 len = strlen(string);
		if (len == 0)
			return true;
		for (int i = 0; i < len; ++i){
			if (!isspace(string[i]))
				return false;
		}
		return true;
	}

	CodeWriter& appendLiteral(const char * string) {
		if (isNullOrEmpty(string)) {
			return append('\"').append(String(string).expandEscapes()).append('\"');
		} else {
			char *end = NULL;
			F32 f = strtof(string, &end);
			if (errno == ERANGE || end == NULL || (end - string) != strlen(string))
				return append('\"').append(String(string).expandEscapes()).append('\"');
			else
				return append(string);
		}
	}

CodeWriter& needLine() {
	needReturn = true;
	return *this;
}

CodeWriter& endLine() {
	needEnd = true;
	return *this;
}

};

class Decompiler
{
	//CodeWriter writer;
	CodeBlock & block;

	Expression exprStack[MaxStackSize * 2];
	U32 _EXPR = 0;

	IterStackRecord iterStack[MaxStackSize];

	Frame frameStack[MaxStackSize]; // track frame starts
	U32 callFrame[MaxStackSize];  // track current position in callArgs for current frame
	U32 callArgs[MaxStackSize];

	U32 _ITER = 0;    ///< Stack pointer for iterStack.
	U32 _FRAME = 0;
	U32 _CALLFR = 0;
	U32 _CALLARGS = 0;
	StringTableEntry curPackage = NULL;
	F64 *curFloatTable = NULL;
	char *curStringTable = NULL;
	S32 curStringTableLen = 0; //clint to ensure we dont overwrite it

	CodeWriter writerArray[10];
	CodeWriter *curWriter;
	U32 _WRITER = 0;
	U32 curInstructionPointer = 0;

public:

	Decompiler(CodeBlock & cb) : block(cb) {}

	void reset() {
		curInstructionPointer = 0;
		_FRAME = 0;   ///< Stack pointer for frameStack;
		_CALLFR = 0;
		_EXPR = 0;
		_CALLARGS = 0;
		_WRITER = 0;
		SetGlobalTable();
		curWriter = &writerArray[_WRITER];
		CodeWriter &writer = (*this->curWriter);
		writer.reset();
	}

	void SetGlobalTable()
	{
		curFloatTable = block.globalFloats;
		curStringTable = block.globalStrings;
		curStringTableLen = block.globalStringsMaxLen;
	}
	void SetFunctionTable()
	{
		curFloatTable = block.functionFloats;
		curStringTable = block.functionStrings;
		curStringTableLen = block.functionStringsMaxLen;
	}

	void UpdatePackage(StringTableEntry package)
	{
		if (curPackage != package){
			if (curPackage) (*curWriter).endBlock(true).appendline().flush();
			curPackage = package;
			if (curPackage) (*curWriter).format("package %s", curPackage).appendline().startBlock();
		}
	}

	void CheckPackageScope()
	{
		if (_FRAME == 0)
		{
			UpdatePackage(NULL); // close out the global scope
		}
	}

	Frame &GetCurrentFrame()
	{
		return _FRAME > 0 ? frameStack[_FRAME - 1] : frameStack[0];
	}

	bool PopFrame() {
		bool result = true;
		CodeWriter &writer = (*this->curWriter);
		Frame& curFrame = GetCurrentFrame();
		switch (curFrame.type)
		{
		case Frame::IFCOND:
		{
			//writeCurrentExpr(writer);
			//popExpr();
			result = false;
		} break;
		case Frame::INLINEIF:
		{
			pushExpr().set(Expression::SPECIALOP, 0);
			result = false;
		} break;
		case Frame::OBJECT:
		{
			//if (!curFrame.placeAtRoot) {
			if (_EXPR > curFrame.expr){
				writeCurrentExpr(writer);
				popExpr();
				writer.endLine().needLine();
			}
		}break;
		}
		curFrame.reset();
		--_FRAME;
		collapseExpr();
		return result;
	}

	void CheckFrameWrite()
	{
		CheckFrameWrite(_FRAME);
	}

	void CheckFrameWrite(U32 idx)
	{
		if (idx > 0)
		{
			Frame& curFrame = frameStack[idx - 1];
			if (!curFrame.written /*&& curFrame.start <= curInstructionPointer && curFrame.end >= curInstructionPointer*/)
			{
				curFrame.written = true;
				if (curFrame.type == Frame::INLINEIF)
				{
					PopFrame();
				}
				else
				{
					// recursively process until parent frame is written
					CheckFrameWrite(idx - 1);
					// jump around the expression stack looking for inline if/else
					CodeWriter &writer = (*this->curWriter);
					switch (curFrame.type)
					{
					case Frame::IF:
					{
						Expression& expr = exprStack[curFrame.expr];
						writeExpr(writer, curFrame.expr);
						for (U32 idx = walkExpr(curFrame.expr - 1), end = curFrame.expr; idx <= end; idx++)
							exprStack[idx].reset();
					}	break;
					case Frame::ELSE:
						break;
					}
				}
			}
		}
	}

	/// Check for end of frame and close out block
	void CheckFrameEnd(U32 ip)
	{
		U32 maxExpr = 0;
		bool changed = false;
		while (_FRAME > 0)
		{
			Frame &curFrame = GetCurrentFrame();
			if (ip < curFrame.end)
				break;

			maxExpr = curFrame.expr;
			if (curFrame.type == Frame::INLINEIF && !curFrame.written)
			{
				changed = PopFrame();
				break;
			}
			bool checkFrameWrite = true;
			if (curFrame.type == Frame::IFCOND)
			{
				if (!curFrame.written)
				{
					if (curInstructionPointer == curFrame.end)
					{
						Compiler::CompiledInstructions prevInstruction = (Compiler::CompiledInstructions)block.code[curFrame.start - 1];
						pushExpr().setOp(prevInstruction == OP_JMPIFNOT_NP ? OP_AND : OP_OR);// push back on stack with arg = 1 so we can build complex expression
						curFrame.written = true;
						PopFrame();
					}
					break;
				}
				checkFrameWrite = false;
			}

			if (checkFrameWrite)
				CheckFrameWrite(_FRAME);
			if (PopFrame())
			{
				CodeWriter &writer = (*this->curWriter);
				writer.endBlock();
				ASSERT2(_EXPR > maxExpr, "Unprocessed expressions: %d at %d\n", _EXPR, ip);
				if (_EXPR > maxExpr)
					writer.format("/* %d | %d */", _EXPR, ip);
				writer.needLine();
			}
			changed = true;
		}
		if (changed)
		{
			ASSERT2(_EXPR > maxExpr, "Unprocessed expressions: %d at %d\n", _EXPR, ip);
			if (_EXPR > maxExpr)
			{
				for (U32 idx = maxExpr; idx < _EXPR; ++idx)
					exprStack[idx].reset();
				_EXPR = maxExpr;
			}
		}
		if (changed && _FRAME == 0)
		{
			ASSERT2(_EXPR != 0, "Unprocessed expressions: %d at %d\n", _EXPR, ip);
			SetGlobalTable();
		}
	}

	
	static int _PtFuncCompare(const void *a, const void *b) {
		Frame* fa = (Frame*)a, *fb = (Frame*)b;
		int diff = (fb->end - fa->end);
		if (diff == 0)
			diff = (fa->start - fb->start);
		return diff == 0 ? 0 : diff > 0 ? 1 : -1;
	}
	void PushFrameEnd(Frame::FrameType type, U32 start, U32 end, bool written = false) {
		frameStack[_FRAME++].set(type, start, end, written, (U16)_EXPR);
		if (_FRAME > 1) qsort(frameStack, _FRAME, sizeof(frameStack[0]), _PtFuncCompare);
	}
	void ChangeFrameType(Frame::FrameType type) {
		frameStack[_FRAME].type = type;
	}

	Expression& pushExpr(){
		return exprStack[++_EXPR];
	}

	Expression& curExpr(){
		return exprStack[_EXPR];
	}

	void collapseExpr() {
		while (_EXPR > 0 && exprStack[_EXPR].type == Expression::NONE)
			_EXPR--;
	}
	void popExpr(){
		if (_EXPR <= 0) {
			exprStack[0].reset();
		} else {
			U32 idx = walkExpr(_EXPR);
			while (_EXPR >= idx && _EXPR > 0)
				exprStack[_EXPR--].reset();
			collapseExpr();
		}
	}

	void writeCurrentExpr(CodeWriter& writer, bool wrap = false) {
		writeExpr(writer, _EXPR, wrap);
	}

	U32 walkExpr(U32 idx) {
		if (idx < 0 || idx > _EXPR)
			return 0;
		Expression &expr = exprStack[idx];
		switch (expr.type)
		{
		case Expression::NONE: 
			return idx - 1;
		
		case Expression::UNARYOP:
			return walkExpr(idx - 1);
		
		case Expression::BINARYOP:
			return walkExpr(walkExpr(idx - 1) - 1); 
		
		case Expression::SPECIALOP:
			switch (expr.op.val)
			{
			case 0:
				U32 arg3 = walkExpr(idx - 1);
				U32 elsebr = walkExpr(arg3 - 1);
				U32 arg2 = walkExpr(elsebr - 1);
				U32 cond = walkExpr(arg2 - 1);
				return cond;
				//return walkExpr(cond-1);
				break;
			} break;
		
		case Expression::STRINGOP:
			switch (expr.op.val)
			{
			case OP_ADVANCE_STR:
			case OP_ADVANCE_STR_NUL:
			case OP_ADVANCE_STR_COMMA:
			case OP_TERMINATE_REWIND_STR:
			case OP_ADVANCE_STR_APPENDCHAR:
				return walkExpr(idx - 1);
			case OP_REWIND_STR:
				return walkExpr(walkExpr(idx - 1) - 1);
			case OP_COMPARE_STR:
				return walkExpr(walkExpr(idx - 1) - 1);
			default:
				return walkExpr(idx - 1);
			}
			break;

		case Expression::ARRAYOP:
			switch (expr.op.val)
			{
			case  OP_SETCURFIELD:
				break;
			case OP_SETCURVAR_ARRAY:
			case OP_SETCURVAR_ARRAY_CREATE:
			{
				U32 arg1 = idx - 2; // index
				U32 arg2 = walkExpr(arg1) - 1; // array
				U32 arg3 = walkExpr(arg2) - 1; // array
				return arg3 + 1;
			} break;
			case OP_SETCURFIELD_ARRAY:
				return walkExpr(idx - 1);
			case OP_SAVEFIELD_FLT:
			case OP_SAVEFIELD_UINT:
			case OP_SAVEFIELD_STR:
			{
				U32 arg1 = idx - 1; // index
				U32 arg2 = walkExpr(arg1) - 1; // array
				U32 arg3 = walkExpr(arg2) - 1; // array
				Expression &earg2 = exprStack[arg2];
				if (earg2.type == Expression::BINARYOP) // special case
				{
					return walkExpr(arg2);
				}
				else
				{
					bool isArray = (earg2.type == Expression::ARRAYOP && earg2.op.val == OP_SETCURFIELD_ARRAY);
					if (isArray) {
						arg3 = walkExpr(arg3) - 1;
					}
					return walkExpr(arg3);
				}
			}
			case OP_LOADFIELD_UINT:
			case OP_LOADFIELD_FLT:
			case OP_LOADFIELD_STR:
			{
				U32 arg1 = idx - 1;
				U32 arg2 = walkExpr(arg1) - 1;
				U32 arg3 = walkExpr(arg2) - 2; // array
				Expression &earg2 = exprStack[arg2];
				bool isArray = (earg2.type == Expression::ARRAYOP && earg2.op.val == OP_SETCURFIELD_ARRAY);
				if (isArray) {
					arg2 = arg2 - 1;
					return walkExpr(arg3);
				}
				return walkExpr(arg2);
			}
			} break;


		case Expression::GENERALOP:
			switch (expr.op.val)
			{
			case OP_SETCUROBJECT_INTERNAL:
				return walkExpr(walkExpr(idx - 1) - 1);
			}
			return idx;

		case Expression::JUMPOP:
			switch (expr.op.val)
			{
			case OP_JMP:
				return idx;
			case OP_JMPIF_NP:
			case OP_JMPIFNOT_NP:
				return walkExpr(idx - 1);
			default:
				return walkExpr(idx - 1);
			}break;

		case Expression::FLOATLITERAL: 
		case Expression::UINTLITERAL: 
		case Expression::STRINGLITERAL:
		case Expression::STRINGALLOC:
			return idx;
		}
		return 0;
	}

	bool shouldWrapExpr(U32 idx)
	{
		if (idx < 0 || idx > _EXPR)
			return false;
		Expression &expr = exprStack[idx];
		switch (expr.type)
		{
		case Expression::NONE: 
			return false;
		case Expression::UNARYOP:
			return true;

		case Expression::STRINGOP:
			switch (expr.op.val){
			case OP_ADVANCE_STR_COMMA:
				return false;
			case OP_REWIND_STR:
			case OP_COMPARE_STR:
				return true;
			default:
				return false;
			}

		case Expression::BINARYOP:
			switch (expr.op.val)
			{
			case OP_SAVEVAR_FLT:
			case OP_SAVEVAR_UINT:
			case OP_SAVEVAR_STR:
				return false;
			}
			return true;
		case Expression::ARRAYOP:
			switch (expr.op.val)
			{
			case OP_SAVEFIELD_FLT:
			case OP_SAVEFIELD_UINT:
			case OP_SAVEFIELD_STR:
				return false;
			case OP_LOADFIELD_UINT:
			case OP_LOADFIELD_FLT:
			case OP_LOADFIELD_STR:
				return true;
			}
			return false;
		case Expression::FLOATLITERAL: 
		case Expression::UINTLITERAL: 
		case Expression::STRINGLITERAL:
		case Expression::STRINGALLOC:
			return false;
		}
		return false;
	}

	void writeExpr(CodeWriter& writer, U32 idx, bool wrap = false, bool ignoreFrame = false) {
		if (idx < 0 || idx > _EXPR)
			return;
		if (!ignoreFrame)
			CheckFrameWrite();
		bool callWrap = wrap;
		bool shouldWrap = shouldWrapExpr(idx);
		wrap &= shouldWrap;
		Expression &expr = exprStack[idx];
		switch (expr.type)
		{
		case Expression::NONE: break;

		case Expression::FLOATLITERAL: writer.format("%g", expr.floatVal); break;
		case Expression::UINTLITERAL: writer.format("%u", expr.intVal); break;
		case Expression::STRINGLITERAL:
		case Expression::STRINGALLOC:
			if (expr.strVal) writer.append(expr.strVal);
			break;

		case Expression::UNARYOP:
		{
			switch (expr.op.val)
			{
			case OP_NOT:  writer.append("!"); break;
			case OP_NOTF: writer.append("!"); break;
			case OP_ONESCOMPLEMENT: writer.append("~"); break;
			case OP_NEG: writer.append("-"); break;
			}
			wrap &= shouldWrapExpr(idx - 1);
			if (wrap) writer.append("(");
			writeExpr(writer, idx - 1, shouldWrap, ignoreFrame);
			if (wrap) writer.append(")");
		} break;

		case Expression::BINARYOP:
		{
			U32 arg1 = idx - 1;
			U32 arg2 = walkExpr(arg1) - 1;
			if (wrap) writer.append("(");

			if (expr.op.val == OP_OR || expr.op.val == OP_AND)
				swap(arg1, arg2);

			writeExpr(writer, arg1, shouldWrap, ignoreFrame);
			switch (expr.op.val)
			{
			case OP_CMPEQ: writer.append(" == "); break;
			case OP_CMPGR: writer.append(" > "); break;
			case OP_CMPGE: writer.append(" >= "); break;
			case OP_CMPLT: writer.append(" < "); break;
			case OP_CMPLE: writer.append(" <= "); break;
			case OP_CMPNE: writer.append(" != "); break;
			case OP_XOR:   writer.append(" ^ "); break;
			case OP_MOD:   writer.append(" % "); break;
			case OP_BITAND:writer.append(" & "); break;
			case OP_BITOR: writer.append(" | "); break;
			case OP_SHR:   writer.append(" >> "); break;
			case OP_SHL:   writer.append(" << "); break;
			case OP_AND:   writer.append(" && "); break;
			case OP_OR:    writer.append(" || "); break;
			case OP_ADD:   writer.append(" + "); break;
			case OP_SUB:   writer.append(" - "); break;
			case OP_MUL:   writer.append(" * "); break;
			case OP_DIV:   writer.append(" / "); break;

			case OP_SAVEVAR_FLT:
			case OP_SAVEVAR_UINT:
			case OP_SAVEVAR_STR:
				writer.append(" = ");
				break;
			}
			writeExpr(writer, arg2, shouldWrap, ignoreFrame);
			if (wrap) writer.append(")");
		} break;

		case Expression::STRINGOP:
		{
			U32 arg1 = idx - 1;
			if (expr.op.val == OP_TERMINATE_REWIND_STR
				|| expr.op.val == OP_ADVANCE_STR_NUL
				)
			{
				writeExpr(writer, arg1, shouldWrap, ignoreFrame);
			}
			else if (expr.op.val == OP_REWIND_STR)
			{
				U32 arg2 = walkExpr(arg1) - 1;
				if (wrap) writer.append("(");
				writeExpr(writer, arg2, false, ignoreFrame);
				writeExpr(writer, arg1, false, ignoreFrame);
				if (wrap) writer.append(")");
			}
			else if (expr.op.val == OP_COMPARE_STR)
			{
				U32 arg2 = walkExpr(arg1) - 1;
				if (wrap) writer.append("(");
				writeExpr(writer, arg2, shouldWrap, ignoreFrame);
				writer.append(" $= ");
				writeExpr(writer, arg1, shouldWrap, ignoreFrame);
				if (wrap) writer.append(")");
			}
			else if (expr.op.val == OP_ADVANCE_STR_COMMA)
			{
				writeExpr(writer, arg1, shouldWrap, ignoreFrame);
				writer.append(',');
			}
			else if (expr.op.val == OP_ADVANCE_STR_APPENDCHAR)
			{
				U32 arg2 = walkExpr(arg1) - 1;
				writeExpr(writer, arg1, shouldWrap, ignoreFrame);
				switch (expr.op.arg)
				{
				case '\n': writer.append(" NL "); break;
				case '\t': writer.append(" TAB "); break;
				case ' ': writer.append(" SPC "); break;
				}
				//writeExpr(writer, arg2, shouldWrap, ignoreFrame);
			}
			else
			{
				U32 arg2 = walkExpr(arg1) - 1;
				Expression expr2 = exprStack[arg2];
				//writeExpr(writer, arg2, shouldWrap, ignoreFrame);
				writeExpr(writer, arg1, shouldWrap, ignoreFrame);
				if (expr2.type == Expression::STRINGOP
					&& (expr2.op.val == OP_ADVANCE_STR_APPENDCHAR || expr2.op.val == OP_ADVANCE_STR_COMMA))
					;
				else
					writer.append(" @ ");
			}
		} break;

		case Expression::ARRAYOP:
		{
			switch (expr.op.val)
			{
			case OP_SETCURVAR_ARRAY:
			case OP_SETCURVAR_ARRAY_CREATE:
				break;
			case OP_SETCURFIELD_ARRAY:
			{
				U32 arg1 = idx - 1;
				U32 arg2 = walkExpr(arg1) - 1;
				writeExpr(writer, arg2, false, ignoreFrame);
				writer.append(".");
				writeExpr(writer, arg1, false, ignoreFrame);
			} break;

			case OP_LOADFIELD_UINT:
			case OP_LOADFIELD_FLT:
			case OP_LOADFIELD_STR:
			{
				U32 arg1 = idx - 1;
				U32 arg2 = walkExpr(arg1) - 1;
				U32 arg3 = walkExpr(arg2) - 2; // array
				Expression &earg2 = exprStack[arg2];
				bool isArray = (earg2.type == Expression::ARRAYOP && earg2.op.val == OP_SETCURFIELD_ARRAY);
				if (isArray) {
					arg2 = arg2 - 1;
				}
				if (earg2.type == Expression::STRINGOP && earg2.strVal == NULL)
				{
					writeExpr(writer, arg1, false, ignoreFrame);
				}
				else
				{
					writeExpr(writer, arg2, false, ignoreFrame);
					writer.append(".");
					writeExpr(writer, arg1, false, ignoreFrame);
				}
				if (isArray)
				{
					U32 arg4 = walkExpr(arg3) - 1; // array
					writer.append("[");
					writeExpr(writer, arg3, shouldWrap, ignoreFrame);
					writer.append("]");
					arg3 = arg4;
				}
			} break;

			case OP_SAVEFIELD_FLT:
			case OP_SAVEFIELD_UINT:
			case OP_SAVEFIELD_STR:
			{
				U32 arg1 = idx - 1; // index
				U32 arg2 = walkExpr(arg1) - 1; // array
				U32 arg3 = walkExpr(arg2) - 2; // array

				Expression& earg2 = exprStack[arg2];
				if (earg2.type == Expression::BINARYOP) // TODO: special case hack for a.b++;  Honestly the curfield,curobject,curfieldarray needs a rewrite
				{
					writeExpr(writer, arg2-1, shouldWrap, ignoreFrame);
					writer.append(" = ");
					writeExpr(writer, arg2, shouldWrap, ignoreFrame);
				}
				else
				{
					// check if field is array type or not, if so then process additional arg
					bool isArray = (exprStack[arg2].type == Expression::ARRAYOP && exprStack[arg2].op.val == OP_SETCURFIELD_ARRAY);
					if (isArray) {
						arg2 = arg2 - 1;
					}
					if (exprStack[arg2].type == Expression::STRINGLITERAL && exprStack[arg2].strVal == NULL) {
						writeExpr(writer, arg1, false, ignoreFrame);
					}
					else {
						writeExpr(writer, arg2, false, ignoreFrame);
						writer.append(".");
						writeExpr(writer, arg1, false, ignoreFrame);
					}
					if (isArray)
					{
						U32 arg4 = walkExpr(arg3) - 1; // array
						writer.append("[");
						writeExpr(writer, arg3, shouldWrap, ignoreFrame);
						writer.append("]");
						arg3 = arg4;
					}
					writer.append(" = ");
					Expression &earg3 = exprStack[arg3];
					if (earg3.type == Expression::STRINGOP && earg3.op.val == OP_ADVANCE_STR){
						arg3 = arg3 - 1;
					}
					writeExpr(writer, arg3, shouldWrap, ignoreFrame);
				}
			} break;

			}
		} break;

		case Expression::GENERALOP:
		{
			switch (expr.op.val)
			{
			case OP_SETCUROBJECT_INTERNAL:
			{
				U32 arg1 = idx - 1;
				U32 arg2 = walkExpr(arg1) - 1;
				writeExpr(writer, arg2, false, ignoreFrame);
				writer.append("-->");
				writeExpr(writer, arg1, false, ignoreFrame);
				//popExpr();
			} break;

			case OP_STR_TO_NONE:
			case OP_FLT_TO_NONE:
			case OP_UINT_TO_NONE:
			{
				U32 arg1 = idx - 1;
				writeExpr(writer, arg1, false, ignoreFrame);
				writer.endLine();
				popExpr();
			} break;
			}
		} break;

		case Expression::JUMPOP:
		{
			bool invertJump = false;
			switch (expr.op.val)
			{
			case OP_JMPIF_NP:
			case OP_JMPIFNOT_NP:
			{
				writeExpr(writer, idx - 1, callWrap, ignoreFrame); // skip to next
			}	break;

			case OP_JMPIF:
			case OP_JMPIFF:
				invertJump = true;
			case OP_JMPIFNOT:
			case OP_JMPIFFNOT:
			{
				U32 frameidx = expr.op.arg;
				Frame& frame = frameStack[frameidx];
				int expr = frame.expr;
				writer.append("if (");
				if (invertJump) writer.append("!");
				writeExpr(writer, expr - 1, invertJump, ignoreFrame);
				writer.appendline(")");
				writer.startBlock();
			}	break;

			case OP_JMP:
				writer.appendline("else").startBlock();
				break;
			}
		} break;

		case Expression::SPECIALOP:
		{
			switch (expr.op.val)
			{
			case 0:
				U32 arg3 = walkExpr(idx - 1);
				U32 elsebr = walkExpr(arg3 - 1);
				U32 arg2 = walkExpr(elsebr - 1);
				U32 cond = arg2 - 2;

				writeExpr(writer, cond, false, ignoreFrame);
				writer.append(" ? ");
				writeExpr(writer, arg2, false, ignoreFrame);
				writer.append(" : ");
				writeExpr(writer, arg3, false, ignoreFrame);
				break;
			}
		} break;

		}
	}

	bool isJumpExpr(U32 ip)
	{
		U32 *code = block.code;
		switch (code[ip])
		{
		case OP_JMPIFFNOT:
		case OP_JMPIFNOT:
		case OP_JMPIFF:
		case OP_JMPIF:
		case OP_JMPIFNOT_NP:
		case OP_JMPIF_NP:
		case OP_JMP:
			return true;
		}
		return false;
	}

	Frame::FrameType FrameTypeForIp(U32 ip)
	{
		for (int i = _FRAME - 1; i > 0; --i)
		{
			Frame& frame = frameStack[i];
			if (ip >= frame.start && ip <= frame.end)
				return frame.type;
		}
		return Frame::NONE;
	}

	// return true if instruction pointer matches ending for a while loop in stack
	bool CheckWhileEnd(U32 ip)
	{
		for (int i = _FRAME - 1; i > 0; --i)
		{
			Frame& frame = frameStack[i];
			if (frame.type == Frame::WHILE && frame.end == ip)
				return true;
		}
		return false;
	}

	String exec(U32 ip, U32 end)
	{
		U32 *code = block.code;
		U32 codeSize = block.codeSize;
		U32 iterDepth = 0;
		bool invertJump = false;
		StringTableEntry curVar = NULL, curField = NULL, curObject = NULL;
		U32 inObject = 0;
		reset();
		UpdatePackage(NULL);// start out in global scope		

		for (Compiler::CompiledInstructions instruction = (Compiler::CompiledInstructions)code[curInstructionPointer = ip++]
			; ip < codeSize
			; instruction = (Compiler::CompiledInstructions)code[curInstructionPointer = ip++])
		{
			switch (instruction) {
			default:
				ASSERT1(1, "Unhandled instruction: %d\n", instruction);
				break;
			case OP_FUNC_DECL:
			{
				StringTableEntry fnName = U32toSTE(code[ip]);
				StringTableEntry fnNamespace = U32toSTE(code[ip + 1]);
				StringTableEntry fnPackage = U32toSTE(code[ip + 2]);
				bool hasBody = bool(code[ip + 3]);
				U32 newIp = code[ip + 4];
				U32 argc = code[ip + 5];

				UpdatePackage(fnPackage);

				CheckFrameWrite();
				CodeWriter &writer = (*this->curWriter);
				writer.append("function ");
				if (fnNamespace) {
					writer.format("%s::%s", fnNamespace, fnName);
				}
				else {
					writer.append(fnName);
				}
				writer.append("(");
				for (int i = 0; i < argc; ++i){
					StringTableEntry argName = U32toSTE(code[ip + 6 + i]);
					if (i != 0) writer.append(", ");
					writer.append(argName && argName[0] ? argName : "%unused");
				}
				writer.append(")");
				if (hasBody) {
					writer.appendline().startBlock();
					SetFunctionTable();
					PushFrameEnd(Frame::FUNC, ip, newIp, true);
				}
				else {
					writer.endLine();
				}
				ip += 6 + argc;
				break;
			}

			case OP_RETURN:
			{
				CheckFrameWrite();
				CheckPackageScope();
				CodeWriter &writer = (*this->curWriter);
				writer.append("return ");
				writeCurrentExpr(writer);
				writer.endLine();
				popExpr();
			} break;
			case OP_RETURN_VOID:
			{
				CheckFrameWrite();
				CheckPackageScope();
				CodeWriter &writer = (*this->curWriter);
				Frame& curFrame = GetCurrentFrame();
				if (_FRAME == 0 || (curFrame.type == Frame::FUNC && curFrame.end == ip)) {
					; // skip return for last instruction of frame
				} else {
					writer.append("return").endLine();
				}
				collapseExpr();
			} break;

			case OP_SETCURVAR:
			case OP_SETCURVAR_CREATE:
				curVar = U32toSTE(code[ip++]);
				break;

				// Establish the call frame. Track where call args currently is
			case OP_PUSH_FRAME:
				callFrame[_CALLFR++] = _CALLARGS;
				break;

			case OP_PUSH:
				callArgs[_CALLARGS++] = _EXPR;
				curVar = NULL;
				break;

			case OP_LOADVAR_UINT:
			case OP_LOADVAR_FLT:
			case OP_LOADVAR_STR:
				if (curVar){
					pushExpr().set(curVar);
				}
				break;

			case OP_TAG_TO_STR:
			{
				const char* str = curStringTable ? curStringTable + code[ip++] : "";
				CodeWriter literalStr;
				literalStr.append('\'').append(String(str).expandEscapes()).append('\'');
				curVar = StringTable->insert(literalStr.end());
				pushExpr().set(curVar);				
			}	break;

			case OP_LOADIMMED_STR:
			{
				const char* str = curStringTable ? curStringTable + code[ip++] : "";
				CodeWriter literalStr;
				literalStr.appendLiteral(str);
				curVar = StringTable->insert(literalStr.end());
				pushExpr().set(curVar);				
			} break;

			case OP_DOCBLOCK_STR:
			{
				CheckFrameWrite();
				CheckPackageScope();
				String str(curStringTable ? curStringTable + code[ip++] : "");
				str.replace("\n", "\n///");
				if (str.endsWith("///"))
					str.erase(str.length() - 3, 3);
				curWriter->append("///").append(str.c_str()).appendline();
			} break;
			case OP_LOADIMMED_IDENT: 
				pushExpr().set(U32toSTE(code[ip++]));
				break;

			case OP_LOADIMMED_UINT: 
				pushExpr().set(code[ip++]); 
				break;

			case OP_LOADIMMED_FLT:  
				pushExpr().set(curFloatTable[code[ip++]]); 
				break;

			case OP_SAVEVAR_FLT:
			case OP_SAVEVAR_UINT: 
			case OP_SAVEVAR_STR:
				pushExpr().set(curVar);
				pushExpr().setOp(instruction);
				break;

			case OP_CALLFUNC_RESOLVE:
			case OP_CALLFUNC:
			{
				bool ignoreFrames = true; // ignore frames in expr writing so IFCOND is not written out 
				//CheckFrameWrite();
				CheckPackageScope();
				CodeWriter writer;
				StringTableEntry fnNamespace = U32toSTE(code[ip + 1]);
				StringTableEntry fnName = U32toSTE(code[ip]);
				U32 callType = code[ip + 2];
				StringTableEntry *callArgv;
				U32 argsPos = callFrame[--_CALLFR];
				U32 argOffset = 0;
				if (callType == FuncCallExprNode::MethodCall)
				{
					U32 exprStart = callArgs[argsPos];
					writeExpr(writer, exprStart, true, ignoreFrames);
					writer.append('.');
					argOffset = 1;
				}
				if (fnNamespace && fnNamespace[0])
				{
					writer.append(fnNamespace);
					writer.append("::");
				}
				writer.append(fnName).append("(");
				ip += 3;

				U32 argc = _CALLARGS - argsPos;
				U32 frameStart = _EXPR;
				for (U32 i = argOffset; i < argc; ++i)
				{					
					if (i > argOffset) writer.append(", ");
					U32 exprStart = callArgs[argsPos + i];
					writeExpr(writer, exprStart, false, ignoreFrames);
				}
				for (U32 i = 0; i < argc; ++i) popExpr();
				// cleanup 
				_CALLARGS = argsPos;
				writer.append(")");

				pushExpr().set(writer.end(), true);
			} break;


			// inline OR, AND statements
			case OP_JMPIF_NP:
			case OP_JMPIFNOT_NP:
			{
				U32 newIp = code[ip++];
				ASSERT(newIp < ip, "Unexpected JUMP location for IF statements");
				Frame& prevFrame = GetCurrentFrame();
				if (prevFrame.type == Frame::IFCOND /*&& !prevFrame.written*/)
				{
					Compiler::CompiledInstructions prevInstruction = (Compiler::CompiledInstructions)code[prevFrame.start-1];
					pushExpr().setOp(prevInstruction == OP_JMPIFNOT_NP ? OP_AND : OP_OR);// push back on stack with arg = 1 so we can build complex expression
					//PushFrameEnd(Frame::IFCOND, ip - 1, newIp);
					//pushExpr().setOp(instruction, _FRAME - 1);
					prevFrame.type = Frame::IFCOND;
					prevFrame.start = ip - 1;
					prevFrame.end = newIp;
					prevFrame.expr = _EXPR;
				}
				else
				{
					if (isJumpExpr(newIp))
					{
						U32 nextIp = code[newIp + 1]; // follow next jump
						if (nextIp > newIp)
							newIp = nextIp; // extend the jump range on inline if statements
					}

					//pushExpr().setOp(instruction, 0);
					PushFrameEnd(Frame::IFCOND, ip - 1, newIp);
				}
			} break;

			// standard if statements (float and integer variants)
			case OP_JMPIF:
			case OP_JMPIFF:
			case OP_JMPIFNOT:
			case OP_JMPIFFNOT:
			{
				CodeWriter& writer = (*this->curWriter);

				U32 newIp = code[ip++];
				if (newIp > ip) // ignore backward jumps and only deal with foward jump
				{
					CheckFrameEnd(ip);

					// Test for if/for/while conditions
					bool handled = false;
					if (newIp != ip && isJumpExpr(newIp - 2))
					{
						U32 retAddr = code[newIp - 1];
						if (retAddr == ip)
						{
							CheckFrameWrite();
							CheckPackageScope();
							PushFrameEnd(Frame::WHILE, ip - 1, newIp,true);
							writer.append("while (");
							if (invertJump) writer.append("!");
							writeCurrentExpr(writer, invertJump);
							writer.appendline(")");
							popExpr();
							writer.startBlock();
							handled = true;
						}
					}
					if (!handled)
					{
						Frame& prevFrame = GetCurrentFrame();
						if (prevFrame.type == Frame::IFCOND )// && !prevFrame.written)
						{
							pushExpr().setOp(instruction == OP_JMPIFNOT || instruction == OP_JMPIFFNOT ? OP_AND : OP_OR);// push back on stack with arg = 1 so we can build complex expression
							//pushExpr().setOp(instruction, _FRAME - 1);
							//prevFrame.type = Frame::IFCOND;
							//prevFrame.start = ip - 1;
							//prevFrame.end = newIp;
							//prevFrame.expr = _EXPR;
							//Expression& prevExpr = exprStack[prevFrame.expr];
							//pushExpr().setOp(instruction == OP_JMPIF_NP ? OP_OR : OP_AND);// push back on stack with arg = 1 so we can build complex expression
							//pushExpr().setOp(instruction, _FRAME - 1);
							//prevFrame.type = Frame::IF;
							//prevFrame.start = ip - 1;
							//prevFrame.end = newIp;
							//prevFrame.expr = _EXPR;
							//handled = true;
						}
					}
					if (!handled)
					{
						pushExpr().setOp(instruction, _FRAME);
						PushFrameEnd(Frame::IF, ip - 1, newIp);
					}
				}
				else if (newIp == ip) // basically empty if condition
				{
					bool handled = false;
					if (newIp != ip && isJumpExpr(newIp - 2))
					{
						U32 retAddr = code[newIp - 1];
						if (retAddr == ip)
						{
							CheckFrameWrite();
							CheckPackageScope();
							PushFrameEnd(Frame::WHILE, ip - 1, newIp, true);
							writer.append("while (");
							if (invertJump) writer.append("!");
							writeCurrentExpr(writer, invertJump);
							writer.appendline(")");
							popExpr();
							writer.startBlock();
							handled = true;
						}
					}
					if (!handled)
					{
						Frame& prevFrame = GetCurrentFrame();
						if (prevFrame.type == Frame::IFCOND)// && !prevFrame.written)
						{
							pushExpr().setOp(instruction == OP_JMPIFNOT || instruction == OP_JMPIFFNOT ? OP_AND : OP_OR);// push back on stack with arg = 1 so we can build complex expression
						}
					}
					if (!handled)
					{
						pushExpr().setOp(instruction, _FRAME);
						PushFrameEnd(Frame::IF, ip - 1, newIp);
					}
				}
				else
				{
					if (GetCurrentFrame().type == Frame::WHILE)
					{
						popExpr();
					}
					CheckFrameEnd(ip + 1);
				}
			} break;

			case OP_JMP:
			{
				CodeWriter& writer = (*this->curWriter);
				U32 newIp = code[ip++];
				if (code[newIp] == OP_ITER_END)
				{
					CheckFrameEnd(ip + 1);
					writer.append("break").endLine();
				}
				else if (newIp > ip)
				{
					// check for inline iff (update frame if so)
					Frame& prevFrame = GetCurrentFrame();
					bool handled = false;
					if (prevFrame.type == Frame::IF && !prevFrame.written)
					{
						if (prevFrame.start - 1 == ip - 2 - 2)
						{
							Frame::FrameType type = FrameTypeForIp(newIp);
							if (type == Frame::WHILE) {
								CheckFrameWrite();
								writer.append("continue").endLine();
								CheckFrameEnd(ip + 1);
								handled = true;
							}
						}
						else
						{
							pushExpr().setOp(instruction, _FRAME);
							prevFrame.type = Frame::INLINEIF;
							prevFrame.start = ip - 2;
							prevFrame.end = newIp;
							prevFrame.expr = _EXPR;
							handled = true;
						}
					}
					if (!handled)
					{
						// check for matching while at same ending location
						if (CheckWhileEnd(newIp))
						{
							// default to else
							//CheckFrameEnd(ip);
							writer.append("break").endLine();
							CheckFrameEnd(ip + 1);
						}
						else
						{
							// default to else
							CheckFrameEnd(ip + 1);
							writer.appendline("else").startBlock();
							PushFrameEnd(Frame::ELSE, ip - 2, newIp);
						}
					}
				}
				else {
					CheckFrameEnd(ip + 1);
				}
				//writeCurrentExpr(writer);
				//popExpr();				
			} break;

			
			case OP_CMPEQ:
			case OP_CMPGR:
			case OP_CMPGE:
			case OP_CMPLT:
			case OP_CMPLE:
			case OP_CMPNE:
			case OP_XOR:
			case OP_MOD:
			case OP_BITAND:
			case OP_BITOR:
			case OP_NOT:
			case OP_NOTF:
			case OP_ONESCOMPLEMENT:
			case OP_SHR:
			case OP_SHL:
			case OP_AND:
			case OP_OR:
			case OP_ADD:
			case OP_SUB:
			case OP_MUL:
			case OP_DIV:
			case OP_NEG:
				pushExpr().setOp(instruction);
				break;

			case OP_ADVANCE_STR:
				pushExpr().setOp(instruction);
				break;
			case OP_ADVANCE_STR_COMMA:
			case OP_ADVANCE_STR_NUL:
			case OP_REWIND_STR:
			//case OP_TERMINATE_REWIND_STR:
			case OP_COMPARE_STR:
				pushExpr().setOp(instruction);
				break;
			case OP_TERMINATE_REWIND_STR:
			{
				if (curExpr().type == Expression::STRINGOP)
				{
					switch (curExpr().op.val)
					{
					case OP_ADVANCE_STR:
						exprStack[_EXPR--].reset(); // remove the "advance_str" op?
						break;
					}
				}
			} break;
			case OP_ADVANCE_STR_APPENDCHAR:
			{
				pushExpr().setOp(instruction, code[ip++]);
			}	break;


			case OP_STR_TO_UINT:
			case OP_FLT_TO_UINT:
			case OP_STR_TO_FLT:
			case OP_FLT_TO_STR:
			case OP_UINT_TO_FLT:
			case OP_UINT_TO_STR:
				break; // ignore
			case OP_STR_TO_NONE:
			case OP_FLT_TO_NONE:
			case OP_UINT_TO_NONE:
				CheckFrameWrite();
				CheckPackageScope();
				writeCurrentExpr(*curWriter);
				(*curWriter).endLine();
				popExpr();
				break;

			case OP_SETCURVAR_ARRAY:
			case OP_SETCURVAR_ARRAY_CREATE:
			{
				CodeWriter literalStr;

				U32 arg1 = _EXPR-1; // index
				U32 arg2 = walkExpr(arg1) - 2; // array

				writeExpr(literalStr, arg2);
				literalStr.append("[");
				writeExpr(literalStr, arg1); // TODO: need to convert string to integer if quoted string
				literalStr.append("]");
				popExpr();
				curVar = StringTable->insert(literalStr.end());
			} break;

			case OP_CREATE_OBJECT:
			{
				CheckFrameWrite();
				CheckPackageScope();
				// Read some useful info.
				StringTableEntry objParent = U32toSTE(code[ip]);
				bool isDataBlock = code[ip + 1];
				bool isInternal = code[ip + 2];
				bool isSingleton = code[ip + 3];
				U32  lineNumber = code[ip + 4];
				U32  failJump = code[ip + 5];


				this->curWriter = &writerArray[++_WRITER];

				CodeWriter &defWriter = (*this->curWriter);
				defWriter.reset();

				writerArray[_WRITER].mIndent = writerArray[_WRITER - 1].mIndent;

				StringTableEntry *callArgv;
				if (isDataBlock)
					defWriter.append("datablock ");
				else if (isSingleton)
					defWriter.append("singleton ");
				else
					defWriter.append("new ");
				
				U32 argsPos = callFrame[--_CALLFR];
				U32 argc = _CALLARGS - argsPos;
				U32 frameStart = _EXPR;
				for (U32 i = 0; i < argc; ++i)
				{
					if (i > 1) defWriter.append(", ");
					U32 exprStart = callArgs[argsPos + i];
					writeExpr(defWriter, exprStart);
					if (i == 0) 
						defWriter.append("(");
					else if (objParent && objParent[0])
					{
						defWriter.append(" : ");
						defWriter.append(objParent);
					}
				}
				// cleanup 
				_CALLARGS = argsPos;
				U32 exprStart = (argsPos == 0) ? 0 : callArgs[argsPos];
				while (_EXPR > exprStart) {
					popExpr();
				}
				if (argsPos) popExpr();
				_EXPR = exprStart;
				defWriter.append(")");

				// Advance the IP past the create info...

				// check if empty block or not
				if (code[ip + 6] != OP_ADD_OBJECT || code[ip + 7] != OP_FINISH_OBJECT) {
					defWriter.appendline().startBlock();
					PushFrameEnd(Frame::OBJECT,ip,failJump+1);
				}
				ip += 6;
				break;
			}
			case OP_ADD_OBJECT:
			{
				bool placeAtRoot = code[ip++];
			} break;
			case OP_END_OBJECT:
			{
				bool placeAtRoot = code[ip++];
				GetCurrentFrame().placeAtRoot = placeAtRoot;

			} break;

			case OP_FINISH_OBJECT: 
			{
				Frame& curFrame = GetCurrentFrame();
				bool placeAtRoot = curFrame.placeAtRoot;

				CheckFrameWrite();
				CheckPackageScope();
				CheckFrameEnd(ip);
				CodeWriter &objWriter = (*this->curWriter);
				// writeCurrentExpr(objWriter); // why do I need this ????
				//objWriter.endLine();
				popExpr();
				this->curWriter = &writerArray[--_WRITER];
				if (placeAtRoot) {
					pushExpr().set(objWriter.end(), true);
				} else {
					this->curWriter->append(objWriter.end()).endLine().needLine();
				}
				objWriter.reset();
				//PopFrame();
			} break;

			case OP_SETCUROBJECT:
				break;
			case OP_SETCUROBJECT_NEW:
				curObject = NULL;
				pushExpr().set(curObject);
				break;
			case OP_SETCUROBJECT_INTERNAL:
			{	
				int recurse = code[ip++];
				pushExpr().setOp(instruction);
			}	break;

			case OP_SETCURFIELD:
				curField = U32toSTE(code[ip++]);
				break;

			case OP_SETCURFIELD_ARRAY:
			{
				pushExpr().setOp(instruction);
			}break;

			case OP_SETCURFIELD_TYPE:
				ip++;
				break;

			case OP_LOADFIELD_UINT:
			case OP_LOADFIELD_FLT:
			case OP_LOADFIELD_STR:
				pushExpr().set(curField);
				pushExpr().setOp(instruction);
				break;

			case OP_SAVEFIELD_UINT:
			case OP_SAVEFIELD_FLT:
			case OP_SAVEFIELD_STR:
				pushExpr().set(curField);
				pushExpr().setOp(instruction);
				break;


			case OP_ASSERT:
			{
				CheckFrameWrite();
				CheckPackageScope();

				CodeWriter &writer = (*this->curWriter);
				StringTableEntry  str = curStringTable ? curStringTable + code[ip++] : "";
				//StringTableEntry alertString = U32toSTE(code[ip++]);
				writer.append("assert(");
				U32 arg1 = _EXPR; // index
				writeExpr(writer, arg1);
				writer.append(", ").appendLiteral(str).append(")").endLine();
				popExpr();
			} break;
			case OP_BREAK:
				break;

			case OP_ITER_BEGIN:       ///< Prepare foreach iterator.
			case OP_ITER_BEGIN_STR:   ///< Prepare foreach$ iterator.
			{
				CheckFrameWrite();
				CheckPackageScope();
				StringTableEntry varName = U32toSTE(code[ip]);
				U32 failIp = code[ip + 1];

				U32 arg1 = _EXPR; // index
				//U32 arg2 = walkExpr(arg1) - 2; // array

				CodeWriter &forWriter = (*this->curWriter);
				forWriter.append("foreach ( ");
				if (varName && varName[0])
					forWriter.append(varName);
				else
					forWriter.append("%unused");
				forWriter.append(" in ");
				writeExpr(forWriter, arg1);
				forWriter.append(")");
				popExpr(); popExpr();
				forWriter.appendline().startBlock();
				PushFrameEnd(Frame::ITER,ip,failIp);

				ip += 2;
			} break;
			
			case OP_ITER:             ///< Enter foreach loop.
				ip++;
				break;
			case OP_ITER_END:         ///< End foreach loop.
				// cleanup
				break;
			}

			CheckFrameEnd(ip);
		}
		CheckFrameWrite();
		UpdatePackage(NULL);// close out the global scope
		return this->curWriter->appendline().end();
	}
};

extern
String Decompile(CodeBlock& cb){
	Decompiler decomp(cb);
	return decomp.exec(0, cb.codeSize);
}

const char *CodeBlock::exec(U32 ip, const char *functionName, Namespace *thisNamespace, 
	U32 argc, const char **argv, bool noCalls, StringTableEntry packageName, S32 setFrame)
{
	return NULL;
}
