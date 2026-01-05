using System;
using System.IO;
using System.Collections;
using System.Diagnostics;

using LibClang;

namespace Cpp2Beef;

abstract class Cpp2BeefGenerator
{
	protected abstract Span<char8*> Args { get; }
	protected abstract StreamWriter GetWriterForHeader(StringView header);

	protected virtual bool IndentBlocks => true;

	protected virtual void GetNameInBindings(CXCursor cursor, String outString)
	{
		let spelling = GetCursorSpelling!(cursor);
		getName: switch (cursor.kind)
		{
		case .TypedefDecl:
			switch (spelling)
			{
			case "size_t": outString.Append("c_size");
			case "wchar_t": outString.Append("c_wchar");
			case "va_list": outString.Append("VarArgs");
			case "int8_t": outString.Append("int8");
			case "int16_t": outString.Append("int16");
			case "int32_t": outString.Append("int32");
			case "int64_t": outString.Append("int64");
			case "uint8_t": outString.Append("uint8");
			case "uint16_t": outString.Append("uint16");
			case "uint32_t": outString.Append("uint32");
			case "uint64_t": outString.Append("uint64");
			default: fallthrough getName;
			}
		default:
			Compiler.Identifier.GetSourceName(spelling, outString);
		}
	}

	enum AttrFlags { None = 0, Packed = 1, NoDiscard = 2 }
	protected virtual void WriteCustomAttributes(CXCursor cursor)
	{
		AttrFlags attrs = 0;
		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			AttrFlags* attrs = (.)client_data;
			switch (cursor.kind)
			{
			case .PackedAttr: *attrs |= .Packed;
			case .WarnUnusedResultAttr: *attrs |= .NoDiscard;
			default:
			}
		   	return .Continue;
		}, &attrs);

		if (cursor.kind == .EnumDecl) str.Append("[AllowDuplicates] ");
		else if (attrs.HasFlag(.Packed)) str.Append("[Packed] ");
		if (attrs.HasFlag(.NoDiscard)) str.Append("[NoDiscard] ");
	}

	protected virtual enum { case NoBlock, StaticBlock, CustomBlock(StringView); } GetCursorBlock(CXCursor cursor)
	{
		switch (cursor.kind)
		{
		case .StructDecl,
			 .ClassDecl,
			 .UnionDecl,
			 .EnumDecl,
			 .TypeAliasDecl,
			 .TypedefDecl,
			 .Namespace: return .NoBlock;

		case .FunctionDecl,
			 .CXXMethod,
			 .Constructor,
			 .Destructor,
			 .ConversionFunction,
			 .FieldDecl,
			 .VarDecl,
			 .MacroDefinition: return .StaticBlock;

		default: Runtime.FatalError(scope $"Missing GetCursorBlock implementation for {_}");
		}
	}

	protected void BeginCursor(CXCursor cursor)
	{
		decltype(GetCursorBlock(default)) cxblock = default;
		void WriteBlock()
		{
			StringView target;
			switch (cxblock)
			{
			case .NoBlock: target = "";
			case .StaticBlock: target = "static";
			case .CustomBlock(let p0): target = p0;
			}

			if (block == target) return;
			if (!block.IsEmpty)
			{
				if (IndentBlocks) indent.Length--;
				str.TrimEnd();
				str.Append("\n", indent, "}\n");
			}
			block.Set(target);

			switch (cxblock)
			{
			case .NoBlock:
			case .StaticBlock:
				str.Append("\n", indent, "static\n");
				str.Append(indent, "{\n");
				if (IndentBlocks) indent.Append('\t');
			case .CustomBlock(let p0):
				str.Append("\n", indent, "extension ", p0, "\n");
				str.Append(indent, "{\n");
				if (IndentBlocks) indent.Append('\t');
			}
		}

		if (canChangeBlock)
		{
			cxblock = GetCursorBlock(cursor);
			WriteBlock();
			defer:: WriteBlock();
		}
		WriteComments(cursor);
		WriteQueuedOpenSquirly();
	}

	protected virtual void HandleCursor(CXCursor cursor)
	{
		switch (cursor.kind)
		{
		case .StructDecl,
			 .ClassDecl,
			 .UnionDecl,
			 .EnumDecl:
			if (templateParams.IsEmpty && Clang.EqualCursors(cursor, Clang.GetTypeDeclaration(Clang.GetCursorType(cursor))) == 0)
				return;
			BeginCursor(cursor);
			if (_ case .EnumDecl) Enum(cursor);
			else Record(cursor);

		case .TypeAliasDecl, .TypedefDecl: BeginCursor(cursor); TypeAlias(cursor);
		case .Namespace: BeginCursor(cursor); Namespace(cursor);

		case .FunctionDecl: BeginCursor(cursor); FunctionDecl(cursor);
		case .CXXMethod,
			 .Constructor,
			 .Destructor,
			 .ConversionFunction:
			if (templateParams.IsEmpty && Clang.EqualCursors(Clang.GetCursorLexicalParent(cursor), Clang.GetCursorSemanticParent(cursor)) == 0)
				return;
			BeginCursor(cursor);
			CXXMethod(cursor);

		case .FieldDecl: BeginCursor(cursor); FieldDecl(cursor);
		case .VarDecl: BeginCursor(cursor); VarDecl(cursor);

		case .MacroDefinition:
			if (Clang.Cursor_IsMacroFunctionLike(cursor) != 0) return;
			let tokens = ScopeTokenize!(cursor, unit);
			if (tokens.Length <= 1) return;
			BeginCursor(cursor);
			MacroDefinition(cursor);

		case .InclusionDirective, .MacroExpansion, .CXXBaseSpecifier, .CXXAccessSpecifier,
			 .LinkageSpec, .TemplateTypeParameter, .NonTypeTemplateParameter: // ignored
		default: Debug.WriteLine(scope $"Unhandled cursor: {_}");
		}
	}

	protected virtual bool IsOutParam(CXCursor arg, CXCursor method)
	{
		let comment = Clang.Cursor_GetParsedComment(method);
		let spelling = GetCursorSpelling!(arg);
		for (let i < Clang.Comment_GetNumChildren(comment))
		{
			let param = Clang.Comment_GetChild(comment, i);
			if (Clang.Comment_GetKind(param) != .ParamCommand) continue;
			if (ScopeCXString!(Clang.ParamCommandComment_GetParamName(param)) != spelling) continue;
			return Clang.ParamCommandComment_GetDirection(param) == .Out;
		}

		return false;
	}

	protected virtual void ModifyWrapperPrintingPolicy(CXPrintingPolicy policy) {}

	protected CXIndex index = Clang.CreateIndex(excludeDeclarationsFromPCH: 0, displayDiagnostics: 1) ~ Clang.DisposeIndex(_);
	protected CXTranslationUnit unit;

	protected append String str = .(1024);
	protected append String wrapperBuf = .(1024);
	protected CXCursor currentCursor = Clang.GetNullCursor();

	private append String block = .(64);
	protected append String indent = .(8);
	private bool canChangeBlock = true;
	private bool queuedOpenSquirly = false;
	protected append String fullCursorName = .(256);
	protected CXPrintingPolicy printingPolicy;

	private StreamWriter currentWritter;
	private StreamWriter wrapperWritter;
	protected StringView WrapperFilePath;

	private append String wrapperTemplateChain = .(16);
	private append String templateParams = .(16);
	private append String templateParamsWhere = .(64);
	private String defferedWrapperWrite = null;

	private struct UnitMacroIndex : this(uint32 line, CXFile file), IHashable
	{
		public int GetHashCode() => line;
		public static bool operator==(Self lhs, Self rhs) => lhs.line == rhs.line && Clang.File_IsEqual(lhs.file, rhs.file) != 0;
	}
	protected CXSourceLocation prevEnd = Clang.GetNullLocation();
	private Dictionary<String, CXSourceLocation> files_prevEnds = new .(32) ~ DeleteDictionaryAndKeys!(_);
	private append Dictionary<UnitMacroIndex, CXCursor> unitMacros = .(128);

	public static mixin ScopeCXString(CXString str)
	{
		defer:mixin Clang.DisposeString(str);
		StringView(Clang.GetCString(str))
	}

	public static mixin GetCursorSpelling(CXCursor cursor)
	{
		ScopeCXString!:mixin(Clang.GetCursorSpelling(cursor))
	}

	protected mixin GetTokenSpelling(CXToken token)
	{
		ScopeCXString!:mixin(Clang.GetTokenSpelling(unit, token))
	}

	protected virtual void PreGeneration() {}
	protected virtual void PostGeneration() {}

	public enum GenerationError
	{
		ParsingFailed
	}

	public Result<void, GenerationError> Generate(char8* headerPath, StringView wrapperPath)
	{
		unitMacros.Clear();
		files_prevEnds.Clear();
		prevEnd = Clang.GetNullLocation();

		wrapperWritter = scope .();
		if (!wrapperPath.IsNull)
			wrapperWritter.Create(wrapperPath);
		else
			wrapperWritter = null;
		WrapperFilePath = wrapperPath;
		wrapperBuf.Set("""
			#define private public
			#define protected public

			#include <
			""");
		wrapperBuf.Append(headerPath);
		wrapperBuf.Append("""
			>
			#include <stdint.h>

			template <typename T>
			using __type = T;

			extern "C"
			{

			""");

		let args = Args;
#if DEBUG
		findLang: do
		{
			for (let arg in args)
			{
				StringView view = .(arg);
				if (view == "-x" || view.StartsWith("--language"))
					break findLang;
			}
			Runtime.FatalError("You must set a language via Args (e.g. --language=c++)");
		}
#endif
		CXTranslationUnit_Flags unitFlags = .SkipFunctionBodies | .DetailedPreprocessingRecord;
		unit = Clang.ParseTranslationUnit(index, headerPath, args.Ptr, (.)args.Length, null, 0, (.)unitFlags);
		if (unit == null) return .Err(.ParsingFailed);

		printingPolicy = Clang.GetCursorPrintingPolicy(Clang.GetTranslationUnitCursor(unit));
		ModifyWrapperPrintingPolicy(printingPolicy);
		defer Clang.PrintingPolicy_Dispose(printingPolicy);

		PreGeneration();
		Clang.VisitChildren(Clang.GetTranslationUnitCursor(unit), (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);

			CXSourceLocation* prevEndPtr;
			{
				let location = Clang.GetCursorLocation(cursor);
				Clang.GetSpellingLocation(location, let file, let line, ?, ?);
				let header = ScopeCXString!(Clang.GetFileName(file));
				self.currentWritter = self.GetWriterForHeader(header);
				if (self.currentWritter == null) return .Continue;

				if (cursor.kind == .MacroDefinition)
				{
					self.unitMacros.Add(.(line, file), cursor);
					return .Continue;
				}

				if (self.files_prevEnds.TryGetRefAlt(header, ?, out prevEndPtr))
					self.prevEnd = *prevEndPtr;
				else
				{
					self.prevEnd = Clang.GetLocationForOffset(self.unit, file, 2);
					switch (self.files_prevEnds.TryAdd(new .(header)))
					{
					case .Added(?, out prevEndPtr):
					case .Exists: Runtime.FatalError();
					}
				}
			}

			self.WriteCursor(cursor);
			*prevEndPtr = self.prevEnd;

			return .Continue;
		}, Internal.UnsafeCastToPtr(this));
		PostGeneration();

		if (wrapperBuf.IsEmpty)
			wrapperWritter.Write("}\n\n//begin-comptime\n");

		return .Ok;
	}

	protected void WriteCursor(CXCursor cursor, bool macro = false)
	{
		if ((macro) != (cursor.kind == .MacroDefinition)) return;

		CXCursor prevCursor = currentCursor;
		currentCursor = cursor;
		defer { currentCursor = prevCursor; }

		int removeLenFullCursorName;
		{
			int curLen = fullCursorName.Length;
			if (!fullCursorName.IsEmpty)
				fullCursorName.Append("::");
			fullCursorName.Append(GetCursorSpelling!(cursor));
			removeLenFullCursorName = fullCursorName.Length - curLen;
		}
		defer { fullCursorName.Length -= removeLenFullCursorName; }

		var cursor;
		int removeLenWrapperTemplateChain = 0;
		defer { wrapperTemplateChain.Length -= removeLenWrapperTemplateChain; }
		if (!macro)
		{
			templateParams.Clear();
			templateParamsWhere.Clear();
			switch (cursor.kind)
			{
			case .FunctionTemplate, .ClassTemplate, .TypeAliasTemplateDecl:
				templateParams.Append('<');
				let len = wrapperTemplateChain.Length;
				Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
				{
					Self self = (.)Internal.UnsafeCastToObject(client_data);
					switch (cursor.kind)
					{
					case .TemplateTypeParameter:
					case .NonTypeTemplateParameter:
						self.templateParamsWhere.Append(" where ", GetCursorSpelling!(cursor), " : const ");
						self.Flush();
						self.Type(Clang.GetCursorType(cursor));
						self.templateParamsWhere.Append(self.str);
						self.str.Clear();
					case .TemplateTemplateParameter:
						Runtime.FatalError("C++ template template parameters are not supported");
					default: return .Continue;
					}
					let spelling = GetCursorSpelling!(cursor);
					self.templateParams.Append(spelling, ", ");
					self.wrapperTemplateChain.Append("__{typeof(", spelling, ")}");
					return .Continue;
				}, Internal.UnsafeCastToPtr(this));
				templateParams.Length -= 2;
				templateParams.Append('>');
				removeLenWrapperTemplateChain = wrapperTemplateChain.Length - len;
				cursor.kind = Clang.GetTemplateCursorKind(cursor);
			default:
			}
		}

		HandleCursor(cursor);
		Flush();
	}

	protected void Flush()
	{
		currentWritter.Write(str);
		str.Clear();
	}
	protected void FlushWrapper()
	{
		wrapperWritter.Write(wrapperBuf);
		wrapperBuf.Clear();
	}

	protected virtual void AllWhiteSpaceUntil(CXSourceLocation from)
	{
		StringView file;
		{
			Clang.GetSpellingLocation(from, let cxfile, ?, ?, ?);
			char8* ptr = Clang.GetFileContents(unit, cxfile, let size);
			file = .(ptr, (.)size);
		}

		for (int i = from.int_data; i >= 0; i--)
		{
			char8 c = file[i];
			if (c.IsWhiteSpace)
				str.Append(c);
			else if (c != '/' && c != '*')
				break;
		}
	}

	protected virtual void WriteComments(CXCursor cursor)
	{
		if (cursor.kind == .MacroDefinition)
		{
			str.Append(indent);
			return;
		}
		WriteComments(Clang.GetCursorExtent(cursor));
	}

	protected virtual void WriteComments(CXSourceRange writeUntilRangeStart)
	{
		let curStart = Clang.GetRangeStart(writeUntilRangeStart);
		Clang.GetSpellingLocation(curStart, let curFile, let curLine, let curColumn, let curOffset);
		Clang.GetSpellingLocation(prevEnd, let prevFile, let prevLine, let prevColumn, let prevOffset);

		defer { prevEnd = Clang.GetRangeEnd(writeUntilRangeStart); }
		if (Clang.File_IsEqual(curFile, prevFile) == 0) Runtime.FatalError();

		var between = writeUntilRangeStart;
		between.begin_int_data = prevOffset;
		between.end_int_data = curOffset;

		CXToken* tokenPtr = null; uint32 tokenCount = 0;
		Clang.Tokenize(unit, between, &tokenPtr, &tokenCount);
		defer Clang.DisposeTokens(unit, tokenPtr, tokenCount);
		Span<CXToken> tokens = .(tokenPtr, tokenCount);

		uint32 line = prevLine;
		mixin WriteLinesUntil(uint32 targetLine, CXSourceLocation from)
		{
			int numNewLines = targetLine - line;
			for (let i < numNewLines)
			{
				str.Append('\n');
				if (queuedOpenSquirly)
				{
					str.Append(indent);
					str[^1] = '{';
					queuedOpenSquirly = false;
				}
				line++;
				if (unitMacros.TryGet(.(line, curFile), ?, let macro))
				{
					WriteCursor(macro, macro: true);
					if (i+1 == numNewLines) str.Append(' ');
				}
			}
			if (numNewLines == 0)
				AllWhiteSpaceUntil(from);
			else
				str.Append(indent);
		}

		for (let token in tokens)
		{
			if (Clang.GetTokenKind(token) != .Comment) continue;
			let extent = Clang.GetTokenExtent(unit, token);
			let start = Clang.GetRangeStart(extent);
			let end = Clang.GetRangeStart(extent);
			Clang.GetSpellingLocation(start, ?, let startLine, ?, ?);
			Clang.GetSpellingLocation(end, ?, let endLine, ?, ?);
			WriteLinesUntil!(startLine, start);
			for (let c in ScopeCXString!(Clang.GetTokenSpelling(unit, token)))
			{
				str.Append(c);
				if (c == '\n')
				{
					str.Append(indent);
					line++;
				}
			}
			Flush();
		}
		WriteLinesUntil!(curLine, curStart);
	}

	protected void WriteQueuedOpenSquirly()
	{
		if (queuedOpenSquirly)
		{
			str.Append("{ ");
			queuedOpenSquirly = false;
		}
	}

	protected virtual bool DocString(CXCursor cursor)
	{
		return false;
		/*let doc = ScopeCXString!(Clang.Cursor_GetRawCommentText(cursor));
		if (doc.IsEmpty) return false;
		for (let line in str.Split('\n'))
			str.Append(indent, line, "\n");
		return true;*/
	}

	protected virtual void Type(CXType type, enum { None, InParam, OutParam } paramMode = .None)
	{
		switch (type.kind)
		{
		case .Void: str.Append("void");
		case .Bool: str.Append("bool");
		case .Char_U, .UChar: str.Append("c_uchar");
		case .Char16: str.Append("char16");
		case .Char32: str.Append("char32");
		case .UShort: str.Append("c_ushort");
		case .UInt: str.Append("c_uint");
		case .ULong: str.Append("c_ulong");
		case .ULongLong: str.Append("c_ulonglong");
		case .Char_S, .SChar: str.Append("c_char");
		case .WChar: str.Append("c_wchar");
		case .Short: str.Append("c_short");
		case .Int: str.Append("c_int");
		case .Long: str.Append("c_long");
		case .LongLong: str.Append("c_longlong");
		case .Float: str.Append("float");
		case .Double: str.Append("double");
		case .NullPtr: str.Append("decltype(null)");
		case .Pointer:
			let pointee = Clang.GetPointeeType(type);
			if (paramMode == .OutParam) str.Append("out ");
			Type(pointee);
			if (paramMode != .OutParam && pointee.kind != .FunctionProto && pointee.kind != .FunctionNoProto)
				str.Append('*');
		case .LValueReference,  .RValueReference:
			let nonRefType = Clang.GetNonReferenceType(type);
			switch (paramMode)
			{
			case .None: str.Append("ref ");
			case .InParam:
				switch (type.kind)
				{
				case .LValueReference:
					if (Clang.IsConstQualifiedType(nonRefType) == 0)
						str.Append("ref ");
					else
						fallthrough;
				case .RValueReference:
					str.Append("in ");
				default: Runtime.FatalError();
				}
			case .OutParam: str.Append("out ");
			}
			Type(nonRefType);
		case .FunctionNoProto: str.Append("function "); Type(Clang.GetResultType(type)); str.Append("()");
		case .FunctionProto: str.Append("function "); Type(Clang.GetResultType(type)); WriteFunctionProtoParams(type, ScopeTokenize!(currentCursor, unit));
		case .ConstantArray: Type(Clang.GetArrayElementType(type)); str.Append('['); Clang.GetArraySize(type).ToString(str); str.Append(']');
		case .IncompleteArray: Type(Clang.GetArrayElementType(type)); str.Append('*');
		case .Auto: str.Append("var");
		case .Elaborated, .Record, .Enum, .Typedef:
			CXCursor decl = Clang.GetTypeDeclaration(type);
			if (Clang.Cursor_IsAnonymous(decl) != 0)
			{
				switch (decl.kind)
				{
				case .EnumDecl: Enum(decl);
				case .StructDecl, .ClassDecl, .UnionDecl: Record(decl);
				default: Runtime.FatalError(scope $"Unhandled anon type: {_}");
				}
				break;
			}

			{
				String qualified = scope .(256);
				String buffer = scope .(256);
				GetNameInBindings(decl, qualified);
				CXCursor parent = decl;
				while (true)
				{
					parent = Clang.GetCursorSemanticParent(parent);
					if (Clang.IsDeclaration(parent.kind) == 0 || parent.kind == .LinkageSpec) break;
					buffer.Clear();
					switch (parent.kind)
					{
					case .FunctionTemplate, .ClassTemplate, .TypeAliasTemplateDecl:
						GetNameInBindings(parent, buffer);
						buffer.Append('<');
						Clang.VisitChildren(parent, (cursor, parent, client_data) =>
						{
							String buffer = (.)Internal.UnsafeCastToObject(client_data);
							switch (cursor.kind)
							{
							case .TemplateTypeParameter, .NonTypeTemplateParameter:
							case .TemplateTemplateParameter:
								Runtime.FatalError("C++ template template parameters are not supported");
							default: return .Continue;
							}
							buffer.Append(GetCursorSpelling!(cursor), ", ");
							return .Continue;
						}, Internal.UnsafeCastToPtr(buffer));
						buffer.Length -= 2;
						buffer.Append('>');
					default:
						Flush();
						Type(Clang.GetCursorType(parent));
						buffer.Append(str);
						str.Clear();
					}
					buffer.Append('.');
					qualified.Insert(0, buffer);
				}
				str.Append(qualified);
			}

			let numTemplateArgs = Clang.Type_GetNumTemplateArguments(type);
			if (numTemplateArgs > 0)
			{
				str.Append('<');
				for (let i < numTemplateArgs)
				{
					if (i > 0) str.Append(", ");
					Type(Clang.Type_GetTemplateArgumentAsType(type, (.)i));
				}
				str.Append('>');
			}
		case .Unexposed: str.Append(ScopeCXString!(Clang.GetTypeSpelling(Clang.GetUnqualifiedType(type)))); // template param
		default: Runtime.FatalError(scope $"Unhandled type: {_}");
		}
	}

	protected virtual void WriteFunctionProtoParams(CXType type, Span<CXToken> tokens = null)
	{
		var iter = tokens.GetEnumerator();
		for (let i < 2)
			for (let token in iter)
				if (Clang.GetTokenKind(token) == .Punctuation && GetTokenSpelling!(token) == "(")
					break;

		str.Append('(');
		let numArgs = Clang.GetNumArgTypes(type);
		for (let i < numArgs)
		{
			if (i > 0) str.Append(", ");
			Type(Clang.GetArgType(type, (.)i));
			CXToken last = default;
			for (let token in iter)
			{
				if (Clang.GetTokenKind(token) == .Punctuation && { let spelling = GetTokenSpelling!(token); spelling == "," || spelling == ")" })
				{
					str.Append(' ');
					str.Append(GetTokenSpelling!(last));
					break;
				}
				last = token;
			}
		}
		if (Clang.IsFunctionTypeVariadic(type) != 0)
		{
			if (numArgs > 0) str.Append(", ");
			str.Append("...");
		}
		str.Append(')');
	}

	protected void AccessSpecifier(CXCursor cursor)
	{
		switch (Clang.GetCXXAccessSpecifier(cursor))
		{
		case .InvalidAccessSpecifier,
			 .Public: str.Append("public ");
		case .Protected: str.Append("protected ");
		case .Private: str.Append("private ");
		}
	}

	protected mixin BeginBody(CXCursor cursor)
	{
		indent.Append('\t');
		prevEnd = Clang.GetCursorLocation(cursor);
		queuedOpenSquirly = true;

		if (canChangeBlock)
		{
			defer:mixin { canChangeBlock = true; }
			canChangeBlock = false;
		}

		defer:mixin
		{
			var extent = Clang.GetCursorExtent(cursor);
			extent.begin_int_data = extent.end_int_data;
			WriteComments(extent);
			str.TrimEnd();

			if (defferedWrapperWrite != null)
			{
				str.Append("\n\n", indent, "[Comptime, OnCompile(.TypeInit)] static void __WriteWrapper()\n",
					indent, "{\n",
					indent, "\tif (typeof(T).IsGenericParam) return;\n",
					defferedWrapperWrite,
					indent, "}");
				delete defferedWrapperWrite;
				defferedWrapperWrite = null;
			}

			if (queuedOpenSquirly)
			{
				switch (cursor.kind)
				{
				case .StructDecl, .UnionDecl, .ClassDecl:
					str.Append(';');
				default:
					str.Append(" {}");
				}
				indent.Length--;
				queuedOpenSquirly = false;
			}
			else
			{
				indent.Length--;
				str.Append("\n", indent, "}");
			}
		}
	}

	const int int_maxDigits = scope $"{int.MaxValue:X}".Length + 1;
	protected virtual void CppWrapperName(CXCursor cursor, String outString)
	{
		String hashCode = ScopeCXString!(Clang.GetCursorUSR(cursor))
			.GetHashCode().ToString(..scope .(int_maxDigits), "X", null);
		outString.Append("cpp2beef_");
		if (hashCode.StartsWith('-')) hashCode[0] = 'm';
		outString.Append('0', int_maxDigits - hashCode.Length);
		outString.Append(hashCode);
	}

	protected virtual enum { C, Cpp } Linkable_Attributes(CXCursor cursor)
	{
		let mangledName = ScopeCXString!(Clang.Cursor_GetMangling(cursor));
		let name = GetNameInBindings(cursor, ..scope .(mangledName.Length));
		WriteCustomAttributes(cursor);
		if (mangledName == name)
		{
			str.Append("[CLink] ");
			return .C;
		}
		else
		{
			str.Append("[LinkName(\"");
			CppWrapperName(cursor, str);
			str.Append('"');
			if (!wrapperTemplateChain.IsEmpty)
				if (templateParams.IsEmpty)
					str.Append(" + __template_chain");
				else
					str.Append(" + CppWrapperF($\"", wrapperTemplateChain, "\")");
			str.Append(")] ");
			return .Cpp;
		}
	}

	protected virtual void Method_Parameters(CXCursor cursor)
	{
		let numArgs = Clang.Cursor_GetNumArguments(cursor);
		for (let i < numArgs)
		{
			if (i > 0) str.Append(", ");
			let arg = Clang.Cursor_GetArgument(cursor, (.)i);
			currentCursor = arg;
			bool isOutParam = IsOutParam(arg, cursor);
			Type(Clang.GetCursorType(arg), isOutParam ? .OutParam : .InParam);
			str.Append(' ');
			GetNameInBindings(arg, str);
			str.TrimEnd();
			WriteTokensAfterEquals(arg);
		}
		if (Clang.Cursor_IsVariadic(cursor) != 0)
		{
			if (numArgs > 0) str.Append(", ");
			str.Append("...");
		}
		currentCursor = cursor;
	}

	protected virtual void WriteTypeAndName(CXCursor cursor, CXType type, enum { Standard, TypeAlias, ConversionFunction, Ctor, Dtor } format = .Standard)
	{
		Flush();
		AllWhiteSpaceBetween(
			Clang.GetRangeStart(Clang.GetCursorExtent(cursor)),
			Clang.GetRangeStart(Clang.Cursor_GetSpellingNameRange(cursor, 0, 0))
		);
		if (format == .Ctor)
		{
			str.Append("this");
			return;
		}
		String whitespace = scope .(str);
		str.Clear();
		Type(type);
		whitespace.Length -= Math.Min(str.Length, whitespace.Length);
		if (whitespace.IsEmpty) whitespace.Append(' ');
		switch (format)
		{
		case .Standard:
			str.Append(whitespace);
			GetNameInBindings(cursor, str);
			str.Append(templateParams);
		case .TypeAlias:
			String typeStr = scope .(str);
			str.Set(whitespace);
			GetNameInBindings(cursor, str);
			str.Append(templateParams, " = ", typeStr);
		case .ConversionFunction:
			String typeStr = scope .(str);
			str.Clear();
			str.Append("operator", whitespace, typeStr, templateParams);
		case .Ctor: Runtime.FatalError();
		case .Dtor: str.Append(whitespace, "Dispose");
		}
	}

	protected static mixin ScopeTokenize(CXCursor cursor, CXTranslationUnit unit)
	{
		let range = Clang.GetCursorExtent(cursor);
		CXToken* tokenPtr = null; uint32 tokenCount = 0;
		Clang.Tokenize(unit, range, &tokenPtr, &tokenCount);
		defer:mixin Clang.DisposeTokens(unit, tokenPtr, tokenCount);
		Span<CXToken>(tokenPtr, (.)tokenCount)
	}

	protected virtual void WriteTokensAfterEquals(CXCursor cursor)
	{
		var tokens = ScopeTokenize!(cursor, unit);
		CXSourceLocation prevEnd = Clang.GetNullLocation();
		findEquals: do
		{
			for (let token in tokens)
			{
				let spelling = GetTokenSpelling!(token);
				if (spelling != "=")
				{
					prevEnd = Clang.GetRangeEnd(Clang.GetTokenExtent(unit, token));
					continue;
				}
				tokens.RemoveFromStart(@token.Index);
				break findEquals;
			}
			return;
		}
		WriteTokens(tokens, prevEnd, .Identifier);
	}

	protected virtual void AllWhiteSpaceBetween(CXSourceLocation start, CXSourceLocation end)
	{
		if (Clang.EqualLocations(start, Clang.GetNullLocation()) != 0 ||
			Clang.EqualLocations(end, Clang.GetNullLocation()) != 0) return;

		StringView file;
		{
			Clang.GetSpellingLocation(start, let cxfile, ?, ?, ?);
			char8* ptr = Clang.GetFileContents(unit, cxfile, let size);
			file = .(ptr, (.)size);
		}

		for (int i = start.int_data; i < end.int_data; i++)
		{
			char8 c = file[i];
			//if (c.IsWhiteSpace) str.Append(c);
			str.Append(c.IsWhiteSpace ? c : ' ');
		}
	}

	protected virtual void WriteToken(CXToken token)
	{
		let spelling = ScopeCXString!(Clang.GetTokenSpelling(unit, token));
		let kind = Clang.GetTokenKind(token);
		token: switch (kind)
		{
		case .Literal:
			str.Append(spelling);
			if (str.EndsWith("LL"))
			str.Length--;
		case .Identifier:
			var cursor = Clang.GetCursor(unit, Clang.GetTokenLocation(unit, token));
			if (Clang.Cursor_IsNull(cursor) != 0) fallthrough;
			cursor = Clang.GetCursorDefinition(cursor);
			if (Clang.Cursor_IsNull(cursor) != 0) fallthrough;
			GetNameInBindings(cursor, str);
		case .Punctuation, .Comment, .Keyword:
			str.Append(spelling);
		}
	}

	protected virtual void WriteTokens(Span<CXToken> tokens, CXSourceLocation prevTokenEnd, CXTokenKind prevKind)
	{
		var prevTokenEnd;
		for (let token in tokens)
		{
			let extent = Clang.GetTokenExtent(unit, token);
			AllWhiteSpaceBetween(prevTokenEnd, Clang.GetRangeStart(extent));
			WriteToken(token);
			prevTokenEnd = Clang.GetRangeEnd(extent);
		}

		//TODO: move to custom utils
		/* // This formats the tokens instead of copying the format 1:1
		const CXTokenKind Unary = (.)-1;
		var prevKind;
		for (let token in tokens)
		{
			let spelling = ScopeCXString!(Clang.GetTokenSpelling(unit, token));
			let kind = Clang.GetTokenKind(token);
			token: switch (kind)
			{
			case .Keyword, .Identifier, .Literal:
				if (prevKind == .Keyword || prevKind == .Identifier || prevKind == .Punctuation)
					str.Append(' ');
				if (kind == .Identifier) do
				{
					var cursor = Clang.GetCursor(unit, Clang.GetTokenLocation(unit, token));
					if (Clang.Cursor_IsNull(cursor) != 0) break;
					cursor = Clang.GetCursorDefinition(cursor);
					if (Clang.Cursor_IsNull(cursor) != 0) break;
					GetNameInBindings(cursor, str);
					break token;
				}	
				str.Append(spelling);
				if (kind == .Literal && str.EndsWith("LL"))
					str.Length--;
			case .Punctuation:
				defer str.Append(spelling);
				if (prevKind != Unary && !(spelling == "(" && prevKind != .Punctuation) && spelling != ")")
				{
					if (spelling != ",")
						str.Append(' ');
					if (prevKind != .Punctuation)
						break;
				}
				prevKind = Unary;
				continue;
			case .Comment:
			}
			prevKind = kind;
		}*/
	}

	protected virtual void Namespace(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		str.Append("extension ");
		GetNameInBindings(cursor, str);
		{
			BeginBody!(cursor);
			Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
			{
				Self self = (.)Internal.UnsafeCastToObject(client_data);
				self.WriteCursor(cursor);
				return .Continue;
			}, Internal.UnsafeCastToPtr(this));
		}
		if (Clang.Cursor_IsInlineNamespace(cursor) != 0)
		{
			str.Append(indent, "public static using ");
			GetNameInBindings(cursor, str);
			str.Append(";\n");
		}
	}

	protected virtual void WriteMethodWrapper(CXCursor cursor)
	{
		let parent = Clang.GetCursorType(Clang.GetCursorSemanticParent(cursor));
		var resultType = Clang.GetCursorResultType(cursor);
		bool nonStatic = (cursor.kind == .CXXMethod && Clang.CXXMethod_IsStatic(cursor) == 0) || cursor.kind == .Destructor || cursor.kind == .ConversionFunction;

		if (!wrapperTemplateChain.IsEmpty)
			FlushWrapper();
		StringView parentSpelling = "\" + __cpp_type + \"";
		if (wrapperTemplateChain.IsEmpty)
			parentSpelling = ScopeCXString!::(Clang.GetFullyQualifiedName(parent, printingPolicy, 0));

		if (cursor.kind == .Constructor)
			wrapperBuf.Append("__type<", parentSpelling, ">");
		else
		{
			wrapperBuf.Append("__type<");
			wrapperBuf.Append(ScopeCXString!(Clang.GetFullyQualifiedName(resultType, printingPolicy, 0)));
			wrapperBuf.Append("> ");
		}
		CppWrapperName(cursor, wrapperBuf);
		if (!wrapperTemplateChain.IsEmpty)
		{
			if (templateParams.IsEmpty)
				wrapperBuf.Append("\" + __template_chain + \"");
			else
				wrapperBuf.Append("\" + CppWrapperF($\"", wrapperTemplateChain, "\") + \"");
		}
		wrapperBuf.Append('(');
		if (nonStatic)
			wrapperBuf.Append(parentSpelling, " *self");
		for (int i < Clang.Cursor_GetNumArguments(cursor))
		{
			let arg = Clang.Cursor_GetArgument(cursor, (.)i);
			if (nonStatic || i > 0) wrapperBuf.Append(", ");
			wrapperBuf.Append("__type<");
			wrapperBuf.Append(ScopeCXString!(Clang.GetFullyQualifiedName(Clang.GetCursorType(arg), printingPolicy, 0)));
			wrapperBuf.Append("> p");
			i.ToString(wrapperBuf);
		}
		wrapperBuf.Append(") { ");
		if (resultType.kind != .Void) wrapperBuf.Append("return ");
		if (nonStatic)
		{
			if (cursor.kind != .Destructor)
				wrapperBuf.Append("self->", GetCursorSpelling!(cursor));
			else
				wrapperBuf.Append("self->~", parentSpelling);
		}
		else if (cursor.kind == .Constructor)
			wrapperBuf.Append(parentSpelling);
		else
			wrapperBuf.Append(fullCursorName);
		wrapperBuf.Append('(');
		for (int i < Clang.Cursor_GetNumArguments(cursor))
		{
			if (i > 0) wrapperBuf.Append(", ");
			wrapperBuf.Append('p');
			i.ToString(wrapperBuf);
		}
		wrapperBuf.Append("); }");

		if (wrapperTemplateChain.IsEmpty)
		{
			wrapperBuf.Append('\n');
			FlushWrapper();
		}
		else
		{
			if (defferedWrapperWrite == null)
				defferedWrapperWrite = new .(1024);
			defferedWrapperWrite.Append(indent, "\tCppWrapperWriter<\"", WrapperFilePath, "\">.Write(\"extern \\\"C\\\" ");
			wrapperBuf.ToString(defferedWrapperWrite);
			defferedWrapperWrite.Append("\\n\");\n");
			wrapperBuf.Clear();
		}
	}

	protected virtual void FunctionDecl(CXCursor cursor)
	{
		if (Linkable_Attributes(cursor) == .Cpp)
			WriteMethodWrapper(cursor);

		AccessSpecifier(cursor);
		str.Append("static extern ");
		WriteTypeAndName(cursor, Clang.GetCursorResultType(cursor));
		str.Append('(');
		Method_Parameters(cursor);
		str.Append(')');
		str.Append(templateParamsWhere);
		str.Append(';');
	}

	protected virtual void CXXMethod(CXCursor cursor) //TODO: conversion function
	{
		void Attributes()
		{
			if (Linkable_Attributes(cursor) == .Cpp)
				WriteMethodWrapper(cursor);
		}

		let spelling = GetCursorSpelling!(cursor);
		if (spelling.StartsWith("operator"))
		{
			if (spelling == "operator[]")
			{
				Runtime.Assert(templateParams.IsEmpty, "Properties can't have generics");
				AccessSpecifier(cursor);
				if (Clang.CXXMethod_IsStatic(cursor) != 0) str.Append("static ");
				str.Append("extern ");
				Type(Clang.GetCursorResultType(cursor));
				str.Append(" this[");
				Method_Parameters(cursor);
				str.Append("] { ");
				Attributes();
				str.Append("get; }");
				return;
			}

			Attributes();
			AccessSpecifier(cursor);
			str.Append("static extern ");
			WriteTypeAndName(cursor, Clang.GetCursorResultType(cursor),
				cursor.kind == .ConversionFunction ? .ConversionFunction : .Standard);
			str.Append("(in Self, ");
			Method_Parameters(cursor);
			if (str.EndsWith(", ")) str.Length -= 2;
			str.Append(')');
			str.Append(templateParamsWhere);
			str.Append(';');
			return;
		}

		Attributes();
		AccessSpecifier(cursor);
		if (Clang.CXXMethod_IsStatic(cursor) != 0) str.Append("static ");
		str.Append("extern ");
		switch (cursor.kind)
		{
		case .Constructor: WriteTypeAndName(cursor, default, .Ctor);
		case .Destructor: WriteTypeAndName(cursor, Clang.GetCursorResultType(cursor), .Dtor);
		case .CXXMethod:
			WriteTypeAndName(cursor, Clang.GetCursorResultType(cursor));
		default:
			Runtime.FatalError("Unhandled c++ method kind");
		}
		str.Append('(');
		Method_Parameters(cursor);
		str.Append(')');
		if (Clang.CXXMethod_IsStatic(cursor) == 0 && Clang.CXXMethod_IsConst(cursor) == 0 && cursor.kind == .CXXMethod)
			str.Append(" mut");
		str.Append(templateParamsWhere);
		str.Append(';');
	}

	protected virtual void FieldDecl(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		AccessSpecifier(cursor);
		WriteTypeAndName(cursor, Clang.GetCursorType(cursor));
		str.Append(';');
	}

	protected virtual void VarDecl(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		let type = Clang.GetCursorType(cursor);
		switch (Clang.GetCursorLinkage(cursor))
		{
		case .Internal when Clang.IsConstQualifiedType(type) != 0:
			AccessSpecifier(cursor);
			str.Append("const ");
			WriteTypeAndName(cursor, type);
			WriteTokensAfterEquals(cursor);
			str.Append(';');
		case .External, .UniqueExternal:
			Flush();
			let linkLang = Linkable_Attributes(cursor);
			if (linkLang == .Cpp) str.Clear();
			AccessSpecifier(cursor);
			str.Append("static extern ");
			if (linkLang == .Cpp && type.kind != .LValueReference && type.kind != .RValueReference)
				str.Append("ref ");
			WriteTypeAndName(cursor, type);
			if (linkLang == .Cpp)
			{
				let wrapperName = CppWrapperName(cursor, ..scope .());
				str.Append(" { [LinkName(\"", wrapperName, "\")] get; }");
				wrapperBuf.Append("__type<");
				wrapperBuf.Append(ScopeCXString!(Clang.GetFullyQualifiedName(type, printingPolicy, 0)));
				wrapperBuf.Append("> ");
				if (type.kind != .LValueReference && type.kind != .RValueReference)
					wrapperBuf.Append('&');
				wrapperBuf.Append(wrapperName, "() { return ", fullCursorName, "; }\n");
				FlushWrapper();
			}
			str.Append(';');
		default:
			Runtime.FatalError(scope $"Unhandled var linkage: {_}");
		}
	}

	public static bool HasVTable(CXCursor cursor)
	{
		return Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			switch (cursor.kind)
			{
			case .CXXBaseSpecifier:
				if (HasVTable(Clang.GetCursorDefinition(cursor)))
					return .Break;
			case .CXXMethod, .Constructor, .Destructor, .ConversionFunction:
				if (Clang.CXXMethod_IsVirtual(cursor) != 0)
					return .Break;
			default:
			}
			return .Continue;
		}, null) != 0;
	}

	protected virtual void Record(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		switch (cursor.kind)
		{
		case .StructDecl, .ClassDecl: str.Append("[CRepr] ");
		case .UnionDecl: str.Append("[CRepr, Union] ");
		default: Runtime.FatalError("Unhandled record type");
		}
		AccessSpecifier(cursor);
		str.Append("struct ");
		if (Clang.Cursor_IsAnonymous(cursor) == 0)
			GetNameInBindings(cursor, str);
		str.Append(templateParams);
		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			if (cursor.kind != .Destructor) return .Continue;
			String str = (.)Internal.UnsafeCastToObject(client_data);
			str.Append(" : IDisposable");
			return .Break;
		}, Internal.UnsafeCastToPtr(str));

		str.Append(templateParamsWhere);
		BeginBody!(cursor);
		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);
			switch (cursor.kind)
			{
			case .FieldDecl, .CXXMethod, .Constructor, .Destructor, .ConversionFunction, .FunctionDecl, .VarDecl,
				 .FunctionTemplate, .ClassTemplate, .TypeAliasTemplateDecl, .StructDecl, .ClassDecl,
				 .UnionDecl, .EnumDecl, .TypeAliasDecl, .TypedefDecl:
				self.WriteComments(cursor);
				return .Break;
			default:
				return .Continue;
			}
		}, Internal.UnsafeCastToPtr(this));

		if (!wrapperTemplateChain.IsEmpty)
		{
			WriteQueuedOpenSquirly();
			str.Append("private const String __cpp_type = \"", GetCursorSpelling!(cursor), "<\"");
			Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
			{
				Self self = (.)Internal.UnsafeCastToObject(client_data);
				switch (cursor.kind)
				{
				case .TemplateTypeParameter:
					self.str.Append(" + CppTypeToC(typeof(", GetCursorSpelling!(cursor), "))");
				case .NonTypeTemplateParameter:
					self.str.Append(" + CppConstValue<");
					self.Type(Clang.GetCursorType(cursor));
					self.str.Append(">(", GetCursorSpelling!(cursor), ")");
				default: return .Continue;
				}
				self.str.Append(" + \", \"");
				return .Continue;
			}, Internal.UnsafeCastToPtr(this));
			str.Length -= 3;
			str.Append(">\";\n", indent);
			str.Append("private const String __template_chain = CppWrapperF($\"", wrapperTemplateChain, "\");\n\n", indent);
			/*str.Append("[CppWriteToWrapper<\"", WrapperFilePath, "\">(\"\\ntemplate ");
			switch (cursor.kind)
			{
			case .StructDecl: str.Append("struct ");
			case . ClassDecl: str.Append("class " );
			case . UnionDecl: str.Append("union " );
			default: Runtime.FatalError();
			}
			str.Append("\" + __cpp_type + \";\\n\")]\n\n", indent);*/
		}

		bool firstBase = true;
		bool hasVTable = HasVTable(cursor);
		Clang.VisitCXXBaseClasses(Clang.GetCursorType(cursor), (cursor, client_data) =>
		{
			(Self self, bool* firstBase, bool hasVTable) = *(.)client_data;

			Runtime.Assert(Clang.IsVirtualBase(cursor) == 0, "Virtual bases are not supported");
			self.WriteQueuedOpenSquirly();
			if (*firstBase && hasVTable && !HasVTable(Clang.GetCursorDefinition(cursor)))
				self.str.Append(self.indent, "private void* cppVtable;\n");
			self.str.Append(self.indent);
			self.AccessSpecifier(cursor);
			self.str.Append("using ");
			self.Type(Clang.GetCursorType(cursor));
			self.str.Append(";\n");

			*firstBase = false;
			return .Continue;
		}, &(this, &firstBase, hasVTable));

		if (firstBase && hasVTable)
			str.Append(indent, "private void* cppVtable;\n");

		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);
			self.WriteCursor(cursor);
			if (Clang.Cursor_IsAnonymousRecordDecl(cursor) != 0)
				self.str..TrimEnd()..Append(";");
			return .Continue;
		}, Internal.UnsafeCastToPtr(this));
	}

	protected virtual void Enum(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		AccessSpecifier(cursor);
		str.Append("enum ");
		if (Clang.Cursor_IsAnonymous(cursor) == 0)
			GetNameInBindings(cursor, str);
		str.Append(templateParams);
		if (Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
			{
				if (cursor.kind == .PackedAttr)
					return .Break;
			   	return .Continue;
			}, null) == 0)
		{
			str.Append(" : ");
			Type(Clang.GetEnumDeclIntegerType(cursor));
			str.Append(templateParamsWhere);
		}	
		BeginBody!(cursor);
		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);
			Runtime.Assert(cursor.kind == .EnumConstantDecl);

			self.BeginCursor(cursor);
			self.WriteCustomAttributes(cursor);
			self.GetNameInBindings(cursor, self.str);
			self.WriteTokensAfterEquals(cursor);
			//Clang.GetEnumConstantDeclValue(cursor).ToString(self.str);
			self.str.Append(',');

			return .Continue;
		}, Internal.UnsafeCastToPtr(this));
	}

	protected virtual void TypeAlias(CXCursor cursor)
	{
		CXType type;
		switch (cursor.kind)
		{
		case .TypedefDecl: type = Clang.GetTypedefDeclUnderlyingType(cursor);
		case .TypeAliasDecl: type = Clang.GetCursorType(cursor);
		default:
			Runtime.FatalError("Unhandled type alias cursor kind");
		}

		if (type.kind == .Pointer) do
		{
			let pointee = Clang.GetPointeeType(type);
			if (pointee.kind != .FunctionProto && pointee.kind != .FunctionNoProto)
				break;
			WriteCustomAttributes(cursor);
			AccessSpecifier(cursor);
			str.Append("function ");
			WriteTypeAndName(cursor, Clang.GetResultType(pointee));
			WriteFunctionProtoParams(pointee, ScopeTokenize!(cursor, unit));
			str.Append(templateParamsWhere);
			str.Append(';');
			return;
		}

		WriteCustomAttributes(cursor);
		AccessSpecifier(cursor);
		str.Append("typealias");
		WriteTypeAndName(cursor, type, .TypeAlias);
		Runtime.Assert(templateParamsWhere.IsEmpty, "type aliases can't have constraints (yet)");
		str.Append(';');
	}

	protected virtual void MacroDefinition(CXCursor cursor)
	{
		let tokens = ScopeTokenize!(cursor, unit);
		WriteCustomAttributes(cursor);
		str.Append("public const let ");
		GetNameInBindings(cursor, str);
		AllWhiteSpaceBetween(Clang.GetRangeEnd(Clang.GetTokenExtent(unit, tokens[0])), Clang.GetRangeStart(Clang.GetTokenExtent(unit, tokens[1])));
		str.Append("= ");
		WriteTokens(tokens[1...], Clang.GetNullLocation(), .Punctuation);
		str.Append(';');
	}
}
