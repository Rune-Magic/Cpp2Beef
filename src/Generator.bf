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
		=> Compiler.Identifier.GetSourceName(GetCursorSpelling!(cursor), outString);

	enum AttrFlags { None = 0, Packed = 1, NoDiscard = 2, AllowDuplicates }
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

	protected virtual HandleCursorResult<Self> HandleCursor(CXCursor cursor)
	{
		/*if (Clang.EqualCursors(Clang.GetCanonicalCursor(cursor), cursor) != 0)
			return .SkipCursor;*/
		if (cursor.kind == .MacroDefinition)
		{
			if (Clang.Cursor_IsMacroFunctionLike(cursor) != 0) return .SkipCursor;
			let tokens = ScopeTokenize!(cursor);
			if (tokens.Length <= 1) return .SkipCursor;
		}
		return .Include;
	}

	public enum HandleCursorResult<T>
	{
		case SkipCursor, Include;
		case Custom(bool staticBlock, bool singleLine, function void(T this, CXCursor cursor) write);
	}

	private CXIndex index = Clang.CreateIndex(excludeDeclarationsFromPCH: 0, displayDiagnostics: 1) ~ Clang.DisposeIndex(_);
	private CXTranslationUnit unit;

	protected append String str = .(1024);

	private bool newLineAfterCurrent = false;
	private append String block = .(64);
	protected append String indent = .(8);
	private append String virtualWrapper = .(128);
	private CXSourceLocation prevEnd = Clang.GetNullLocation();
	private bool queuedOpenSquirly = false;

	private struct UnitMacroIndex : this(uint32 line, CXFile file), IHashable
	{
		public int GetHashCode() => line;
		public static bool operator==(Self lhs, Self rhs) => lhs.line == rhs.line && Clang.File_IsEqual(lhs.file, rhs.file) != 0;
	}
	private append Dictionary<UnitMacroIndex, CXCursor> unitMacros = .(128);
	private append HashSet<CXFile> fileHandles = .(16);

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

	public enum GenerationError
	{
		ParsingFailed
	}

	public Result<void, GenerationError> Generate(char8* headerPath)
	{
		virtualWrapper.Set("bf_wrapper");
		unitMacros.Clear();
		fileHandles.Clear();
		prevEnd = Clang.GetNullLocation();

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

		Clang.VisitChildren(Clang.GetTranslationUnitCursor(unit), (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);

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

				if (self.fileHandles.Add(file))
				{
					self.prevEnd = location;
					self.prevEnd.int_data = 0;
				}
			}

			self.WriteCursor(cursor, canChangeBlock: true);

			return .Continue;
		}, Internal.UnsafeCastToPtr(this));

		return .Ok;
	}

	void WriteCursor(CXCursor cursor, bool canChangeBlock, bool macro = false)
	{
		if ((macro) != (cursor.kind == .MacroDefinition)) return;

		void StaticBlock()
		{
			if (!canChangeBlock || block == "static") return;
			block.Set("static");
			str.Append("\n", indent, "static\n");
			str.Append(indent, "{\n");
			if (IndentBlocks) indent.Append('\t');
			newLineAfterCurrent = false;
		}
		void NoBlock()
		{
			if (!canChangeBlock || block != "static") return;
			block.Set("");
			if (IndentBlocks) indent.Length--;
			str.Append(indent, "\n}");
			newLineAfterCurrent = true;
		}

		int spellingCount = 2;
		{
			let spelling = GetCursorSpelling!(cursor);
			spellingCount += spelling.Length;
			virtualWrapper.Append("__");
			virtualWrapper.Append(spelling);
		}
		defer { virtualWrapper.Length -= spellingCount; }

		switch (HandleCursor(cursor)) //TODO: don't make this an after-thought
		{
		case .SkipCursor: return;
		case .Custom(let staticBlock, let singleLine, let write):
			if (staticBlock) StaticBlock(); else NoBlock();
			if (singleLine) SingleLine(cursor); else MultiLine(cursor);
			if (!macro) WriteComments(cursor);
			write(this, cursor);
			return;
		case .Include:
			if (macro)
			{
				StaticBlock();
				SingleLine(cursor);
				str.Append(indent);
				MacroDefinition(cursor);
				return;
			}
		}

		switch (cursor.kind)
		{
		case .StructDecl,
			 .ClassDecl,
			 .UnionDecl: NoBlock(); MultiLine(cursor); WriteComments(cursor); Record(cursor);
		case .EnumDecl: NoBlock(); MultiLine(cursor); WriteComments(cursor); Enum(cursor);
		case .TypeAliasDecl, .TypedefDecl: NoBlock(); SingleLine(cursor); WriteComments(cursor); TypeAlias(cursor);
		case .Namespace: NoBlock(); MultiLine(cursor); WriteComments(cursor); Namespace(cursor);

		case .FunctionDecl: StaticBlock(); SingleLine(cursor); WriteComments(cursor); FunctionDecl(cursor);
		case .CXXMethod,
			 .Constructor,
			 .Destructor,
			 .ConversionFunction: StaticBlock(); SingleLine(cursor); WriteComments(cursor); CXXMethod(cursor);
		case .FieldDecl: StaticBlock(); SingleLine(cursor); WriteComments(cursor); FieldDecl(cursor);
		case .VarDecl: StaticBlock(); SingleLine(cursor); WriteComments(cursor); VarDecl(cursor);

		case .FunctionTemplate: StaticBlock(); SingleLine(cursor); WriteComments(cursor);
		case .ClassTemplate: NoBlock(); MultiLine(cursor); WriteComments(cursor);
		case .TypeAliasTemplateDecl: NoBlock(); SingleLine(cursor); WriteComments(cursor);

		case .InclusionDirective, .MacroExpansion, .CXXBaseSpecifier, .CXXAccessSpecifier: // ignored
		default: Debug.WriteLine(scope $"Unhandled cursor: {_}");
		}

		Flush();
	}

	StreamWriter currentWritter;
	protected void Flush()
	{
		currentWritter.Write(str);
		str.Clear();
	}

	// Use these methods if you don't want to rely on `WriteComments`
	protected virtual void SingleLine(CXCursor cursor)
	{
		/*if (newLineAfterCurrent)
			str.Append('\n');
		newLineAfterCurrent = DocString(cursor);
		str.Append(indent);*/
	}
	protected virtual void MultiLine(CXCursor cursor)
	{
		/*str.Append('\n');
		newLineAfterCurrent = true;
		DocString(cursor);
		str.Append(indent);*/
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
		if (cursor.kind == .MacroDefinition) return;
		WriteComments(Clang.GetCursorExtent(cursor));
	}

	protected virtual void WriteComments(CXSourceRange writeUntilRangeStart)
	{
		let curStart = Clang.GetRangeStart(writeUntilRangeStart);
		Clang.GetSpellingLocation(curStart, let curFile, let curLine, let curColumn, let curOffset);
		Clang.GetSpellingLocation(prevEnd, let prevFile, let prevLine, let prevColumn, let prevOffset);

		defer { prevEnd = Clang.GetRangeEnd(writeUntilRangeStart); }
		if (Clang.File_IsEqual(curFile, prevFile) == 0) return;

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
					WriteCursor(macro, canChangeBlock: true, macro: true);
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

	protected virtual bool DocString(CXCursor cursor)
	{
		let doc = ScopeCXString!(Clang.Cursor_GetRawCommentText(cursor));
		if (doc.IsEmpty) return false;
		for (let line in str.Split('\n'))
			str.Append(indent, line, "\n");
		return true;
	}

	protected virtual void Type(CXType type)
	{
		void PossiblyAnon(function void(Self this, CXCursor) writeDecl)
		{
			CXCursor decl = Clang.GetTypeDeclaration(type);
			if (Clang.Cursor_IsAnonymous(decl) != 0)
			{
				writeDecl(this, decl);
				str.TrimEnd();
			}
			else
			{
				//Clang.GetTypePrettyPrinted();
				GetNameInBindings(decl, str);
			}
		}

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
			Type(pointee);
			if (pointee.kind != .FunctionProto && pointee.kind != .FunctionNoProto)
				str.Append('*');
		case .LValueReference: str.Append( "in "); Type(Clang.GetNonReferenceType(type));
		case .RValueReference: str.Append("ref "); Type(Clang.GetNonReferenceType(type));
		case .Record: PossiblyAnon(=> Record);
		case .Enum: PossiblyAnon(=> Enum);
		case .Typedef:
			CXCursor decl = Clang.GetTypeDeclaration(type);
			let spelling = GetCursorSpelling!(decl);
			switch (spelling)
			{
			case "size_t": str.Append("c_size");
			case "wchar_t": str.Append("c_wchar");
			case "int8_t": str.Append("int8");
			case "int16_t": str.Append("int16");
			case "int32_t": str.Append("int32");
			case "int64_t": str.Append("int64");
			case "uint8_t": str.Append("uint8");
			case "uint16_t": str.Append("uint16");
			case "uint32_t": str.Append("uint32");
			case "uint64_t": str.Append("uint64");
			default: GetNameInBindings(decl, str);
			}
		case .FunctionNoProto: str.Append("function "); Type(Clang.GetResultType(type)); str.Append("()");
		case .FunctionProto: str.Append("function "); Type(Clang.GetResultType(type)); WriteFunctionProtoParams(type);
		case .ConstantArray: Type(Clang.GetArrayElementType(type)); str.Append('['); Clang.GetArraySize(type).ToString(str); str.Append(']');
		case .IncompleteArray: Type(Clang.GetArrayElementType(type)); str.Append('*');
		case .Auto: str.Append("var");
		case .Elaborated:
			let named = Clang.Type_GetNamedType(type);
			if (named.kind != .Unexposed) { Type(named); break; }
			let unqualified = Clang.GetUnqualifiedType(type);
			str.Append(ScopeCXString!(Clang.GetTypeSpelling(unqualified)));
		default: Runtime.FatalError(scope $"Unhandled type kind: {_}");
		}
	}

	protected virtual void WriteFunctionProtoParams(CXType type)
	{
		str.Append('(');
		let numArgs = Clang.GetNumArgTypes(type);
		for (uint32 i < (.)numArgs)
		{
			if (i > 0) str.Append(", ");
			Type(Clang.GetArgType(type, i));
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
		{
			indent.Append('\t');
			newLineAfterCurrent = false;
			prevEnd = Clang.GetCursorLocation(cursor);
			queuedOpenSquirly = true;
		}

		defer:mixin
		{
			var extent = Clang.GetCursorExtent(cursor);
			extent.begin_int_data = extent.end_int_data;
			WriteComments(extent);
			str.TrimEnd();

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
				newLineAfterCurrent = false;
				queuedOpenSquirly = false;
			}
			else
			{
				indent.Length--;
				str.Append(indent, "\n}");
				newLineAfterCurrent = true;
			}
		}
	}

	protected virtual void Linkable_Attributes(CXCursor cursor)
	{
		let mangledName = ScopeCXString!(Clang.Cursor_GetMangling(cursor));
		let name = GetNameInBindings(cursor, ..scope .(mangledName.Length));
		WriteCustomAttributes(cursor);
		if (mangledName == name)
			str.Append("[CLink] ");
		//!!!!!! disabled for asthetics
		//else
		//	str.Append("[LinkName(\"", mangledName, "\")] ");
	}

	protected virtual void Method_Parameters(CXCursor cursor)
	{
		let numArgs = Clang.Cursor_GetNumArguments(cursor);
		for (uint32 i < (.)numArgs)
		{
			if (i > 0) str.Append(", ");
			let arg = Clang.Cursor_GetArgument(cursor, i);
			Type(Clang.GetCursorType(arg));
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
	}

	protected virtual void WriteTypeAndName(CXCursor cursor, CXType type, enum { Standard, TypeAlias, ConversionFunction, Ctor, Dtor } format = .Standard)
	{
		Flush();
		AllWhiteSpaceBetween(
			Clang.GetRangeStart(Clang.GetCursorExtent(cursor)),
			Clang.GetRangeStart(Clang.Cursor_GetSpellingNameRange(cursor, 0, 0))
		);
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
		case .TypeAlias:
			String typeStr = scope .(str);
			str.Clear();
			GetNameInBindings(cursor, str);
			str.Append(whitespace, "= ", typeStr);
		case .ConversionFunction:
			String typeStr = scope .(str);
			str.Clear();
			str.Append("operator", whitespace, typeStr);
		case .Ctor, .Dtor:
			let originalLen = GetCursorSpelling!(cursor).Length;
			StringView beefName = _ == .Ctor ? "this" : "~this";
			int change = beefName.Length - originalLen;
			if (change > 0)
				whitespace.Length -= change;
			else
				str.Append(' ', -change);
			str.Append(beefName);
		}
	}

	protected mixin ScopeTokenize(CXCursor cursor)
	{
		let range = Clang.GetCursorExtent(cursor);
		CXToken* tokenPtr = null; uint32 tokenCount = 0;
		Clang.Tokenize(unit, range, &tokenPtr, &tokenCount);
		defer:mixin Clang.DisposeTokens(unit, tokenPtr, tokenCount);
		Span<CXToken>(tokenPtr, (.)tokenCount)
	}

	protected virtual void WriteTokensAfterEquals(CXCursor cursor)
	{
		var tokens = ScopeTokenize!(cursor);
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

	protected void AllWhiteSpaceBetween(CXSourceLocation start, CXSourceLocation end)
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
		str.Append("static class ");
		GetNameInBindings(cursor, str);
		{
			BeginBody!(cursor);
			Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
			{
				Self self = (.)Internal.UnsafeCastToObject(client_data);
				self.WriteCursor(cursor, canChangeBlock: false);
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

	protected virtual void FunctionDecl(CXCursor cursor)
	{
		Linkable_Attributes(cursor);
		AccessSpecifier(cursor);
		str.Append("static extern ");
		WriteTypeAndName(cursor, Clang.GetCursorResultType(cursor));
		str.Append('(');
		Method_Parameters(cursor);
		str.Append(");");
	}

	protected virtual void CXXMethod(CXCursor cursor) //TODO: conversion function
	{
		void Attributes()
		{
			if (Clang.CXXMethod_IsVirtual(cursor) != 0)
			{
				WriteCustomAttributes(cursor);
				str.Append("[CppVirtual, LinkName(\"", virtualWrapper, "\")] ");
			}
			else
			{
				Linkable_Attributes(cursor);
			}
		}

		let spelling = GetCursorSpelling!(cursor);
		if (spelling.StartsWith("operator"))
		{
			if (spelling == "operator[]")
			{
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
			str.Append(");");
			return;
		}

		Attributes();
		AccessSpecifier(cursor);
		if (Clang.CXXMethod_IsStatic(cursor) != 0) str.Append("static ");
		str.Append("extern ");
		switch (cursor.kind)
		{
		case .Constructor: WriteTypeAndName(cursor, .() { kind = .Void }, .Ctor);
		case .Destructor: WriteTypeAndName(cursor, .() { kind = .Void }, .Dtor);
		case .CXXMethod:
			WriteTypeAndName(cursor, Clang.GetCursorResultType(cursor));
		default:
			Runtime.FatalError("Unhandled c++ method kind");
		}
		str.Append('(');
		Method_Parameters(cursor);
		str.Append(");");
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
			WriteTypeAndName(cursor, Clang.GetCursorType(cursor));
			WriteTokensAfterEquals(cursor);
			str.Append(";");
		case .External, .UniqueExternal:
			Linkable_Attributes(cursor);
			AccessSpecifier(cursor);
			str.Append("static extern ");
			WriteTypeAndName(cursor, Clang.GetCursorType(cursor));
			str.Append(";");
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
		str.Append("struct ");
		if (Clang.Cursor_IsAnonymous(cursor) == 0)
			GetNameInBindings(cursor, str);

		BeginBody!(cursor);

		bool firstBase = true;
		bool hasVTable = HasVTable(cursor);
		Clang.VisitCXXBaseClasses(Clang.GetCursorType(cursor), (cursor, client_data) =>
		{
			(Self self, bool* firstBase, bool hasVTable) = *(.)client_data;

			Runtime.Assert(Clang.IsVirtualBase(cursor) == 0, "Virtual bases are not supported");
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
			self.WriteCursor(cursor, canChangeBlock: false);
			if (Clang.Cursor_IsAnonymousRecordDecl(cursor) != 0)
				self.str..TrimEnd()..Append(";");
			return .Continue;
		}, Internal.UnsafeCastToPtr(this));
	}

	protected virtual void Enum(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		str.Append("enum ");
		if (Clang.Cursor_IsAnonymous(cursor) == 0)
			GetNameInBindings(cursor, str);
		if (Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
			{
				if (cursor.kind == .PackedAttr)
					return .Break;
			   	return .Continue;
			}, null) == 0)
		{
			str.Append(" : ");
			Type(Clang.GetEnumDeclIntegerType(cursor));
		}	
		BeginBody!(cursor);
		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);
			Runtime.Assert(cursor.kind == .EnumConstantDecl);

			self.SingleLine(cursor);
			self.WriteComments(cursor);
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
			str.Append("function ");
			WriteTypeAndName(cursor, Clang.GetResultType(pointee));
			WriteFunctionProtoParams(pointee);
			str.Append(';');
			return;
		}

		WriteCustomAttributes(cursor);
		str.Append("typealias ");
		WriteTypeAndName(cursor, type, .TypeAlias);
		str.Append(';');
	}

	protected virtual void MacroDefinition(CXCursor cursor)
	{
		let tokens = ScopeTokenize!(cursor);
		WriteCustomAttributes(cursor);
		str.Append("public const let ");
		GetNameInBindings(cursor, str);
		AllWhiteSpaceBetween(Clang.GetRangeEnd(Clang.GetTokenExtent(unit, tokens[0])), Clang.GetRangeStart(Clang.GetTokenExtent(unit, tokens[1])));
		str.Append("= ");
		WriteTokens(tokens[1...], Clang.GetNullLocation(), .Punctuation);
		str.Append(';');
	}
}
