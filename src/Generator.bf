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
		if (Clang.GetCanonicalCursor(cursor) != cursor) return .SkipCursor;
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
	private CXSourceLocation prevEnd; //TODO

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

		virtualWrapper.Set("bf_wrapper");

		Clang.VisitChildren(Clang.GetTranslationUnitCursor(unit), (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);

			{
				let location = Clang.GetCursorLocation(cursor);
				Clang.GetSpellingLocation(location, let file, ?, ?, ?);
				let header = ScopeCXString!(Clang.GetFileName(file));
				self.currentWritter = self.GetWriterForHeader(header);
				if (self.currentWritter == null) return .Continue;
			}

			self.WriteCursor(cursor, canChangeBlock: true);

			return .Continue;
		}, Internal.UnsafeCastToPtr(this));

		return .Ok;
	}

	void WriteCursor(CXCursor cursor, bool canChangeBlock)
	{
		void StaticBlock()
		{
			if (!canChangeBlock || block == "static") return;
			block.Set("static");
			str.Append("\n", indent, "static\n");
			str.Append(indent, "{\n");
			indent.Append('\t');
			newLineAfterCurrent = false;
		}
		void NoBlock()
		{
			if (!canChangeBlock || block != "static") return;
			block.Set("");
			indent.Length--;
			str.Append(indent, "}\n");
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

		switch (HandleCursor(cursor))
		{
		case .SkipCursor: return;
		case .Custom(let staticBlock, let singleLine, let write):
			if (staticBlock) StaticBlock(); else NoBlock();
			if (singleLine) SingleLine(cursor); else MultiLine(cursor);
			write(this, cursor);
			return;
		case .Include:
		}

		switch (cursor.kind)
		{
		case .StructDecl,
			 .ClassDecl,
			 .UnionDecl: NoBlock(); MultiLine(cursor); Record(cursor);
		case .EnumDecl: NoBlock(); MultiLine(cursor); Enum(cursor);
		case .TypeAliasDecl, .TypedefDecl: NoBlock(); SingleLine(cursor); TypeAlias(cursor);
		case .Namespace: NoBlock(); MultiLine(cursor); Namespace(cursor);

		case .FunctionDecl: StaticBlock(); SingleLine(cursor); FunctionDecl(cursor);
		case .CXXMethod,
			 .Constructor,
			 .Destructor,
			 .ConversionFunction: StaticBlock(); SingleLine(cursor); CXXMethod(cursor);
		case .FieldDecl: StaticBlock(); SingleLine(cursor); FieldDecl(cursor);
		case .VarDecl: StaticBlock(); SingleLine(cursor); VarDecl(cursor);

		case .FunctionTemplate: StaticBlock(); SingleLine(cursor);
		case .ClassTemplate: NoBlock(); MultiLine(cursor);
		case .TypeAliasTemplateDecl: NoBlock(); SingleLine(cursor);

		case .MacroDefinition: StaticBlock(); SingleLine(cursor); MacroDefinition(cursor);

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

	void SingleLine(CXCursor cursor)
	{
		if (newLineAfterCurrent)
			str.Append('\n');
		newLineAfterCurrent = DocString(cursor);
		str.Append(indent);
	}
	void MultiLine(CXCursor cursor)
	{
		str.Append('\n');
		newLineAfterCurrent = true;
		DocString(cursor);
		str.Append(indent);
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
		case .LValueReference: str.Append("ref "); Type(Clang.GetNonReferenceType(type));
		case .RValueReference: str.Append("in "); Type(Clang.GetNonReferenceType(type));
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

	protected mixin BeginBody()
	{
		str.Append("\n", indent, "{\n");
		indent.Append('\t');
		newLineAfterCurrent = false;
		defer:mixin
		{
			indent.Length--;
			str.Append(indent, "}\n");
			newLineAfterCurrent = true;
		}
	}

	protected void Linkable_Attributes(CXCursor cursor)
	{
		let mangledName = ScopeCXString!(Clang.Cursor_GetMangling(cursor));
		let name = GetNameInBindings(cursor, ..scope .(mangledName.Length));
		WriteCustomAttributes(cursor);
		if (mangledName == name)
			str.Append("[CLink] ");
		else
			str.Append("[LinkName(\"", mangledName, "\")] ");
	}

	protected void Method_Parameters(CXCursor cursor)
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

	protected mixin ScopeTokenize(CXCursor cursor)
	{
		let range = Clang.GetCursorExtent(cursor);
		CXToken* tokenPtr = null; uint32 tokenCount = 0;
		Clang.Tokenize(unit, range, &tokenPtr, &tokenCount);
		defer:mixin Clang.DisposeTokens(unit, tokenPtr, tokenCount);
		Span<CXToken>(tokenPtr, (.)tokenCount)
	}

	protected void WriteTokensAfterEquals(CXCursor cursor)
	{
		var tokens = ScopeTokenize!(cursor);
		findEquals: do
		{
			for (let token in tokens)
			{
				let spelling = GetTokenSpelling!(token);
				if (spelling != "=") continue;
				tokens.RemoveFromStart(@token.Index);
				break findEquals;
			}
			return;
		}
		WriteTokens(tokens, .Identifier);
	}

	protected virtual void WriteTokens(Span<CXToken> tokens, CXTokenKind prev)
	{
		const CXTokenKind Unary = (.)-1;
		var prev;
		for (let token in tokens)
		{
			let spelling = ScopeCXString!(Clang.GetTokenSpelling(unit, token));
			let kind = Clang.GetTokenKind(token);
			token: switch (kind)
			{
			case .Keyword, .Identifier, .Literal:
				if (prev == .Keyword || prev == .Identifier || prev == .Punctuation)
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
				if (prev != Unary && !(spelling == "(" && prev != .Punctuation) && spelling != ")")
				{
					if (spelling != ",")
						str.Append(' ');
					if (prev != .Punctuation)
						break;
				}
				prev = Unary;
				continue;
			case .Comment:
			}
			prev = kind;
		}
	}

	protected virtual void Namespace(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		str.Append("static class ");
		GetNameInBindings(cursor, str);
		{
			BeginBody!();
			Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
			{
				Self self = (.)Internal.UnsafeCastToObject(client_data);
				self.WriteCursor(cursor, canChangeBlock: false);
				return .Continue;
			}, Internal.UnsafeCastToPtr(this));
		}
		if (Clang.Cursor_IsInlineNamespace(cursor) != 0)
		{
			str.Append("public static using ");
			GetNameInBindings(cursor, str);
			str.Append(";\n");
		}
	}

	protected virtual void FunctionDecl(CXCursor cursor)
	{
		Linkable_Attributes(cursor);
		AccessSpecifier(cursor);
		str.Append("static extern ");
		Type(Clang.GetCursorResultType(cursor));
		str.Append(' ');
		GetNameInBindings(cursor, str);
		str.Append('(');
		Method_Parameters(cursor);
		str.Append(");\n");
	}

	protected virtual void CXXMethod(CXCursor cursor)
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
				str.Append("extern ");
				Type(Clang.GetCursorResultType(cursor));
				str.Append(" this[");
				Method_Parameters(cursor);
				str.Append("] { ");
				Attributes();
				str.Append("get; }\n");
				return;
			}

			Attributes();
			AccessSpecifier(cursor);
			str.Append("static extern ");
			GetNameInBindings(cursor, str);
			str.Append("(in Self, ");
			Method_Parameters(cursor);
			str.Append(");\n");
			return;
		}

		Attributes();
		AccessSpecifier(cursor);
		str.Append("extern ");
		switch (cursor.kind)
		{
		case .Constructor: str.Append("this");
		case .Destructor: str.Append("~this");
		case .CXXMethod:
			Type(Clang.GetCursorResultType(cursor));
			str.Append(' ');
			GetNameInBindings(cursor, str);
		default:
			Runtime.FatalError("Unhandled c++ method kind");
		}
		str.Append('(');
		Method_Parameters(cursor);
		str.Append(");\n");
	}

	protected virtual void FieldDecl(CXCursor cursor)
	{
		WriteCustomAttributes(cursor);
		AccessSpecifier(cursor);
		Type(Clang.GetCursorType(cursor));
		str.Append(' ');
		GetNameInBindings(cursor, str);
		str.Append(";\n");
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
			Type(Clang.GetCursorType(cursor));
			str.Append(' ');
			GetNameInBindings(cursor, str);
			WriteTokensAfterEquals(cursor);
			str.Append(";\n");
		case .External, .UniqueExternal:
			Linkable_Attributes(cursor);
			AccessSpecifier(cursor);
			str.Append("static extern ");
			Type(Clang.GetCursorType(cursor));
			str.Append(' ');
			GetNameInBindings(cursor, str);
			str.Append(";\n");
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
		BeginBody!();

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
				self.str..TrimEnd()..Append(";\n");
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
		BeginBody!();
		Clang.VisitChildren(cursor, (cursor, parent, client_data) =>
		{
			Self self = (.)Internal.UnsafeCastToObject(client_data);
			Runtime.Assert(cursor.kind == .EnumConstantDecl);

			self.SingleLine(cursor);
			self.WriteCustomAttributes(cursor);
			self.GetNameInBindings(cursor, self.str);
			self.WriteTokensAfterEquals(cursor);
			//Clang.GetEnumConstantDeclValue(cursor).ToString(self.str);
			self.str.Append(",\n");

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
			Type(Clang.GetResultType(pointee));
			str.Append(' ');
			GetNameInBindings(cursor, str);
			WriteFunctionProtoParams(pointee);
			str.Append(";\n");
			return;
		}

		WriteCustomAttributes(cursor);
		str.Append("typealias ");
		GetNameInBindings(cursor, str);
		str.Append(" = ");
		Type(type);
		str.Append(";\n");
	}

	protected virtual void MacroDefinition(CXCursor cursor)
	{
		let tokens = ScopeTokenize!(cursor);
		WriteCustomAttributes(cursor);
		str.Append("public const let ");
		GetNameInBindings(cursor, str);
		str.Append(" =");
		WriteTokens(tokens[1...], .Punctuation);
		str.Append(";\n");
	}
}
