using System;
using System.IO;
using System.Collections;
using System.Diagnostics;

namespace Cpp2Beef;

static class CppWrapperWriter<WrapperFilePath> where WrapperFilePath : const String
{
	public const String filePath = WrapperFilePath;
	public static HashSet<Type> structs = Compiler.IsComptime ? new .(128) : null ~ delete _;

	[Comptime, OnCompile(.TypeDone)]
	static void OnTypeDone()
	{
		if (!Compiler.IsBuilding) return;

		StringView filePathView = filePath;
		if (filePathView.IsNull) return;
		let tempPath = scope $"{filePath}.temp";
		{
			StreamReader reader = scope .()..Open(filePathView);
			StreamWriter writer = scope .()..Create(tempPath);
			String line = scope .(128);
			while (true)
			{
				StringView lineView = reader.ReadLine(..line..Clear());
				lineView.Trim();
				writer.WriteLine(lineView);
				if (lineView == "//begin-comptime") break;
			}
		}
		File.Delete(filePathView);
		File.Move(tempPath, filePathView);
	}

	public static void Write(StringView text)
	{
		if (Compiler.IsBuilding)
			File.WriteAllText(filePath, text, doAppend: true);
	}
}

static
{
	[Comptime(ConstEval=true)]
	public static String CppWrapperF(String format, params Span<Type> args)
	{
		String outString = scope .(128);
		outString.AppendF(format, args);
		if (outString.EndsWith('*')) outString.Append("PTR");
		for (int i < outString.Length)
			if (!outString[i].IsLetterOrDigit)
			{
				outString[i] = '_';
			}
		return outString;
	}

	[Comptime(ConstEval=true)]
	public static String CppTypeToC(Type type)
	{
		switch (type)
		{
		case typeof(int): return("intptr_t");
		case typeof(int8), typeof(char8): return("int8_t");
		case typeof(int16), typeof(char16): return("int16_t");
		case typeof(int32), typeof(char32): return("int32_t");
		case typeof(int64): return("int64_t");

		case typeof(uint): return("uintptr_t");
		case typeof(uint8): return("uint8_t");
		case typeof(uint16): return("uint16_t");
		case typeof(uint32): return("uint32_t");
		case typeof(uint64): return("uint64_t");

		case typeof(bool): return("bool");
		case typeof(float): return("float");
		case typeof(double): return("double");

		default:
			if (type.IsValueType)
				return(scope $"char[{type.Size}]");
		
			return("void*");
		}
	}
}
