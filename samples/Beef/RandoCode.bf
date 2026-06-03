using System;
using System.Collections;
using System.Text;
using System.IO;
using System.Threading.Tasks;
using System.Threading;
using Beefy.utils;
using System.Diagnostics;

namespace RandoCode
{
    class Config
    {
        public int32 mTypeCount = 800; //20; //70 //300
        public float mPrimitiveTypeChance = 0.0f;

        //public float mStructPct = 0.2f; // Otherwise class
        public float mStructPct = 0.0f;

        public float mTypeComplexityPower = 3.0f;

        public float mCreateGenericTypePct = 0.15f;
        public float mSpecializedTypePower = 3.0f;
        public float mUnspecializedTypeScalar = 10.0f;

        public float mTypeDefPoolPower = 1.1f;
        public float mTypeDefPoolOffset = 3.0f;
        public float mTypeDefPoolScalar = 15.0f;

        public float mFieldCountPower = 1.2f;
        public float mFieldCountScalar = 12.0f;
        public float mFieldStaticPct = 0.2f;

        public float mVoidReturnPct = 0.6f;
        public float mMethodCodeComplexityPower = 2.0f;
		public float mMethodLengthScalar = 100.0f;
        public float mMethodCountScalar = 300.0f; //2000.0f;//300.0f;
        public float mAssignMemberPct = 0.3f;
        public float mCreateLocalPct = 0.3f;
        
        public float mParamCountPower = 3.0f;
        public float mParamCountScalar = 5.0f;

        public float mNewSpacespaceChance = 0.2f; // Chance a class will generate a new namespace vs using an existing one
        public float mRootNamespaceChance = 0.1f; // Chance we will create root namespace vs adding to an existing one
    }

    class WordGroup
    {
        public List<String>[] mWords ~
		{
			for (var list in mWords)
				DeleteContainerAndItems!(list);
			delete _;
		};
        public HashSet<String> mUsedNames = new HashSet<String>() ~ DeleteContainerAndItems!(_);

        public void GetName(float complexity, int parts, bool firstUpper, String outName)
        {
            int listNum = (int)(complexity * 3.999f);

			var parts;
            for (int tryCount = 0; true; tryCount++)
            {
                if (tryCount > 4)
                    parts++;

                for (int namePartIdx = 0; namePartIdx < parts; namePartIdx++)
                {
                    int idx = Program.sRand.Next(mWords[listNum].Count);
                    String namePart = scope String(mWords[listNum][idx]);
                    if ((firstUpper) || (namePartIdx > 0))
						namePart[0] = namePart[0].ToUpper;
                    outName.Append(namePart);
                }

                if (mUsedNames.Contains(outName))
                    continue;
                mUsedNames.Add(new String(outName));
                return;
            }
        }
    }
    
    class LocalDef
    {
        public String mName ~ delete _;
        public TypeDef mTypeDef;
    }    

    class MethodDef
    {
        public String mName ~ delete _;
        public int mParamCount;
        public TypeDef mReturnType;
        public List<LocalDef> mLocals = new List<LocalDef>() ~ DeleteContainerAndItems!(_);
        public float mComplexity;
        public int mStatementCount;
		public bool mIsStatic;
    }

    class FieldDef
    {
        public String mName ~ delete _;
        public TypeDef mTypeDef;
        public bool mIsStatic;
    }    

    class TypeDef
    {
        public NamespaceDef mNamespace;        
        public String mName ~ delete _;
        public float mComplexity;
        public bool mIsPrimitive;
        public int mGenericParamIdx = -1;
        public bool mIsStruct;
        public int mUseCount;

        public bool mIsSpecializedGeneric;
        public bool mIsUnspecializedGeneric;
        public List<TypeDef> mGenericParams ~ delete _;

        public HashSet<NamespaceDef> mUsingNamespaces = new HashSet<NamespaceDef>() ~ delete _;
        public List<FieldDef> mFields = new List<FieldDef>() ~ DeleteContainerAndItems!(_);
        public List<MethodDef> mMethods = new List<MethodDef>() ~ DeleteContainerAndItems!(_);
        
        public List<TypeDef> mTypeDefPool = new List<TypeDef>() ~ delete _; // We only refer to types in this pool

        public void GetRootName(String outStr)
        {
            int ltPos = mName.IndexOf('<');
            if (ltPos == -1)
			{
				outStr.Append(mName);
                return;
			}
			outStr.Append(mName, 0, ltPos);
        }

        public void GetFullName(String outStr)
        {
            if (mNamespace == null)
            {
				outStr.Append(mName);
				return;
			}
            mNamespace.GetFullName(outStr);
			outStr.Append(".", mName);
        }
    }

    class NamespaceDef
    {
        public NamespaceDef mParent;
        public String mName ~ delete _;
        public List<NamespaceDef> mChildren = new List<NamespaceDef>() ~ delete _;

        public void GetFullName(String outStr)
        {
            if (mParent != null)
			{
				mParent.GetFullName(outStr);
				outStr.Append(".");
			}
            outStr.Append(mName);
        }
    }

    class Program
    {       
        public static Random sRand ~ delete _;
        int mSeed;

        Config mConfig = new Config() ~ delete _;
        String mBaseDir = new String("src") ~ delete _;
        WordGroup mAdjList ~ delete _;
        WordGroup mAdvList ~ delete _;
        WordGroup mNounList ~ delete _;
        WordGroup mVerbList ~ delete _;
		bool mIsCompat = true;

        TypeDef mCurTypeDef;
        MethodDef mCurMethodDef;
        List<NamespaceDef> mNamespaces = new List<NamespaceDef>() ~ DeleteContainerAndItems!(_);
        TypeDef mVoidType ~ delete _;
        List<TypeDef> mPrimitives = new List<TypeDef>() ~ DeleteContainerAndItems!(_);
        List<TypeDef> mUserTypes = new List<TypeDef>() ~ DeleteContainerAndItems!(_);
		List<TypeDef> mOtherTypes = new List<TypeDef>() ~ DeleteContainerAndItems!(_);

		bool mVerbose;
        int mLineCount;
        bool mWroteLine;
        String mQueuedText = new String() ~ delete _;
        int mIndentCount;
        int mStartIndentCount;

        this()
        {
            //mSeed = (scope Random()).Next() % 100000;
            mSeed = 92968;
            Console.WriteLine("Random seed: {0}", mSeed);
            sRand = new Random(mSeed);
        }

        WordGroup CreateWordGroup(String name)
        {                        
            WordGroup wordGroup = new WordGroup();
            wordGroup.mWords = new List<String>[4];
            for (int i = 0; i < 4; i++)
            {
                wordGroup.mWords[i] = new List<String>();

				StreamReader file = scope StreamReader();

				var filePath = scope String();

				var exePath = scope String();
				Environment.GetExecutableFilePath(exePath);
				Path.GetDirectoryPath(exePath, filePath);
				
				filePath.AppendF("/data/{0}{1}.txt", i + 1, name);

				if (file.Open(filePath) case .Err)
					continue;

                while (!file.EndOfStream)
                {
                    String line = scope String();
					file.ReadLine(line);
                    bool isOnlyLetters = true;
                    for (char8 c in line.RawChars)
                        if (!c.IsLetter)
                            isOnlyLetters = false;
                    if (isOnlyLetters)
                        wordGroup.mWords[i].Add(new String(line));
                }
            }
            return wordGroup;
        }

        void CreatePrimitives()
        {
            mVoidType = new TypeDef();
            mVoidType.mName = new String("void");
            mVoidType.mIsPrimitive = true;

            String[] typeNames;
			if (mIsCompat)
				typeNames = scope:: .[] ( "int", "uint", "int", "float", "double" );
			else
				typeNames = scope:: .[] ( "int", "int16", "int32", "int64", "float", "double" );

            for (var typeName in typeNames)
            {
                TypeDef typeDef = new TypeDef();
                typeDef.mName = new String(typeName);
                typeDef.mIsPrimitive = true;
                mPrimitives.Add(typeDef);
            }
        }

        float GetComplexity(float power)
        {
            return (float)Math.Pow(sRand.NextDouble(), power);
        }

        // Must return either a primitive type or a typedef whose name occurs alphabetically before the current type
        TypeDef GetRandomTypeDef()
        {
            bool wantPrimitive = sRand.NextDouble() < mConfig.mPrimitiveTypeChance;
            if ((!wantPrimitive) && (mUserTypes.Count > 0))
            {
                for (int tryCount = 0; tryCount < 4; tryCount++)
                {
                    TypeDef checkTypeDef = mUserTypes[sRand.Next(mUserTypes.Count)];
                    if (checkTypeDef.mIsUnspecializedGeneric)
                        continue;
                    if (mCurTypeDef == null)
                        return checkTypeDef;
                    if (checkTypeDef.mName.CompareTo(mCurTypeDef.mName) < 0)
                        return checkTypeDef;                    
                }
            }
            return mPrimitives[sRand.Next(mPrimitives.Count)];
        }

        TypeDef GetRandomPooledTypeDef()
        {
            return mCurTypeDef.mTypeDefPool[sRand.Next(mCurTypeDef.mTypeDefPool.Count)];
        }

        void GenerateType()
        {
            float typeComplexity = GetComplexity(mConfig.mTypeComplexityPower);
            String className = new String();
			mNounList.GetName(typeComplexity, 2, true, className);

            TypeDef typeDef = new TypeDef();
            typeDef.mName = className;
            typeDef.mComplexity = typeComplexity;
            mCurTypeDef = typeDef;

            typeDef.mIsUnspecializedGeneric = sRand.NextDouble() < mConfig.mCreateGenericTypePct;
            if (typeDef.mIsUnspecializedGeneric)
            {
                typeDef.mGenericParams = new List<TypeDef>();
                int genericCount = (int)(1.0f + GetComplexity(3.0f) * 3.5f);
                typeDef.mName.Append("<");
                for (int genericIdx = 0; genericIdx < genericCount; genericIdx++)
                {
                    TypeDef genericType = new TypeDef();
                    genericType.mGenericParamIdx = genericIdx;
                    genericType.mName = new String();
					genericType.mName.Append("T");
					mOtherTypes.Add(genericType);

					mAdvList.GetName(0, 1, true, genericType.mName);

                    if (genericIdx > 0)
                        typeDef.mName.Append(", ");
                    typeDef.mName.Append(genericType.mName);

                    typeDef.mGenericParams.Add(genericType);
                    typeDef.mTypeDefPool.Add(genericType);
                }
                typeDef.mName.Append(">");
            }

            typeDef.mIsStruct = sRand.NextDouble() < mConfig.mStructPct;            
             
            if ((mNamespaces.Count == 0) || (sRand.NextDouble() < mConfig.mNewSpacespaceChance))
            {
                NamespaceDef newNamespace = new NamespaceDef();
                newNamespace.mName = new String();
				mAdjList.GetName((float)sRand.NextDouble(), 1, true, newNamespace.mName);

                if ((mNamespaces.Count > 0) && (sRand.NextDouble() >= mConfig.mRootNamespaceChance))
                {
                    NamespaceDef parentNamepace = mNamespaces[sRand.Next(mNamespaces.Count)];
                    parentNamepace.mChildren.Add(newNamespace);
                    newNamespace.mParent = parentNamepace;
                }

                mNamespaces.Add(newNamespace);
                typeDef.mNamespace = newNamespace;
            }
            else
            {
                typeDef.mNamespace = mNamespaces[sRand.Next(mNamespaces.Count)];
            }
            
            mUserTypes.Add(typeDef);
        }

        void CreateTypePool(TypeDef typeDef, int poolSize)
        {
            for (int poolIdx = 0; poolIdx < poolSize; poolIdx++)
            {
                TypeDef poolTypeDef = GetRandomTypeDef();
                ReferenceType(poolTypeDef);
                poolTypeDef.mUseCount++;
                typeDef.mTypeDefPool.Add(poolTypeDef);
            }
        }

        void PopulateType(TypeDef typeDef)
        {
            mCurTypeDef = typeDef;

            int poolSize = (int)(GetComplexity(mConfig.mTypeDefPoolPower) * mConfig.mTypeDefPoolScalar + mConfig.mTypeDefPoolOffset);
            CreateTypePool(typeDef, poolSize);
            
            int fieldCount = (int)(GetComplexity(mConfig.mFieldCountPower) * mConfig.mFieldCountScalar);
            if (typeDef.mIsStruct)
                fieldCount++;
            for (int fieldIdx = 0; fieldIdx < fieldCount; fieldIdx++)
            {
                FieldDef fieldDef = new FieldDef();
                fieldDef.mIsStatic = sRand.NextDouble() < mConfig.mFieldStaticPct;

                // Just to make sure structs have at least one non-static member
                if (fieldIdx == 0)
                    fieldDef.mIsStatic = false;

                // Generic statics don't currently work
                if (typeDef.mGenericParams != null)
                    fieldDef.mIsStatic = false;

                if (fieldDef.mIsStatic)
                {
					fieldDef.mName = new String("s");
					mNounList.GetName((float)sRand.NextDouble(), 1, true, fieldDef.mName);
				}
                else
				{
                    fieldDef.mName = new String("m");
					mNounList.GetName((float)sRand.NextDouble(), 1, true, fieldDef.mName);
				}
                
                fieldDef.mTypeDef = GetRandomPooledTypeDef();
                typeDef.mFields.Add(fieldDef);
            }

            for (int methodIdx = 0; methodIdx < (int)(typeDef.mComplexity * mConfig.mMethodCountScalar); methodIdx++)
            {
                MethodDef methodDef = new MethodDef();

                mCurMethodDef = methodDef;
                float methodComplexity = GetComplexity(mConfig.mMethodCodeComplexityPower);
                methodDef.mName = new String();
				mVerbList.GetName(methodComplexity, 2, true, methodDef.mName);
                methodDef.mComplexity = methodComplexity;
                
                if (sRand.NextDouble() < mConfig.mVoidReturnPct)
                    methodDef.mReturnType = mVoidType;
                else
                    methodDef.mReturnType = GetRandomPooledTypeDef();

                int paramCount = (int)(GetComplexity(mConfig.mParamCountPower) * mConfig.mParamCountScalar);
                for (int paramIdx = 0; paramIdx < paramCount; paramIdx++)
                {
                    LocalDef localDef = new LocalDef();
                    localDef.mName = new String();
					mNounList.GetName((float)sRand.NextDouble(), 1, false, localDef.mName);
                    localDef.mTypeDef = GetRandomPooledTypeDef();
                    methodDef.mLocals.Add(localDef);
                    methodDef.mParamCount++;
                }

                typeDef.mMethods.Add(methodDef);
            }

			MethodDef methodDef = new MethodDef();
			methodDef.mIsStatic = true;
			methodDef.mName = new String("Use");
			methodDef.mReturnType = mVoidType;
			typeDef.mMethods.Add(methodDef);
        }

        void DelTree(StringView dirName)
        {
			Debug.Assert(!dirName.IsEmpty);

			if (!Directory.Exists(dirName))
				return;

            for (var subDir in Directory.EnumerateDirectories(dirName))
            {
				var filePath = scope String();
				subDir.GetFilePath(filePath);
                DelTree(filePath);
            }

            for (var file in Directory.EnumerateFiles(dirName))
            {
				var filePath = scope String();
				file.GetFilePath(filePath);
                if (filePath.EndsWith(".bf"))
                {
                    File.Delete(filePath);
                }
            }
        
            Directory.Delete(dirName);
        }

        FieldDef FindField(TypeDef inside, TypeDef wantTypeDef)
        {
            for (var field in inside.mFields)
            {
                if (field.mTypeDef == wantTypeDef)
                {
                    if (field.mName == "sTar")
                    {
                        NOP!();
                    }
                    return field;
                }
            }
            return null;
        }

        void FindData(TypeDef typeDef, String outStr)
        {
            for (int pass = 0; pass < 10; pass++)
            {
                int targetCategory = (int)(GetComplexity(1.0f) * 3);
                
                if (targetCategory == 0) // 'This'
                {
                    if (typeDef == mCurTypeDef)
                    {
						outStr.Append("this");
						return;
					}
                }

                if ((targetCategory == 1) && (mCurTypeDef.mFields.Count > 0)) // Field
                {
                    var fieldDef = mCurTypeDef.mFields[(int)(sRand.NextDouble() * mCurTypeDef.mFields.Count)];
                    if (fieldDef.mTypeDef == typeDef)
					{
                        outStr.Append(fieldDef.mName);
						return;
					}
                    FieldDef subFieldDef = FindField(fieldDef.mTypeDef, typeDef);
                    if (subFieldDef != null)
                    {
                        if (subFieldDef.mName == "sTar")
                        {
                            
                        }

                        if (subFieldDef.mIsStatic)
						{
                            outStr.Append(fieldDef.mTypeDef.mName, ".", subFieldDef.mName);
							return;
						}
                        else
						{
                            outStr.Append(fieldDef.mName, ".", subFieldDef.mName);
							return;
						}
                    }
                }

                if ((targetCategory == 2) && (mCurMethodDef.mLocals.Count > 0)) // Param / Local
                {
                    var localDef = mCurMethodDef.mLocals[(int)(sRand.NextDouble() * mCurMethodDef.mLocals.Count)];
                    if (localDef.mTypeDef == typeDef)
					{
                        outStr.Append(localDef.mName);
						return;
					}
                    FieldDef subFieldDef = FindField(localDef.mTypeDef, typeDef);
                    if (subFieldDef != null)
                    {
                        if (subFieldDef.mIsStatic)
						{
                            outStr.Append(localDef.mTypeDef.mName, ".", subFieldDef.mName);
							return;
						}
                        else
						{
                            outStr.Append(localDef.mName, ".", subFieldDef.mName);
							return;
						}
                    }
                }
            }

            if (typeDef.mIsPrimitive)
			{
                outStr.Append("0");
				return;
			}
            if ((!typeDef.mIsStruct) && (typeDef.mGenericParamIdx == -1))
			{
				outStr.Append("null");
				return;
			}
        }

        void WriteLine()
        {
            WriteLine("");
        }

        void WriteLine(String str)
        {
            mLineCount++;
            mWroteLine = true;
            for (int i = 0; i < mIndentCount; i++)
                mQueuedText.Append("\t");
            mQueuedText.Append(str, "\n");
        }

        void WriteLine(String str, params Object[] args)
        {                       
            WriteLine(scope String()..AppendF(str, params args));
        }

        void ReferenceType(TypeDef typeDef)
        {
            if (typeDef.mNamespace != null)
                mCurTypeDef.mUsingNamespaces.Add(typeDef.mNamespace);
            if (typeDef.mGenericParams != null)
            {
                for (var genericParam in typeDef.mGenericParams)
                    ReferenceType(genericParam);
            }
        }

        String GetRandomTarget(out TypeDef targetType)
        {
            int targetCategory = (int)(GetComplexity(1.0f) * 3);

            if (targetCategory == 0) // 'This'
            {                
                targetType = mCurTypeDef;
                return "";
            }

            if ((targetCategory == 1) && (mCurTypeDef.mFields.Count > 0)) // Field
            {
                var fieldDef = mCurTypeDef.mFields[(int)(sRand.NextDouble() * mCurTypeDef.mFields.Count)];                
                targetType = fieldDef.mTypeDef;
                return fieldDef.mName;
            }

            if ((targetCategory == 2) && (mCurMethodDef.mLocals.Count > 0)) // Param / Local
            {
                var localDef = mCurMethodDef.mLocals[(int)(sRand.NextDouble() * mCurMethodDef.mLocals.Count)];                
                targetType = localDef.mTypeDef;
                return localDef.mName;
            }

            targetType = null;
            return null;
        }

        MethodDef GenerateMethodCall(String methodTarget, TypeDef targetType)
        {
            if (targetType != null)
            {
                if (targetType.mMethods.Count > 0)
                {
                    var methodDef = targetType.mMethods[sRand.Next(targetType.mMethods.Count)];
					if (methodDef.mIsStatic)
						return null;

                    String[] targets = scope String[methodDef.mParamCount];
                    bool paramsMatch = true;
                    for (int paramIdx = 0; paramIdx < methodDef.mParamCount; paramIdx++)
                    {
                        var paramDef = methodDef.mLocals[paramIdx];
                        targets[paramIdx] = scope:: String();
						FindData(paramDef.mTypeDef, targets[paramIdx]);
                        if (targets[paramIdx].IsEmpty)
                            paramsMatch = false;
                    }

                    if (paramsMatch)
                    {
                        String str = scope String();

                        bool didAssign = false;
                        if (sRand.NextDouble() < mConfig.mAssignMemberPct)
                        {
                            for (var fieldDef in mCurTypeDef.mFields)
                            {
                                if (fieldDef.mTypeDef == methodDef.mReturnType)
                                {
                                    str.Append(fieldDef.mName, " = ");
                                    didAssign = true;
                                    break;
                                }
                            }
                        }

                        if ((!didAssign) && (sRand.NextDouble() < mConfig.mCreateLocalPct) && (methodDef.mReturnType != mVoidType))
                        {
                            ReferenceType(methodDef.mReturnType);

                            LocalDef localDef = new LocalDef();
                            localDef.mName = new String();
							mNounList.GetName((float)sRand.NextDouble(), 1, false, localDef.mName);
                            localDef.mTypeDef = methodDef.mReturnType;
                            mCurMethodDef.mLocals.Add(localDef);
                            str.Append(localDef.mTypeDef.mName);
                            str.Append(" ");
                            str.Append(localDef.mName);
                            str.Append(" = ");
                        }

                        str.Append(methodTarget);
                        if (methodTarget != "")
                            str.Append(".");
                        str.Append(methodDef.mName);
                        str.Append("(");
                        for (int paramIdx = 0; paramIdx < methodDef.mParamCount; paramIdx++)
                        {
                            if (paramIdx > 0)
                                str.Append(", ");
                            str.Append(targets[paramIdx]);
                        }
                        str.Append(");");
                        WriteLine(str);
                        return methodDef;
                    }
                }
            }

            return null;
        }

        MethodDef GenerateMethodCall()
        {            
            TypeDef targetType = null;
            String methodTarget = GetRandomTarget(out targetType);
            return GenerateMethodCall(methodTarget, targetType);  
        }

        void PopLocal()
        {            
            var localDef = mCurMethodDef.mLocals.PopBack();
			delete localDef;
        }

        void GetBoolExpression(String outStr)
        {
            TypeDef targetType;
            String checkTarget = GetRandomTarget(out targetType);
            if (checkTarget == null)
                return;
            if (checkTarget == "")
                checkTarget = "this";

            if (targetType.mIsStruct)
                return;
            if (targetType.mGenericParamIdx != -1)
                return;

            String rhs = scope String();
			FindData(targetType, rhs);
            if ((rhs != null) && (checkTarget != rhs))
            {
				outStr.Append(checkTarget, " != ", rhs);
				return;
			}

            if (targetType.mIsPrimitive)
            {
				outStr.Append(checkTarget, " != 0");
				return;
			}
            outStr.Append(checkTarget, " != null");
        }

        void GenerateMainBlock()
        {
            WriteLine("{");
            mIndentCount++;

            for (var pooledType in mUserTypes)
            {
				if (pooledType.mIsPrimitive)
					continue;
				if (pooledType.mIsUnspecializedGeneric)
					continue;
				if (pooledType.mName == "Program")
					continue;

				var line = scope String();
				pooledType.GetFullName(line);
				line.Append(".Use();");
				WriteLine(line);
            }
			WriteLine("return 0;");
            mIndentCount--;
            WriteLine("}");
        }

		void GenerateUseBlock()
		{
		    WriteLine("{");
		    mIndentCount++;

			var line = scope String();
			mCurTypeDef.GetFullName(line);

			line.Append(" val = ");
			if (!mCurTypeDef.mIsStruct)
				line.Append("new ");
			mCurTypeDef.GetFullName(line);

			line.Append("();");

			WriteLine(line);

			for (var method in mCurTypeDef.mMethods)
			{
				if (method.mIsStatic)
					continue;
				line.Clear();
				line.Append("val.", method.mName);
				line.Append("(");
				for (int argIdx < method.mParamCount)
				{
					if (argIdx > 0)
						line.Append(", ");
					line.Append("default");
				}
				line.Append(");");
				WriteLine(line);
			}

			mIndentCount--;
			WriteLine("}");
		}

        void GenerateBlock()
        {
            int prevLocalIdx = mCurMethodDef.mLocals.Count;

            WriteLine("{");
            mIndentCount++;

			int methodLength = (int)(mCurMethodDef.mComplexity * mConfig.mMethodLengthScalar);

            mWroteLine = false;
            //while (true)
			for (int stmtIdx < methodLength)
            {
                /*if (mCurMethodDef.mStatementCount > 0)
                    mCurMethodDef.mStatementCount--;
                else if ((mWroteLine) && ((0.93 + mCurMethodDef.mComplexity * 0.03) < sRand.NextDouble()))
                    break;*/

                if (sRand.NextDouble() < 0.5f)
                {
                    GenerateMethodCall();                    
                }
                
                if (sRand.NextDouble() < 0.05f / mIndentCount)
                {
                    String localVarName = new String()..AppendF("{0}", (char8)('i' + mIndentCount - 3));
                    
                    String toVal = scope String();
					FindData(mPrimitives[0], toVal);
                    if (toVal.IsEmpty)
                    {
						toVal = scope:: String();
						sRand.Next(1000).ToString(toVal);
					}

                    LocalDef localDef = new LocalDef();
                    localDef.mName = localVarName;
                    localDef.mTypeDef = mPrimitives[0];
                    mCurMethodDef.mLocals.Add(localDef);

                    WriteLine("for (int {0} = 0; {0} < {1}; {0}++)", localVarName, toVal);
                    GenerateBlock();

                    PopLocal();
                }

                if (sRand.NextDouble() < 0.05f / mIndentCount)
                {
                    String boolExpr = scope String();
					GetBoolExpression(boolExpr);
                    if (!boolExpr.IsEmpty)
                    {
                        WriteLine("if ({0})", boolExpr);
                        GenerateBlock();
                        if (sRand.NextDouble() < 0.35f)
                        {
                            WriteLine("else");
                            GenerateBlock();
                        }
                    }
                }
            }

            if (mIndentCount == mStartIndentCount + 1)
            {
                if (mCurMethodDef.mReturnType.mName != "void")
                {
                    String retValName = scope String();
					FindData(mCurMethodDef.mReturnType, retValName);
                    if (retValName.IsEmpty)
                        retValName.Append("default(", mCurMethodDef.mReturnType.mName, ")");
                    WriteLine("return {0};", retValName);
                }
            }

            mIndentCount--;
            WriteLine("}");

            while (mCurMethodDef.mLocals.Count > prevLocalIdx)
                PopLocal();
        }

        TypeDef FixType(TypeDef typeDef)
        {
            if (typeDef.mGenericParamIdx != -1)
                return mCurTypeDef.mGenericParams[typeDef.mGenericParamIdx];
            return typeDef;
        }

        void SpecializeType(TypeDef unspecializedType)
        {
            TypeDef specializedType = new TypeDef();
            mCurTypeDef = specializedType;
            specializedType.mName = new String();
			unspecializedType.GetRootName(specializedType.mName);
            specializedType.mIsSpecializedGeneric = true;
            specializedType.mGenericParams = new List<TypeDef>();
            specializedType.mIsStruct = unspecializedType.mIsStruct;
            specializedType.mNamespace = unspecializedType.mNamespace;
            specializedType.mName.Append("<");
            for (int genericIdx = 0; genericIdx < unspecializedType.mGenericParams.Count; genericIdx++)                
            {
                if (genericIdx > 0)
                    specializedType.mName.Append(", ");
                var genericArg = GetRandomTypeDef();
                //specializedType.mName.Append(genericArg.mName);
				genericArg.GetFullName(specializedType.mName);
                specializedType.mGenericParams.Add(genericArg);
            }
            specializedType.mName.Append(">");

            for (var srcFieldDef in unspecializedType.mFields)
            {
                FieldDef destFieldDef = new FieldDef();
                destFieldDef.mName = new String(srcFieldDef.mName);
                destFieldDef.mTypeDef = FixType(srcFieldDef.mTypeDef);
                destFieldDef.mIsStatic = srcFieldDef.mIsStatic;
                specializedType.mFields.Add(destFieldDef);
            }

            for (var srcMethodDef in specializedType.mMethods)
            {
                MethodDef destMethodDef = new MethodDef();
                destMethodDef.mName = new String(srcMethodDef.mName);
                destMethodDef.mReturnType = FixType(srcMethodDef.mReturnType);
                destMethodDef.mParamCount = srcMethodDef.mParamCount;
                for (var localDef in srcMethodDef.mLocals)
                {
                    LocalDef destLocalDef = new LocalDef();
                    destLocalDef.mName = new String(localDef.mName);
                    destLocalDef.mTypeDef = FixType(localDef.mTypeDef);
                    destMethodDef.mLocals.Add(destLocalDef);
                }
                specializedType.mMethods.Add(destMethodDef);
            }
            mUserTypes.Add(specializedType);
        }

		void ProgressStart()
		{
			if (cProgressSize > 0)
			{
				String str = scope String();
				str.Append("[");
				str.Append(' ', cProgressSize);
				str.Append("]");
				str.Append('\b', cProgressSize + 1);
				Console.Write(str);
			}
		}

		int mProgressIdx = 0;
		void WriteProgress(float pct)
		{
			int progressIdx = (int)Math.Round(pct * cProgressSize);
			while (progressIdx > mProgressIdx)
			{
				mProgressIdx++;
				Console.Write("*");
			}
		}

		const int cProgressSize = 30;

        void Run()
        {
            CreatePrimitives();

            DelTree(mBaseDir);

            mAdjList = CreateWordGroup("adj");
            mAdvList = CreateWordGroup("adv");
            mNounList = CreateWordGroup("noun");
            mVerbList = CreateWordGroup("verb");

            for (int typeIdx = 0; typeIdx < mConfig.mTypeCount; typeIdx++)
                GenerateType();
            
            for (int typeIdx = 0; typeIdx < mUserTypes.Count; typeIdx++ )
            {
                TypeDef typeDef = mUserTypes[typeIdx];                                
                if (typeDef.mIsUnspecializedGeneric)
                {
                    PopulateType(typeDef);
                    int specializeCount = (int)(GetComplexity(mConfig.mSpecializedTypePower) * mConfig.mUnspecializedTypeScalar);
                    for (int specializedIdx = 0; specializedIdx < specializeCount; specializedIdx++)
                        SpecializeType(typeDef);
                }
            }

            for (int typeIdx = 0; typeIdx < mUserTypes.Count; typeIdx++)
            {
                TypeDef typeDef = mUserTypes[typeIdx];
                if ((!typeDef.mIsUnspecializedGeneric) && (!typeDef.mIsSpecializedGeneric))
                    PopulateType(typeDef);
            }

            {
                TypeDef typeDef = new TypeDef();
                typeDef.mName = new String("Program");

                MethodDef methodDef = new MethodDef();
                methodDef.mName = new String("Main");
				methodDef.mParamCount = 1;

				var arrTypeDef = new TypeDef();
				arrTypeDef.mName = new String("System.String[]");
				mOtherTypes.Add(arrTypeDef);

				LocalDef localDef = new LocalDef();
				localDef.mName = new String("args");
				localDef.mTypeDef = arrTypeDef;
				
				methodDef.mLocals.Add(localDef);
                methodDef.mStatementCount = mUserTypes.Count + mPrimitives.Count;
                methodDef.mReturnType = mPrimitives[2];
				methodDef.mIsStatic = true;
                typeDef.mMethods.Add(methodDef);

                mCurTypeDef = typeDef;
                CreateTypePool(typeDef, mUserTypes.Count + mPrimitives.Count);

                mUserTypes.Add(typeDef);
            }

			if (!mVerbose)
				ProgressStart();

            int specializedTypes = 0;
            int unspecializedTypes = 0;
            for (var typeDef in mUserTypes)
			UserTypeBlock:
            {
				var typeFullName = scope String();
				typeDef.GetFullName(typeFullName);

                if (typeDef.mIsSpecializedGeneric)
                {
                    specializedTypes++;
					if (mVerbose)
                    	Console.WriteLine("Skipping type: {0} uses: {1}", typeFullName, typeDef.mUseCount);
                    continue;
                }

                if (typeDef.mIsUnspecializedGeneric)
                    unspecializedTypes++;
				if (mVerbose)
                	Console.WriteLine("Writing type: {0} uses: {1}", typeFullName, typeDef.mUseCount);

                String directory = scope String();
                String namespaceStr = scope String();
                if (typeDef.mNamespace != null)
                {
					typeDef.mNamespace.GetFullName(namespaceStr);
                    directory.Append(mBaseDir, "/");
					directory.Replace('.', '/');
                }
                else
                {
                    directory.Append(mBaseDir);
                }
                
                //StringWriter StringWriter = new StringWriter();
                //mFile = StringWriter;

                String fullPath = scope String()..Append(directory, "/");
				typeDef.GetRootName(fullPath);
				fullPath.Append(".bf");

                FileStream file = scope FileStream();
				bool isOpen = false;

                for (int i = 0; i < 500; i++)
                {
                    Directory.CreateDirectory(directory).IgnoreError();

					if (file.Create(fullPath) case .Ok)
					{
						isOpen = true;
						break;
					}
                    Thread.Sleep(10);
                }

				if (!isOpen)
					Runtime.FatalError("Unable to create file");

				StreamWriter streamWrite = scope .(file, Encoding.UTF8, 4096);

                mCurTypeDef = typeDef;
                //mFile = file;
                mIndentCount = 0;

                WriteLine("// RandoCode seed: {0}", mSeed);
                WriteLine("#pragma warning disable 0168");

                WriteLine();
                if (!namespaceStr.IsEmpty)
                {
                    WriteLine("namespace {0}", namespaceStr);
                    WriteLine("{");
                    mIndentCount++;
                }                
                String typeStr = scope String();
                if (typeDef.mIsStruct)
                    typeStr.Append("struct ");
                else
                    typeStr.Append("class ");
                typeStr.Append(typeDef.mName);
                WriteLine(typeStr);
                WriteLine("{");
                mIndentCount++;

                for (var fieldDef in typeDef.mFields)
                {
                    String str = scope String("public ");
                    if (fieldDef.mIsStatic)
                        str.Append("static ");
                    str.Append(fieldDef.mTypeDef.mName);
                    str.Append(" ");
                    str.Append(fieldDef.mName);
                    str.Append(";");
                    WriteLine(str);
                }
                
                for (int methodIdx = 0; methodIdx < typeDef.mMethods.Count; methodIdx++)
                {
                    WriteLine();
                    var methodDef = typeDef.mMethods[methodIdx];
                    mCurMethodDef = methodDef;                    
                    String str = scope String("public ");

					if (methodDef.mIsStatic)
						str.Append("static ");

                    str.Append(methodDef.mReturnType.mName);
                    str.Append(" ");

                    str.Append(methodDef.mName);
                    str.Append("(");
                    for (int paramIdx = 0; paramIdx < methodDef.mParamCount; paramIdx++)
                    {
                        if (paramIdx > 0)
                            str.Append(", ");

                        var paramDef = methodDef.mLocals[paramIdx];
                        str.Append(paramDef.mTypeDef.mName);
                        str.Append(" ");
                        str.Append(paramDef.mName);
                    }
                    str.Append(")");

					if (typeDef.mIsStruct)
						str.Append("mut ");

                    WriteLine(str);
                    mStartIndentCount = mIndentCount;
                    if (methodDef.mName == "Main")
                        GenerateMainBlock();
					else if (methodDef.mName == "Use")
                        GenerateUseBlock();
                    else
                        GenerateBlock();                    
                }

                mIndentCount--;
                WriteLine("}");
                if (!namespaceStr.IsEmpty)
                {
                    mIndentCount--;
                    WriteLine("}");
                }

                for (var namespaceDef in typeDef.mUsingNamespaces)
				{
					var namespaceName = scope String();
					namespaceDef.GetFullName(namespaceName);
                    streamWrite.WriteLine("using {0};", namespaceName);
				}
                streamWrite.Write(mQueuedText);
				mQueuedText.Clear();

                file.Close();

				if (!mVerbose)
					WriteProgress(@typeDef.Index / (float)mUserTypes.Count);
            }
			if (!mVerbose)
				Console.WriteLine("");

            Console.WriteLine("Types: {0} UnspecializedGenerics: {1} SpecializedGenerics: {2} Lines: {3}", mUserTypes.Count, unspecializedTypes, specializedTypes, mLineCount);
        }

		void HandleConfig(String configPath)
		{
			StructuredData sd = scope StructuredData();
			sd.Load(configPath);

			sd.Get("TypeCount", ref mConfig.mTypeCount);
			sd.Get("PrimitiveTypeChance", ref mConfig.mPrimitiveTypeChance);
			sd.Get("StructPct", ref mConfig.mStructPct);
			sd.Get("TypeComplexityPower", ref mConfig.mTypeComplexityPower);
			sd.Get("CreateGenericTypePct", ref mConfig.mCreateGenericTypePct);
			sd.Get("SpecializedTypePower", ref mConfig.mSpecializedTypePower);
			sd.Get("UnspecializedTypeScalar", ref mConfig.mUnspecializedTypeScalar);
			sd.Get("TypeDefPoolPower", ref mConfig.mTypeDefPoolPower);
			sd.Get("TypeDefPoolOffset", ref mConfig.mTypeDefPoolOffset);
			sd.Get("TypeDefPoolScalar", ref mConfig.mTypeDefPoolScalar);
			sd.Get("FieldCountPower", ref mConfig.mFieldCountPower);
			sd.Get("FieldCountScalar", ref mConfig.mFieldCountScalar);
			sd.Get("FieldStaticPct", ref mConfig.mFieldStaticPct);
			sd.Get("VoidReturnPct", ref mConfig.mVoidReturnPct);
			sd.Get("MethodCodeComplexityPower", ref mConfig.mMethodCodeComplexityPower);
			sd.Get("MethodLengthScalar", ref mConfig.mMethodLengthScalar);
			sd.Get("MethodCountScalar", ref mConfig.mMethodCountScalar);
			sd.Get("AssignMemberPct", ref mConfig.mAssignMemberPct);
			sd.Get("CreateLocalPct", ref mConfig.mCreateLocalPct);
			sd.Get("ParamCountPower", ref mConfig.mParamCountPower);
			sd.Get("ParamCountScalar", ref mConfig.mParamCountScalar);
			sd.Get("NewSpacespaceChance", ref mConfig.mNewSpacespaceChance);
			sd.Get("RootNamespaceChance", ref mConfig.mRootNamespaceChance);
		}

        static void Main(String[] args)
        {
			if (args.Count == 0)
			{
				Console.WriteLine("Usage: RandoCode <configFile>");
				return;
			}

			String cwd = scope String();
			Directory.GetCurrentDirectory(cwd);

            Program pg = new Program();
			pg.HandleConfig(args[0]);
            pg.Run();
			delete pg;
        }
    }
}
