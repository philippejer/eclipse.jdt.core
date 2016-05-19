package org.eclipse.jdt.core.internal.compiler.extensions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.core.extensions.ExtensionsConfig;
import org.eclipse.jdt.internal.compiler.ASTVisitor;
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.AllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.AnnotationMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.Argument;
import org.eclipse.jdt.internal.compiler.ast.ArrayAllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.ArrayInitializer;
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Clinit;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ExplicitConstructorCall;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.FalseLiteral;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ForeachStatement;
import org.eclipse.jdt.internal.compiler.ast.IntLiteral;
import org.eclipse.jdt.internal.compiler.ast.Invocation;
import org.eclipse.jdt.internal.compiler.ast.Literal;
import org.eclipse.jdt.internal.compiler.ast.LocalDeclaration;
import org.eclipse.jdt.internal.compiler.ast.MessageSend;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.NullLiteral;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedSingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedNameReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleNameReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.StringLiteral;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.ast.Wildcard;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.impl.BooleanConstant;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.impl.Constant;
import org.eclipse.jdt.internal.compiler.impl.IntConstant;
import org.eclipse.jdt.internal.compiler.impl.ReferenceContext;
import org.eclipse.jdt.internal.compiler.impl.StringConstant;
import org.eclipse.jdt.internal.compiler.lookup.AnnotationBinding;
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding;
import org.eclipse.jdt.internal.compiler.lookup.Binding;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;
import org.eclipse.jdt.internal.compiler.lookup.CaptureBinding;
import org.eclipse.jdt.internal.compiler.lookup.ClassScope;
import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope;
import org.eclipse.jdt.internal.compiler.lookup.ElementValuePair;
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding;
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding;
import org.eclipse.jdt.internal.compiler.lookup.MethodScope;
import org.eclipse.jdt.internal.compiler.lookup.ParameterizedTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.Scope;
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeIds;
import org.eclipse.jdt.internal.compiler.lookup.WildcardBinding;
import org.eclipse.jdt.internal.compiler.problem.DefaultProblem;
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities;
import org.eclipse.jdt.internal.compiler.util.Util;

/**
 * Big ugly class that contains all compiler extensions related code.
 * Much of the complex AST manipulation code comes from project Lombok.
 */
public class CompilerExtensions {	
	
	public static void addProblem(Scope scope, int sourceStart, int sourceEnd, String message, boolean isError) {
		ReferenceContext referenceContext = scope.referenceContext();
		CompilationResult result = referenceContext.compilationResult();
		int[] lineSeparators = result.getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(sourceStart, lineSeparators, 0, lineSeparators.length - 1);
		int columnNumber = Util.searchColumnNumber(lineSeparators, lineNumber, sourceStart);
		int severity = isError ? ProblemSeverities.Error : ProblemSeverities.Warning;
		DefaultProblem problem = new DefaultProblem(
				result.fileName, message, 0, new String[0], severity, sourceStart, sourceEnd, lineNumber, columnNumber);
		result.record(problem, referenceContext);
	}

	public static long[] copy(long[] array) {
		return array == null ? null : array.clone();
	}
	
	public static long pos(int sourceStart, int sourceEnd) {
		return ((long) sourceStart << 32) | (sourceEnd & 0xFFFFFFFFL);
	}
	
	public static long pos(ASTNode node) {
		return pos(node.sourceStart, node.sourceEnd);
	}
	
	public static long[] poss(int sourceStart, int sourceEnd, int repeat) {
		long p = pos(sourceStart, sourceEnd);
		long[] out = new long[repeat];
		Arrays.fill(out, p);
		return out;
	}
	
	public static long[] poss(ASTNode node, int repeat) {
		return poss(node.sourceStart, node.sourceEnd, repeat);
	}
	
	public static TypeReference makeType(TypeBinding binding, ASTNode pos, boolean allowCompound) {
		int dims = binding.dimensions();
		binding = binding.leafComponentType();
		
		// Primitives
		
		char[] base = null;
		
		switch (binding.id) {
		case TypeIds.T_int:
			base = TypeConstants.INT;
			break;
		case TypeIds.T_long:
			base = TypeConstants.LONG;
			break;
		case TypeIds.T_short:
			base = TypeConstants.SHORT;
			break;
		case TypeIds.T_byte:
			base = TypeConstants.BYTE;
			break;
		case TypeIds.T_double:
			base = TypeConstants.DOUBLE;
			break;
		case TypeIds.T_float:
			base = TypeConstants.FLOAT;
			break;
		case TypeIds.T_boolean:
			base = TypeConstants.BOOLEAN;
			break;
		case TypeIds.T_char:
			base = TypeConstants.CHAR;
			break;
		case TypeIds.T_void:
			base = TypeConstants.VOID;
			break;
		case TypeIds.T_null:
			return null;
		}
		
		if (base != null) {
			if (dims > 0) {
				TypeReference result = new ArrayTypeReference(base, dims, pos(pos));
				return result;
			}
			TypeReference result = new SingleTypeReference(base, pos(pos));
			return result;
		}
		
		if (binding.isAnonymousType()) {
			ReferenceBinding ref = (ReferenceBinding)binding;
			ReferenceBinding[] supers = ref.superInterfaces();
			if (supers == null || supers.length == 0) supers = new ReferenceBinding[] {ref.superclass()};
			if (supers[0] == null) {
				TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				return result;
			}
			return makeType(supers[0], pos, false);
		}
		
		if (binding instanceof CaptureBinding) {
			return makeType(((CaptureBinding)binding).wildcard, pos, allowCompound);
		}
		
		if (binding.isUnboundWildcard()) {
			if (!allowCompound) {
				TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				return result;
			} else {
				Wildcard out = new Wildcard(Wildcard.UNBOUND);
				out.sourceStart = pos.sourceStart;
				out.sourceEnd = pos.sourceEnd;
				return out;
			}
		}
		
		if (binding.isWildcard()) {
			WildcardBinding wildcard = (WildcardBinding) binding;
			if (wildcard.boundKind == Wildcard.EXTENDS) {
				if (!allowCompound) {
					return makeType(wildcard.bound, pos, false);
				} else {
					Wildcard out = new Wildcard(Wildcard.EXTENDS);
					out.bound = makeType(wildcard.bound, pos, false);
					out.sourceStart = pos.sourceStart;
					out.sourceEnd = pos.sourceEnd;
					return out;
				}
			} else if (allowCompound && wildcard.boundKind == Wildcard.SUPER) {
				Wildcard out = new Wildcard(Wildcard.SUPER);
				out.bound = makeType(wildcard.bound, pos, false);
				out.sourceStart = pos.sourceStart;
				out.sourceEnd = pos.sourceEnd;
				return out;
			} else {
				TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				return result;
			}
		}
		
		// Keep moving up via 'binding.enclosingType()' and gather generics from each binding. We stop after a local type, or a static type, or a top-level type.
		// Finally, add however many nullTypeArgument[] arrays as that are missing, inverse the list, toArray it, and use that as PTR's typeArgument argument.
		
		List<TypeReference[]> params = new ArrayList<TypeReference[]>();
		/* Calculate generics */ {
			TypeBinding b = binding;
			while (true) {
				boolean isFinalStop = b.isLocalType() || !b.isMemberType() || b.enclosingType() == null;
				
				TypeReference[] tyParams = null;
				if (b instanceof ParameterizedTypeBinding) {
					ParameterizedTypeBinding paramized = (ParameterizedTypeBinding) b;
					if (paramized.arguments != null) {
						tyParams = new TypeReference[paramized.arguments.length];
						for (int i = 0; i < tyParams.length; i++) {
							tyParams[i] = makeType(paramized.arguments[i], pos, true);
						}
					}
				}
				
				params.add(tyParams);
				if (isFinalStop) break;
				b = b.enclosingType();
			}
		}
		
		char[][] parts;
		
		if (binding.isTypeVariable()) {
			parts = new char[][] { binding.shortReadableName() };
		} else if (binding.isLocalType()) {
			parts = new char[][] { binding.sourceName() };
		} else {
			String[] pkg = new String(binding.qualifiedPackageName()).split("\\."); //$NON-NLS-1$
			String[] name = new String(binding.qualifiedSourceName()).split("\\."); //$NON-NLS-1$
			if (pkg.length == 1 && pkg[0].isEmpty()) pkg = new String[0];
			parts = new char[pkg.length + name.length][];
			int ptr;
			for (ptr = 0; ptr < pkg.length; ptr++) parts[ptr] = pkg[ptr].toCharArray();
			for (; ptr < pkg.length + name.length; ptr++) parts[ptr] = name[ptr - pkg.length].toCharArray();
		}
		
		while (params.size() < parts.length) params.add(null);
		Collections.reverse(params);
		
		boolean isParamized = false;
		
		for (TypeReference[] tyParams : params) {
			if (tyParams != null) {
				isParamized = true;
				break;
			}
		}
		if (isParamized) {
			if (parts.length > 1) {
				TypeReference[][] typeArguments = params.toArray(new TypeReference[0][]);
				TypeReference result = new ParameterizedQualifiedTypeReference(parts, typeArguments, dims, poss(pos, parts.length));
				return result;
			}
			TypeReference result = new ParameterizedSingleTypeReference(parts[0], params.get(0), dims, pos(pos));
			return result;
		}
		
		if (dims > 0) {
			if (parts.length > 1) {
				TypeReference result = new ArrayQualifiedTypeReference(parts, dims, poss(pos, parts.length));
				return result;
			}
			TypeReference result = new ArrayTypeReference(parts[0], dims, pos(pos));
			return result;
		}
		
		if (parts.length > 1) {
			TypeReference result = new QualifiedTypeReference(parts, poss(pos, parts.length));
			return result;
		}
		TypeReference result = new SingleTypeReference(parts[0], pos(pos));
		return result;
	}
	
	public static TypeBinding getForEachComponentType(Expression collection, BlockScope scope) {
		if (collection != null) {
			TypeBinding resolved = collection.resolvedType;
			if (resolved == null) resolved = resolveTypeLazy(collection, scope);
			if (resolved == null) return null;
			if (resolved.isArrayType()) {
				resolved = ((ArrayBinding) resolved).elementsType();
				return resolved;
			} else if (resolved instanceof ReferenceBinding) {
				ReferenceBinding iterableType = ((ReferenceBinding)resolved).findSuperTypeOriginatingFrom(TypeIds.T_JavaLangIterable, false);
				
				TypeBinding[] arguments = null;
				if (iterableType != null) switch (iterableType.kind()) {
					case Binding.GENERIC_TYPE : // for (T t : Iterable<T>) - in case used inside Iterable itself
						arguments = iterableType.typeVariables();
						break;
					case Binding.PARAMETERIZED_TYPE : // for(E e : Iterable<E>)
						arguments = ((ParameterizedTypeBinding)iterableType).arguments;
						break;
					case Binding.RAW_TYPE : // for(Object e : Iterable)
						return null;
				}
				
				if (arguments != null && arguments.length == 1) {
					return arguments[0];
				}
			}
		}
		
		return null;
	}

	public static boolean matches(char[] name1, char[] name2) {
		if ((name1 == null) || (name2 == null) || (name1.length != name2.length))
			return false;
		for (int i = 0; i < name1.length; i++) {
			if (name1[i] != name2[i])
				return false;
		}

		return true;
	}

	public static boolean matches(char[][] name1, char[][] name2) {
		if ((name1 == null) || (name2 == null) || (name1.length != name2.length))
			return false;
		for (int i = 0; i < name1.length; i++) {
			if (!matches(name1[i], name2[i]))
				return false;
		}

		return true;
	}
	
	public static boolean isSingleTypeReference(TypeReference ref, char[] name) {
		if (!(ref instanceof SingleTypeReference)) return false;
		return matches(((SingleTypeReference) ref).token, name);
	}
	
	public static final char[] VAR_NAME = {'v', 'a', 'r'};

	public static boolean isVar(TypeReference ref) {
		return isSingleTypeReference(ref, VAR_NAME);
	}
	
	public static TypeBinding resolveTypeLazy(Expression expression, BlockScope scope) {
		if (expression.resolvedType != null) return expression.resolvedType;
		try {
			return expression.resolveType(scope);
		} catch (NullPointerException e) {
			// may happen if the expression contains an unresolved var reference
			return null;
		} catch (ArrayIndexOutOfBoundsException e) {
			// may happen if the expression contains an unresolved var reference
			return null;
		}
	}
	
	public static void handleVarResolveLocalDeclaration(LocalDeclaration declaration, BlockScope scope) {
		ExtensionsConfig.log("handleVarResolveLocalDeclaration: declaration: " + ExtensionsConfig.asLog(declaration)); //$NON-NLS-1$
		if (isVar(declaration.type)) {
			try {
				TypeBinding binding;
				Expression initialization = declaration.initialization;
				if (initialization == null) {
					initialization = declaration.initializationCopy;
				}				
				if (initialization != null) {
					binding = resolveTypeLazy(initialization, scope);
				} else {
					binding = getForEachComponentType(declaration.collectionCopy, scope);
				}
				TypeReference type = makeType(binding, declaration.type, false);		
				declaration.type = type;
			} catch (NullPointerException e) {
				// may happen with the selection parser (initialization not parsed)
				ExtensionsConfig.log("Cannot resolve initialization"); //$NON-NLS-1$
				if (ExtensionsConfig.EnableLogs) e.printStackTrace();
				TypeReference newType = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(declaration.type, 3));	
				declaration.type = newType;
			}
			ExtensionsConfig.log("Type inferred: " + declaration.type); //$NON-NLS-1$
		}
	}
	
	public static void handleVarResolveForEach(ForeachStatement statement, BlockScope scope) {
		ExtensionsConfig.log("handleVarResolveForEach: statement: " + ExtensionsConfig.asLog(statement)); //$NON-NLS-1$
		if ((statement.elementVariable != null) && isVar(statement.elementVariable.type)) {
			try {
				TypeBinding binding = getForEachComponentType(statement.collection, scope);
				TypeReference type = makeType(binding, statement.elementVariable.type, false);
				statement.elementVariable.type = type;
			} catch (NullPointerException e) {
				// may happen with the selection parser (initialization not parsed)
				ExtensionsConfig.log("Cannot resolve collection"); //$NON-NLS-1$
				TypeReference newType = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(statement.elementVariable.type, 3));	
				statement.elementVariable.type = newType;
			}
			ExtensionsConfig.log("Type inferred: " + statement.elementVariable.type); //$NON-NLS-1$
		}
	}
	
	public static final int AccPublicPrivateProctected = 0x0007;
	
	public static int setPublicByDefault(int modifiers) {
		if ((modifiers & AccPublicPrivateProctected) == 0) {
			modifiers |= ClassFileConstants.AccPublic;
		}
		return modifiers;
	}
	
	public static void handlePublicByDefaultForUnit(CompilationUnitDeclaration unit) {
		ExtensionsConfig.log("handlePublicByDefaultForUnit: unit: " + ExtensionsConfig.asLog(unit)); //$NON-NLS-1$
		unit.traverse(new ASTVisitor() {
			@Override
			public boolean visit(AnnotationMethodDeclaration annotationTypeDeclaration, ClassScope classScope) {
				// not needed
				return super.visit(annotationTypeDeclaration, classScope);
			}

			@Override
			public boolean visit(Clinit clinit, ClassScope scope) {
				// not needed
				return super.visit(clinit, scope);
			}

			@Override
			public boolean visit(ConstructorDeclaration constructorDeclaration, ClassScope scope) {
				constructorDeclaration.modifiers = setPublicByDefault(constructorDeclaration.modifiers);
				return super.visit(constructorDeclaration, scope);
			}

			@Override
			public boolean visit(FieldDeclaration fieldDeclaration, MethodScope scope) {
				if (fieldDeclaration.type != null) { // public modifier not allowed for enums
					fieldDeclaration.modifiers = setPublicByDefault(fieldDeclaration.modifiers);
				}
				return super.visit(fieldDeclaration, scope);
			}

			@Override
			public boolean visit(MethodDeclaration methodDeclaration, ClassScope scope) {
				methodDeclaration.modifiers = setPublicByDefault(methodDeclaration.modifiers);
				return super.visit(methodDeclaration, scope);
			}

			@Override
			public boolean visit(TypeDeclaration localTypeDeclaration, BlockScope scope) {
				// not allowed on local types
				return super.visit(localTypeDeclaration, scope);
			}

			@Override
			public boolean visit(TypeDeclaration memberTypeDeclaration, ClassScope scope) {
				memberTypeDeclaration.modifiers = setPublicByDefault(memberTypeDeclaration.modifiers);
				return super.visit(memberTypeDeclaration, scope);
			}

			@Override
			public boolean visit(TypeDeclaration typeDeclaration, CompilationUnitScope scope) {
				typeDeclaration.modifiers = setPublicByDefault(typeDeclaration.modifiers);
				return super.visit(typeDeclaration, scope);
			}
		}, unit.scope);
	}
	
	public static void handleEndParse(CompilationUnitDeclaration unit, CompilerOptions options) {
		if (options.publicByDefault) {
			handlePublicByDefaultForUnit(unit);
		}
	}
	
	public static char[][] buildName(char[][] base, char[] ...names) {
		char[][] result = new char[base.length + names.length][];
		System.arraycopy(base, 0, result, 0, base.length);
		System.arraycopy(names, 0, result, base.length, names.length);
		return result;
	}
	
	public static char[] asChars(String string) {
		return string.toCharArray();
	}
	
	public static char[][] asChars(String ...strings) {
		char[][] result = new char[strings.length][];
		for (int i = 0; i < strings.length; i++) {
			result[i] = asChars(strings[i]);
		}
		return result;
	}
	
	public static String asString(char[] chars) {
		return new String(chars);
	}
	
	public static final char[][] EXTENSIONS_NAME = asChars("org", "eclipse", "jdt", "annotation", "extensions"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
	public static final char[][] CONDITIONAL_NAME = buildName(EXTENSIONS_NAME, asChars("Conditional")); //$NON-NLS-1$
	public static final char[][] CONDITIONAL_CALLER_FIELD_NAME = buildName(EXTENSIONS_NAME, asChars("ConditionalCallerField")); //$NON-NLS-1$
	public static final char[][] OPTIONAL_NAME = buildName(EXTENSIONS_NAME, asChars("Optional")); //$NON-NLS-1$
	public static final char[][] CALLER_FILE_NAME = buildName(EXTENSIONS_NAME, asChars("CallerFile")); //$NON-NLS-1$
	public static final char[][] CALLER_LINE_NAME = buildName(EXTENSIONS_NAME, asChars("CallerLine")); //$NON-NLS-1$
	public static final char[][] CALLER_CLASS_NAME = buildName(EXTENSIONS_NAME, asChars("CallerClass")); //$NON-NLS-1$
	public static final char[][] CALLER_METHOD_NAME = buildName(EXTENSIONS_NAME, asChars("CallerMethod")); //$NON-NLS-1$
	public static final char[][] ARG_NAME_NAME = buildName(EXTENSIONS_NAME, asChars("ArgName")); //$NON-NLS-1$
	public static final char[][] ARG_NAMES_NAME = buildName(EXTENSIONS_NAME, asChars("ArgNames")); //$NON-NLS-1$		
	public static final char[][][] SYNTHETIC_NAMES = {CALLER_FILE_NAME, CALLER_LINE_NAME, CALLER_CLASS_NAME, CALLER_METHOD_NAME,
			ARG_NAME_NAME, ARG_NAMES_NAME };
	public static final char[] VALUE_NAME = asChars("value"); //$NON-NLS-1$
	
	public static boolean isSyntheticParameterAnnotation(AnnotationBinding annotation) {
		ReferenceBinding type = annotation.getAnnotationType();
		for (int i = 0; i < SYNTHETIC_NAMES.length; i++) {
			if (matches(SYNTHETIC_NAMES[i], type.compoundName))
				return true;
		}
		return false;
	}
	
	public static boolean isConditionalAnnotation(AnnotationBinding annotation) {
		return matches(CONDITIONAL_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isConditionalCallerFieldAnnotation(AnnotationBinding annotation) {
		return matches(CONDITIONAL_CALLER_FIELD_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isOptionalAnnotation(AnnotationBinding annotation) {
		return matches(OPTIONAL_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallFileAnnotation(AnnotationBinding annotation) {
		return matches(CALLER_FILE_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallLineAnnotation(AnnotationBinding annotation) {
		return matches(CALLER_LINE_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallClassAnnotation(AnnotationBinding annotation) {
		return matches(CALLER_CLASS_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallMethodAnnotation(AnnotationBinding annotation) {
		return matches(CALLER_METHOD_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isArgNameAnnotation(AnnotationBinding annotation) {
		return matches(ARG_NAME_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isArgNamesAnnotation(AnnotationBinding annotation) {
		return matches(ARG_NAMES_NAME, annotation.getAnnotationType().compoundName);
	}
	
	public static char[] extractChars(char[] source, int start, int end) {
		if ((source == null) || (start >= source.length) || (end >= source.length) || (end < start)) return new char[0];
		char[] result = new char[end - start + 1];
		System.arraycopy(source, start, result, 0, end - start + 1);
		return result;
	}
	
	// lazily counts and stores synthetic annotations for method parameters
	public static void initSyntheticParameterAnnotations(MethodBinding method) {
		method = method.original();
		if (method.numSyntheticAnnotations >= 0) return; // already done
		method.numSyntheticAnnotations = 0;
		AnnotationBinding[][] parameterAnnotations = method.getParameterAnnotations();
		if (parameterAnnotations == null) return;
		method.syntheticAnnotations = new AnnotationBinding[parameterAnnotations.length];
		for (int i = 0; i < parameterAnnotations.length; i++) {
			AnnotationBinding[] annotations = parameterAnnotations[i];
			AnnotationBinding annotation = null;
			for (int j = 0; j < annotations.length; j++) {
				if (isSyntheticParameterAnnotation(annotations[j])) {
					annotation = annotations[j];
					break;
				}
			}
			if (annotation != null) {
				method.numSyntheticAnnotations += 1;
				method.syntheticAnnotations[i] = annotation;
			}
		}
	}
	
	// adds any synthetic argument types (for compatibility checks)
	public static TypeBinding[] handleSyntheticParametersForCompatibility(MethodBinding method, TypeBinding[] arguments) {
		method = method.original();
		if (arguments == null) arguments = new TypeBinding[0];
		TypeBinding[] parameters = method.parameters;
		if (parameters == null) return arguments;
		initSyntheticParameterAnnotations(method);		
		int numSyntheticAnnotations = method.numSyntheticAnnotations;
		if (numSyntheticAnnotations == 0) return arguments;
		ExtensionsConfig.log("handleSyntheticParametersForCompatibility: method: " + asString(method.selector) + ": adding " +  //$NON-NLS-1$//$NON-NLS-2$
				numSyntheticAnnotations + " synthetic arguments"); //$NON-NLS-1$ 
		AnnotationBinding[] syntheticAnnotations = method.syntheticAnnotations;
		TypeBinding[] result = new TypeBinding[arguments.length + numSyntheticAnnotations];
		for (int i = 0, j = 0; i < result.length; i++) {
			if ((i >= syntheticAnnotations.length) || (syntheticAnnotations[i] == null)) {
				if (j >= arguments.length) return arguments; // not enough arguments
				result[i] = arguments[j++]; // append the original argument type		
			} else {
				result[i] = parameters[i]; // append the synthetic argument type
			}
		}
		return result;
	}
	
	// lazily counts and stores optional annotations for method parameters
	public static void initOptionalParameterAnnotations(MethodBinding method) {
		method = method.original();
		if (method.numOptionalAnnotations >= 0) return; // already done
		method.numOptionalAnnotations = 0;
		AnnotationBinding[][] parameterAnnotations = method.getParameterAnnotations();
		if (parameterAnnotations == null) return;
		method.optionalAnnotations = new AnnotationBinding[parameterAnnotations.length];
		for (int i = parameterAnnotations.length - 1; i >= 0; i--) { // start from the end
			AnnotationBinding[] annotations = parameterAnnotations[i];
			AnnotationBinding annotation = null;
			for (int j = 0; j < annotations.length; j++) {
				if (isOptionalAnnotation(annotations[j])) {
					annotation = annotations[j];
					break;
				}
			}
			if (annotation == null) break; // ignore further annotations
			method.numOptionalAnnotations += 1;
			method.optionalAnnotations[i] = annotation;
		}
	}
	
	// adds any missing optional argument types (for compatibility checks)
	public static TypeBinding[] handleOptionalParametersForCompatibility(MethodBinding method, TypeBinding[] arguments) {
		method = method.original();
		if (arguments == null) arguments = new TypeBinding[0];
		TypeBinding[] parameters = method.parameters;
		if (parameters == null) return arguments;
		initOptionalParameterAnnotations(method);		
		int numOptionalAnnotations = method.numOptionalAnnotations;
		if (numOptionalAnnotations == 0) return arguments;
		int numMissingArguments = parameters.length - arguments.length;
		if ((numMissingArguments <= 0) || (numMissingArguments > numOptionalAnnotations)) return arguments;
		ExtensionsConfig.log("handleOptionalParametersForCompatibility: method: " + asString(method.selector) + ": adding " +  //$NON-NLS-1$//$NON-NLS-2$
				numMissingArguments + " missing optional arguments"); //$NON-NLS-1$ 
		TypeBinding[] result = new TypeBinding[parameters.length];
		System.arraycopy(arguments, 0, result, 0, arguments.length);
		System.arraycopy(parameters, arguments.length, result, arguments.length, numMissingArguments);
		return result;
	}
	
	public static char[] handleSyntheticParametersForSignature(MethodBinding method, char[] methodSignature) {
		method = method.original();
		initSyntheticParameterAnnotations(method);
		int numSyntheticAnnotations = method.numSyntheticAnnotations;
		if (numSyntheticAnnotations == 0) return methodSignature;
		char[][] parameterTypes = Signature.getParameterTypes(methodSignature);
		int length = parameterTypes.length;
		char[][] newParameterTypes = new char[length - numSyntheticAnnotations][];
		for (int i = 0, j = 0; i < length; i++) {
			if (method.syntheticAnnotations[i] == null) {
				newParameterTypes[j++] = parameterTypes[i];
			}
		}
		char[] newMethodSignature = Signature.createMethodSignature(newParameterTypes, Signature.getReturnType(methodSignature));
		ExtensionsConfig.log("handleSyntheticParametersForSignature: old signature: " + asString(methodSignature) + //$NON-NLS-1$
				", new signature: " + asString(newMethodSignature)); //$NON-NLS-1$
		return newMethodSignature;
	}
	
	public static class ParameterNames {
		public char[][] parameterPackageNames;
		public char[][] parameterTypeNames;
		public char[][] parameterNames;
		
		public ParameterNames(char[][] parameterPackageNames, char[][] parameterTypeNames, char[][] parameterNames) {
			this.parameterPackageNames = parameterPackageNames;
			this.parameterTypeNames = parameterTypeNames;
			this.parameterNames = parameterNames;
		}
	}
	
	public static void handleSyntheticParametersForCompletion(MethodBinding method, ParameterNames names, int initialLength) {
		initSyntheticParameterAnnotations(method);		
		int numSyntheticAnnotations = method.numSyntheticAnnotations;
		if (numSyntheticAnnotations == 0) return;
		int newLength = initialLength - numSyntheticAnnotations;
		char[][] newParameterPackageNames = new char[newLength][];
		char[][] newParameterTypeNames = new char[newLength][];
		char[][] newParameterNames = new char[newLength][];
		for (int i = 0, j = 0; i < initialLength; i++) {
			if (method.syntheticAnnotations[i] == null) {
				newParameterPackageNames[j] = names.parameterPackageNames[i];
				newParameterTypeNames[j] = names.parameterTypeNames[i];
				newParameterNames[j++] = names.parameterNames[i];
			}
		}
		names.parameterPackageNames = newParameterPackageNames;
		names.parameterTypeNames = newParameterTypeNames;
		names.parameterNames = newParameterNames;
	}
	
	public static boolean handleOptionalParametersForCompletion(MethodBinding method) {		
		CompilerExtensions.initOptionalParameterAnnotations(method);
		if (method.numOptionalAnnotations == 0) return false;
		if (method.savedParameters == null) method.savedParameters = method.parameters;
		if ((method.parameters.length > 0) &&
				(method.optionalAnnotations[method.parameters.length - 1] != null)) {
			method.parameters = Arrays.copyOf(method.parameters, method.parameters.length - 1);
			return true;
		}
		method.parameters = method.savedParameters;
		method.savedParameters = null;
		return false;
	}
	
	public static final char[] ZeroChars = {'0'};
	
	public static Literal makeDefaultLiteral(TypeBinding type, int s, int e) {
		switch (type.id) {
			case TypeIds.T_int:
			case TypeIds.T_short:
			case TypeIds.T_byte:
			case TypeIds.T_char:
			case TypeIds.T_long:
			case TypeIds.T_double:
			case TypeIds.T_float:
				return IntLiteral.buildIntLiteral(ZeroChars, s, e);
			case TypeIds.T_boolean:
				return new FalseLiteral(s, e);
		}
		return new NullLiteral(s, e);
	}
	
	public static Expression makeCallerFile(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		return new StringLiteral(compilationUnit.getFileName(), position, position, 0);
	}
	
	public static Expression makeCallerLineExpression(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		return IntLiteral.buildIntLiteral(asChars(Integer.toString(lineNumber)), position, position);
	}
	
	public static Expression makeCallerClassExpression(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		return new StringLiteral(classScope.referenceContext.name, position, position, 0);
	}
	
	public static Expression makeCallerMethodExpression(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		MethodScope methodScope = scope.methodScope();
		if (methodScope == null) return new NullLiteral(position, position);
		AbstractMethodDeclaration declaration = methodScope.referenceMethod();
		if (declaration == null) return new NullLiteral(position, position);
		return new StringLiteral(declaration.selector, position, position, 0);
	}
	
	public static Expression findArgNameExpression(Expression expression, Scope scope) {
		if (expression instanceof SingleNameReference) {
			return expression;
		} else if (expression instanceof QualifiedNameReference) {
			QualifiedNameReference reference = (QualifiedNameReference) expression;
			return new SingleNameReference(reference.tokens[reference.tokens.length - 1],
					reference.sourcePositions[reference.sourcePositions.length - 1]);
		} else if (expression instanceof Invocation) {
			Invocation invocation = (Invocation) expression;
			Expression[] arguments = invocation.arguments();
			if (arguments == null) return null;
			for (Expression argument: arguments) {
				Expression foundExpression = findArgNameExpression(argument, scope);
				if (foundExpression != null) return foundExpression;
			}
		}
		return null;
	}
	
	public static Expression makeArgNameExpression(Invocation invocation, Scope scope, int index) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		Expression[] arguments = invocation.arguments();
		if (arguments == null) return new NullLiteral(position, position);
		if ((index < 0) || (index >= arguments.length)) return new NullLiteral(position, position);
		Expression argument = arguments[index];
		if (argument == null) return new NullLiteral(position, position);
		Expression foundArgument = findArgNameExpression(argument, scope);
		if (foundArgument == null) foundArgument = argument;
		char[] contents = compilationUnit.compilationResult().compilationUnit.getContents();
		char[] argumentChars = extractChars(contents, foundArgument.sourceStart, foundArgument.sourceEnd);
		return new StringLiteral(argumentChars, position, position, 0);		
	}
	
	public static Expression makeArgNamesExpression(Invocation invocation, Scope scope, int index) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		Expression[] arguments = invocation.arguments();
		ArrayAllocationExpression result = new ArrayAllocationExpression();
		result.type = new QualifiedTypeReference(TypeConstants.JAVA_LANG_STRING, poss(position, position, 3));
		result.dimensions = new Expression[1];
		result.initializer = new ArrayInitializer();
		if (arguments == null) {
			result.initializer.expressions = new Expression[0];
		} else {
			result.initializer.expressions = new Expression[arguments.length - index];
			for (int i = index; i < arguments.length; i++) {
				Expression argument = arguments[i];
				Expression foundArgument = findArgNameExpression(argument, scope);
				if (foundArgument == null) foundArgument = argument;
				char[] contents = compilationUnit.compilationResult().compilationUnit.getContents();
				char[] argumentChars = extractChars(contents, foundArgument.sourceStart, foundArgument.sourceEnd);
				result.initializer.expressions[i - index] = new StringLiteral(argumentChars, position, position, 0);		
			}
		}
		return result;
	}
	
	public static Object getAnnotationParam(AnnotationBinding annotation, char[] name) {		
		ElementValuePair[] pairs = annotation.getElementValuePairs();
		for (ElementValuePair pair: pairs) {
			if (matches(name, pair.getName())) {
				Object value = pair.getValue();
				return value;
			}
		}
		return null;
	}
	
	public static boolean getBooleanAnnotationParam(AnnotationBinding annotation, char[] name) {
		Object value = getAnnotationParam(annotation, name);
		if (value instanceof BooleanConstant) return ((BooleanConstant) value).booleanValue();
		return false;
	}
	
	public static String getStringAnnotationParam(AnnotationBinding annotation, char[] name) {
		Object value = getAnnotationParam(annotation, name);
		if (value instanceof StringConstant) return ((StringConstant) value).stringValue();
		return null;
	}
	
	public static int getIntAnnotationParam(AnnotationBinding annotation, char[] name) {
		Object value = getAnnotationParam(annotation, name);
		if (value instanceof IntConstant) return ((IntConstant) value).intValue();
		return 0;
	}
	
	public static class InvocationArguments {
		Expression[] arguments;
		TypeBinding[] argumentTypes;
		
		public InvocationArguments(Expression[] arguments, TypeBinding[] argumentTypes) {
			this.arguments = arguments;
			this.argumentTypes = argumentTypes;
		}
	}
	
	public static TypeBinding resolveType(Expression expression, Scope scope) {
		if (scope instanceof BlockScope) {
			return expression.resolveType((BlockScope) scope);
		} else if (scope instanceof ClassScope) {
			return expression.resolveType((ClassScope) scope);
		}
		return null;
	}
	
	// adds synthetic arguments required to invoke the method
	public static void handleSyntheticArgumentsForInvocation(Invocation invocation, InvocationArguments invocationArguments, MethodBinding method, Scope scope) {
		if (!method.isValidBinding()) return;
		method = method.original();
		TypeBinding[] parameters = method.parameters;
		Expression[] oldArguments = invocationArguments.arguments;
		if (oldArguments == null) oldArguments = new Expression[0];
		TypeBinding[] oldArgumentTypes = invocationArguments.argumentTypes;
		if (oldArgumentTypes == null) oldArgumentTypes = new TypeBinding[0];
		if (parameters == null) return;
		initSyntheticParameterAnnotations(method);
		int numSyntheticAnnotations = method.numSyntheticAnnotations;
		if (numSyntheticAnnotations == 0) return;
		ExtensionsConfig.log("handleSyntheticArgumentsForInvocation: method: " + asString(method.selector) + ": adding " + //$NON-NLS-1$ //$NON-NLS-2$
				numSyntheticAnnotations + " synthetic arguments"); //$NON-NLS-1$
		AnnotationBinding[] syntheticAnnotations = method.syntheticAnnotations;
		Expression[] newArguments = new Expression[oldArguments.length + numSyntheticAnnotations];
		TypeBinding[] newArgumentTypes = new TypeBinding[oldArgumentTypes.length + numSyntheticAnnotations];
		for (int i = 0, j = 0; i < newArguments.length; i++) {
			if ((i >= syntheticAnnotations.length) || (syntheticAnnotations[i] == null)) {
				newArguments[i] = oldArguments[j];
				newArgumentTypes[i] = oldArgumentTypes[j++];
			} else {
 				AnnotationBinding annotation = syntheticAnnotations[i];
				if (isCallFileAnnotation(annotation)) {
					newArguments[i] = makeCallerFile(invocation, scope);
				} else if (isCallLineAnnotation(annotation)) {
					newArguments[i] = makeCallerLineExpression(invocation, scope);
				} else if (isCallClassAnnotation(annotation)) {
					newArguments[i] = makeCallerClassExpression(invocation, scope);
				} else if (isCallMethodAnnotation(annotation)) {
					newArguments[i] = makeCallerMethodExpression(invocation, scope);
				} else if (isArgNameAnnotation(annotation)) {
					newArguments[i] = makeArgNameExpression(invocation, scope, j);
				} else if (isArgNamesAnnotation(annotation)) {
					newArguments[i] = makeArgNamesExpression(invocation, scope, j);
				} else {
					newArguments[i] = makeDefaultLiteral(method.parameters[i], invocation.sourceEnd(), invocation.sourceEnd());
				}
				newArgumentTypes[i] = resolveType(newArguments[i], scope);
				TypeBinding parameterType = method.parameters[i];
				if ((newArgumentTypes[i] == null) || !parameterType.isCompatibleWith(newArgumentTypes[i], scope)) {
					newArguments[i] = makeDefaultLiteral(method.parameters[i], invocation.sourceEnd(), invocation.sourceEnd());
					newArgumentTypes[i] = resolveType(newArguments[i], scope);
				}
			}
		}
		invocationArguments.arguments = newArguments;
		invocationArguments.argumentTypes = newArgumentTypes;
	}
	
	// adds synthetic arguments required to invoke the method
	public static void handleSyntheticArgumentsForMessageSend(MessageSend invocation, Scope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		handleSyntheticArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope);		
		invocation.arguments = invocationArguments.arguments;
		invocation.argumentTypes = invocationArguments.argumentTypes;		
	}
	
	// adds synthetic arguments required to invoke the method
	public static void handleSyntheticArgumentsForAllocation(AllocationExpression invocation, Scope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		handleSyntheticArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope);		
		invocation.arguments = invocationArguments.arguments;
		invocation.argumentTypes = invocationArguments.argumentTypes;
	}
	
	// adds synthetic arguments required to invoke the method
	public static TypeBinding[] handleSyntheticArgumentsForConstructorCall(ExplicitConstructorCall invocation, Scope scope, TypeBinding[] argumentTypes) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, argumentTypes);
		handleSyntheticArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope);			
		invocation.arguments = invocationArguments.arguments;
		return invocationArguments.argumentTypes;
	}
	
	public static void checkSyntheticAnnotations(AbstractMethodDeclaration declaration, Scope scope) {
		Argument[] arguments = declaration.arguments;
		if (arguments == null) return;
		for (int i = 0; i < arguments.length; i++) {
			Argument argument = arguments[i];
			if ((argument.annotations == null) || (argument.type == null)) continue;
			TypeBinding argumentType = argument.type.resolvedType;
			if (argumentType == null) continue;
			for (int j = 0; j < argument.annotations.length; j++) {
				Annotation annotation = argument.annotations[j];
				AnnotationBinding annotationType = annotation.getCompilerAnnotation();
				if (annotationType == null) continue;
				if (isCallFileAnnotation(annotationType) || isCallClassAnnotation(annotationType) || isCallMethodAnnotation(annotationType)
						|| isArgNameAnnotation(annotationType)) {
					TypeBinding stringBinding = scope.getJavaLangString();
					if (!stringBinding.isCompatibleWith(argumentType)) {
						addProblem(scope, argument.type.sourceStart, argument.type.sourceEnd,
								"String expected", true); //$NON-NLS-1$
					}
				} else if (isCallLineAnnotation(annotationType)) {
					TypeBinding intBinding = TypeBinding.INT;
					if (!intBinding.isCompatibleWith(argumentType)) {
						addProblem(scope, argument.type.sourceStart, argument.type.sourceEnd,
								"int expected", true); //$NON-NLS-1$
					}
				} else if (isArgNamesAnnotation(annotationType)) {
					TypeBinding stringBinding = scope.getJavaLangString();
					TypeBinding arrayBinding = scope.createArrayType(stringBinding, 1);
					if (!arrayBinding.isCompatibleWith(argumentType)) {
						addProblem(scope, argument.type.sourceStart, argument.type.sourceEnd,
								"String[] expected", true); //$NON-NLS-1$
					}
				}
			}
		}
	}
	
	// adds non-provided optional arguments required to invoke the method
	public static void handleOptionalArgumentsForInvocation(Invocation invocation, InvocationArguments invocationArguments, MethodBinding method, Scope scope) {
		method = method.original();
		if (!method.isValidBinding()) return;
		TypeBinding[] parameters = method.parameters;
		Expression[] oldArguments = invocationArguments.arguments;
		if (oldArguments == null) oldArguments = new Expression[0];
		TypeBinding[] oldArgumentTypes = invocationArguments.argumentTypes;
		if (oldArgumentTypes == null) oldArgumentTypes = new TypeBinding[0];
		initOptionalParameterAnnotations(method);
		if ((parameters == null) || (oldArguments.length >= parameters.length)) return; // already have enough arguments
		int numMissingArguments = parameters.length - oldArguments.length;
		if (numMissingArguments > method.numOptionalAnnotations) return; // should not happen after compatibility checks
		ExtensionsConfig.log("handleOptionalArgumentsForInvocation: method: " + asString(method.selector) + ": adding " + //$NON-NLS-1$ //$NON-NLS-2$
				numMissingArguments + " missing optional arguments"); //$NON-NLS-1$ 
		Expression[] newArguments = new Expression[parameters.length];
		System.arraycopy(oldArguments, 0, newArguments, 0, oldArguments.length);
		TypeBinding[] newArgumentTypes = new TypeBinding[parameters.length];
		System.arraycopy(oldArgumentTypes, 0, newArgumentTypes, 0, oldArgumentTypes.length);
		for (int i = oldArguments.length; i < parameters.length; i++) {
			AnnotationBinding annotation = method.optionalAnnotations[i];
			if (annotation == null) return; // should not happen after compatibility checks
			newArguments[i] = makeDefaultLiteral(method.parameters[i], invocation.sourceEnd(), invocation.sourceEnd());
			newArgumentTypes[i] = resolveType(newArguments[i], scope);
			if (newArgumentTypes[i] == null) return; // cannot resolve a basic literal??
		}
		invocationArguments.arguments = newArguments;
		invocationArguments.argumentTypes = newArgumentTypes;
	}	
	
	// adds synthetic arguments required to invoke the method
	public static void handleOptionalArgumentsForMessageSend(MessageSend invocation, Scope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		handleOptionalArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope);		
		invocation.arguments = invocationArguments.arguments;
		invocation.argumentTypes = invocationArguments.argumentTypes;		
	}
	
	// adds synthetic arguments required to invoke the method
	public static void handleOptionalArgumentsForAllocation(AllocationExpression invocation, Scope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		handleOptionalArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope);		
		invocation.arguments = invocationArguments.arguments;
		invocation.argumentTypes = invocationArguments.argumentTypes;
	}
	
	// adds synthetic arguments required to invoke the method
	public static TypeBinding[] handleOptionalArgumentsForConstructorCall(ExplicitConstructorCall invocation, Scope scope, TypeBinding[] argumentTypes) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, argumentTypes);
		handleOptionalArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope);			
		invocation.arguments = invocationArguments.arguments;
		return invocationArguments.argumentTypes;
	}
	
	public static void checkOptionalAnnotations(AbstractMethodDeclaration declaration, Scope scope) {
		Argument[] arguments = declaration.arguments;
		if (arguments == null) return;
		boolean optionalAllowed = true;
		for (int i = arguments.length - 1; i >= 0; i--) {
			Argument argument = arguments[i];
			if (argument.annotations == null) {
				optionalAllowed = false;
				continue;
			}
			Annotation optionalAnnotation = null;
			for (int j = 0; j < argument.annotations.length; j++) {
				Annotation annotation = argument.annotations[j];
				AnnotationBinding annotationType = annotation.getCompilerAnnotation();
				if (annotationType == null) continue;
				if (isOptionalAnnotation(annotationType)) {
					optionalAnnotation = annotation;
					break;
				}
			}
			if (optionalAnnotation != null) {
				if (!optionalAllowed) {
					addProblem(scope, optionalAnnotation.sourceStart, optionalAnnotation.sourceEnd,
							"Optional arguments must appear last", true); //$NON-NLS-1$
					return;
				}
			} else {
				optionalAllowed = false;
			}
		}
	}
	
	public static boolean checkConditionalCallerFieldAnnotation(AnnotationBinding annotation, BlockScope scope) {
		String value = getStringAnnotationParam(annotation, VALUE_NAME);
		if (value == null) return false;
		char[] name = asChars(value);
		SourceTypeBinding callerType = scope.enclosingSourceType();
		if (callerType == null) return false;
		FieldBinding[] fields = callerType.fields();
		if (fields == null) return false;
		for (FieldBinding field: fields) {
			Constant constant = field.constant();
			if (!(constant instanceof BooleanConstant)) return false;
			if (!matches(field.name, name)) return false;
			return ((BooleanConstant) constant).booleanValue();
		}
		return false;
	}
	
	public static boolean isMessageSendEnabled(MethodBinding method, BlockScope scope) {
		AnnotationBinding[] annotations = method.getAnnotations();
		if (annotations == null) return true;
		boolean enabled = true;
		for (AnnotationBinding annotation: annotations) {
			if (isConditionalAnnotation(annotation)) {
				enabled = getBooleanAnnotationParam(annotation, VALUE_NAME);
				if (enabled) return true;
				// else check for other conditional annotations that might allow this call
			} else if (isConditionalCallerFieldAnnotation(annotation)) {
				enabled = checkConditionalCallerFieldAnnotation(annotation, scope);
				if (enabled) return true;
			}
		}
		return enabled;
	}
}
