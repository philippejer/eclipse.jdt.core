package org.eclipse.jdt.core.internal.compiler.extensions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.AllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration;
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
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.StringLiteral;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.ast.Wildcard;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.impl.BooleanConstant;
import org.eclipse.jdt.internal.compiler.impl.IntConstant;
import org.eclipse.jdt.internal.compiler.impl.StringConstant;
import org.eclipse.jdt.internal.compiler.lookup.AnnotationBinding;
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding;
import org.eclipse.jdt.internal.compiler.lookup.Binding;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;
import org.eclipse.jdt.internal.compiler.lookup.CaptureBinding;
import org.eclipse.jdt.internal.compiler.lookup.ClassScope;
import org.eclipse.jdt.internal.compiler.lookup.ElementValuePair;
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding;
import org.eclipse.jdt.internal.compiler.lookup.MethodScope;
import org.eclipse.jdt.internal.compiler.lookup.ParameterizedTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeIds;
import org.eclipse.jdt.internal.compiler.lookup.WildcardBinding;
import org.eclipse.jdt.internal.compiler.util.Util;

/**
 * Big ugly class that contains all compiler extensions related code.
 * Much of the complex AST manipulation code comes from project Lombok.
 */
public class CompilerExtensions {

	public static void log(String message) {
		System.err.println(message);
		System.err.flush();
	}

	public static String logObject(Object object) {
		String objString = object.toString();
		if (objString.length() > 200)
			objString = objString.substring(0, 200);
		objString = objString.replace("\n", "\\n"); //$NON-NLS-1$ //$NON-NLS-2$
		return objString + " [" + System.identityHashCode(object) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public static long pos(ASTNode node) {
		return ((long) node.sourceStart << 32) | (node.sourceEnd & 0xFFFFFFFFL);
	}

	public static long[] poss(ASTNode node, int repeat) {
		long p = ((long) node.sourceStart << 32) | (node.sourceEnd & 0xFFFFFFFFL);
		long[] out = new long[repeat];
		Arrays.fill(out, p);
		return out;
	}

	public static TypeReference makeTypeReference(TypeBinding binding, ASTNode pos, boolean allowCompound) {
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
			ReferenceBinding ref = (ReferenceBinding) binding;
			ReferenceBinding[] supers = ref.superInterfaces();
			if (supers == null || supers.length == 0)
				supers = new ReferenceBinding[] { ref.superclass() };
			if (supers[0] == null) {
				TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				return result;
			}
			return makeTypeReference(supers[0], pos, false);
		}

		if (binding instanceof CaptureBinding) {
			return makeTypeReference(((CaptureBinding) binding).wildcard, pos, allowCompound);
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
					return makeTypeReference(wildcard.bound, pos, false);
				} else {
					Wildcard out = new Wildcard(Wildcard.EXTENDS);
					out.bound = makeTypeReference(wildcard.bound, pos, false);
					out.sourceStart = pos.sourceStart;
					out.sourceEnd = pos.sourceEnd;
					return out;
				}
			} else if (allowCompound && wildcard.boundKind == Wildcard.SUPER) {
				Wildcard out = new Wildcard(Wildcard.SUPER);
				out.bound = makeTypeReference(wildcard.bound, pos, false);
				out.sourceStart = pos.sourceStart;
				out.sourceEnd = pos.sourceEnd;
				return out;
			} else {
				TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				return result;
			}
		}

		// Keep moving up via 'binding.enclosingType()' and gather generics from each binding. We stop after a local
		// type, or a static type, or a top-level type.
		// Finally, add however many nullTypeArgument[] arrays as that are missing, inverse the list, toArray it, and
		// use that as PTR's typeArgument argument.

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
							tyParams[i] = makeTypeReference(paramized.arguments[i], pos, true);
						}
					}
				}

				params.add(tyParams);
				if (isFinalStop)
					break;
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
			if (pkg.length == 1 && pkg[0].isEmpty())
				pkg = new String[0];
			parts = new char[pkg.length + name.length][];
			int ptr;
			for (ptr = 0; ptr < pkg.length; ptr++)
				parts[ptr] = pkg[ptr].toCharArray();
			for (; ptr < pkg.length + name.length; ptr++)
				parts[ptr] = name[ptr - pkg.length].toCharArray();
		}

		while (params.size() < parts.length)
			params.add(null);
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
				TypeReference result = new ParameterizedQualifiedTypeReference(parts, typeArguments, dims,
						poss(pos, parts.length));
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
	
	public static final char[] VarName = {'v', 'a', 'r'};

	public static boolean isVar(TypeReference ref) {
		if (!(ref instanceof SingleTypeReference)) return false;
		return Arrays.equals(((SingleTypeReference) ref).token, VarName);
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
	
	public static TypeBinding resolveTypeLazy(Expression expression, BlockScope scope) {
		if (expression.resolvedType != null) return expression.resolvedType;
		try {
			return expression.resolveType(scope);
		} catch (NullPointerException e) {
			// may happen if the expression contains an unresolved "var" types
			return null;
		} catch (ArrayIndexOutOfBoundsException e) {
			// may happen if the expression contains an unresolved "var" types
			return null;
		}
	}
	
	public static void handleVarResolveLocalDeclaration(LocalDeclaration declaration, BlockScope scope) {
		log("handleVarResolveLocalDeclaration: declaration: " + logObject(declaration)); //$NON-NLS-1$
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
				TypeReference type = makeTypeReference(binding, declaration.type, false);		
				declaration.type = type;
			} catch (NullPointerException e) {
				// may happen if the parser tries to resolve the declaration before parsing the expression
				// (e.g. hovering the mouse over "var")
				log("Cannot resolve initialization"); //$NON-NLS-1$
				e.printStackTrace();
				TypeReference newType = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(declaration.type, 3));	
				declaration.type = newType;
			}
			log("Type inferred: " + declaration.type); //$NON-NLS-1$
		}
	}
	
	public static void handleVarResolveForEach(ForeachStatement statement, BlockScope scope) {
		log("handleVarResolveForEach: statement: " + logObject(statement)); //$NON-NLS-1$
		if ((statement.elementVariable != null) && isVar(statement.elementVariable.type)) {
			try {
				TypeBinding binding = getForEachComponentType(statement.collection, scope);
				TypeReference type = makeTypeReference(binding, statement.elementVariable.type, false);
				statement.elementVariable.type = type;
			} catch (NullPointerException e) {
				// may happen if the parser tries to resolve the declaration before parsing the expression
				// (e.g. hovering the mouse over "var")
				log("Cannot resolve collection"); //$NON-NLS-1$
				TypeReference newType = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(statement.elementVariable.type, 3));	
				statement.elementVariable.type = newType;
			}
			log("Type inferred: " + statement.elementVariable.type); //$NON-NLS-1$
		}
	}
	
	public static int setPublicAccess(int modifiers) {
		modifiers &= ~ClassFileConstants.AccPrivate;
		modifiers &= ~ClassFileConstants.AccProtected;
		modifiers |= ClassFileConstants.AccPublic;
		return modifiers;
	}
	
	public static void handleStructPostParse(TypeDeclaration type) {
		log("handleStructPostParse: declaration: " + logObject(type)); //$NON-NLS-1$
		if (!type.isStruct) return;
		type.modifiers = setPublicAccess(type.modifiers);
		if (type.fields != null) {
			for (FieldDeclaration field: type.fields) {
				field.modifiers = setPublicAccess(field.modifiers);
			}
		}
		if (type.methods != null) {
			for (AbstractMethodDeclaration method: type.methods) {
				if ((method instanceof MethodDeclaration) ||
						(method instanceof ConstructorDeclaration)) {
					method.modifiers = setPublicAccess(method.modifiers);
				}
			}			
		}
		if (type.memberTypes != null) {
			for (TypeDeclaration memberType: type.memberTypes) {
				memberType.modifiers = setPublicAccess(memberType.modifiers); // public even if not a struct
				handleStructPostParse(memberType);
			}
		}
	}
	
	public static void handleStructPostParse(CompilationUnitDeclaration unit) {
		log("handleStructPostParse: unit: " + logObject(unit)); //$NON-NLS-1$
		if (unit.types != null) {
			for (TypeDeclaration type: unit.types) {
				handleStructPostParse(type);
			}
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
	
	public static final char[][] ExtensionsPackageName = asChars("org", "eclipse", "jdt", "annotation", "extensions"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
	public static final char[][] ConditionalAnnotationName = buildName(ExtensionsPackageName, asChars("Conditional")); //$NON-NLS-1$
	public static final char[][] OptionalParameterName = buildName(ExtensionsPackageName, asChars("Optional")); //$NON-NLS-1$
	public static final char[][] CallerFileAnnotationName = buildName(ExtensionsPackageName, asChars("CallerFile")); //$NON-NLS-1$
	public static final char[][] CallerLineAnnotationName = buildName(ExtensionsPackageName, asChars("CallerLine")); //$NON-NLS-1$
	public static final char[][] CallerClassAnnotationName = buildName(ExtensionsPackageName, asChars("CallerClass")); //$NON-NLS-1$
	public static final char[][] CallerMethodAnnotationName = buildName(ExtensionsPackageName, asChars("CallerMethod")); //$NON-NLS-1$
	public static final char[][] CallerArgAnnotationName = buildName(ExtensionsPackageName, asChars("CallerArg")); //$NON-NLS-1$	
	public static final char[][][] SyntheticAnnotationNames = {CallerFileAnnotationName, CallerLineAnnotationName, CallerClassAnnotationName, CallerMethodAnnotationName, CallerArgAnnotationName };	
	public static final char[] ValueAnnotationParamName = asChars("value"); //$NON-NLS-1$
	
	public static boolean isSyntheticParameterAnnotation(AnnotationBinding annotation) {
		ReferenceBinding type = annotation.getAnnotationType();
		for (int i = 0; i < SyntheticAnnotationNames.length; i++) {
			if (matches(SyntheticAnnotationNames[i], type.compoundName))
				return true;
		}
		return false;
	}
	
	public static boolean isConditionalAnnotation(AnnotationBinding annotation) {
		return matches(ConditionalAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isOptionalAnnotation(AnnotationBinding annotation) {
		return matches(OptionalParameterName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallerFileAnnotation(AnnotationBinding annotation) {
		return matches(CallerFileAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallerLineAnnotation(AnnotationBinding annotation) {
		return matches(CallerLineAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallerClassAnnotation(AnnotationBinding annotation) {
		return matches(CallerClassAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallerMethodAnnotation(AnnotationBinding annotation) {
		return matches(CallerMethodAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallerArgAnnotation(AnnotationBinding annotation) {
		return matches(CallerArgAnnotationName, annotation.getAnnotationType().compoundName);
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
		log("handleSyntheticParametersForCompatibility: method: " + asString(method.selector) + ": adding " +  //$NON-NLS-1$//$NON-NLS-2$
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
		log("handleSyntheticParametersForSignature: old signature: " + asString(methodSignature) + //$NON-NLS-1$
				", new signature: " + asString(newMethodSignature)); //$NON-NLS-1$
		return newMethodSignature;
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
		initOptionalParameterAnnotations(method);
		if ((parameters == null) || (arguments.length >= parameters.length)) return arguments; // already have enough arguments
		int numOptionalAnnotations = method.numOptionalAnnotations;
		int numMissingArguments = parameters.length - arguments.length;
		if (numMissingArguments > numOptionalAnnotations) return arguments; // not enough optional arguments
		log("handleOptionalParametersForCompatibility: method: " + asString(method.selector) + ": adding " + //$NON-NLS-1$ //$NON-NLS-2$
				numMissingArguments + " missing optional arguments"); //$NON-NLS-1$ 
		TypeBinding[] result = new TypeBinding[parameters.length];
		System.arraycopy(arguments, 0, result, 0, arguments.length);
		System.arraycopy(parameters, arguments.length, result, arguments.length, numMissingArguments);
		return result;
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
	
	public static Expression makeCallerFile(Expression expression, BlockScope scope) {
		int position = expression.sourceEnd;
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		return new StringLiteral(compilationUnit.getFileName(), position, position, lineNumber);
	}
	
	public static Expression makeCallerLineExpression(Expression expression, BlockScope scope) {
		int position = expression.sourceEnd;
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		return IntLiteral.buildIntLiteral(asChars(Integer.toString(lineNumber)), position, position);
	}
	
	public static Expression makeCallerClassExpression(Expression expression, BlockScope scope) {
		int position = expression.sourceEnd;
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		return new StringLiteral(classScope.referenceContext.name, position, position, lineNumber);
	}
	
	public static Expression makeCallerMethodExpression(Expression expression, BlockScope scope) {
		int position = expression.sourceEnd;
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		MethodScope methodScope = scope.methodScope();
		if (methodScope == null) return new NullLiteral(position, position);
		AbstractMethodDeclaration declaration = methodScope.referenceMethod();
		if (declaration == null) return new NullLiteral(position, position);
		return new StringLiteral(declaration.selector, position, position, lineNumber);
	}
	
	public static Expression makeCallerArgExpression(Expression expression, BlockScope scope, int index) {
		Invocation invocation = (Invocation) expression;
		int position = expression.sourceEnd;
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		if ((index < 0) || (index >= invocation.arguments().length)) return new NullLiteral(position, position);
		Expression argument = invocation.arguments()[index];
		char[] contents = compilationUnit.compilationResult().compilationUnit.getContents();
		char[] argumentChars = extractChars(contents, argument.sourceStart, argument.sourceEnd);
		return new StringLiteral(argumentChars, position, position, lineNumber);		
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
	
	// adds synthetic arguments required to invoke the method
	public static boolean handleSyntheticArgumentsForInvocation(Expression invocation, InvocationArguments invocationArguments, MethodBinding method, BlockScope scope) {
		method = method.original();
		if (!method.isValidBinding()) return false;
		TypeBinding[] parameters = method.parameters;
		Expression[] oldArguments = invocationArguments.arguments;
		if (oldArguments == null) oldArguments = new Expression[0];
		TypeBinding[] oldArgumentTypes = invocationArguments.argumentTypes;
		if (oldArgumentTypes == null) oldArgumentTypes = new TypeBinding[0];
		if (parameters == null) return false;
		initSyntheticParameterAnnotations(method);
		int numSyntheticAnnotations = method.numSyntheticAnnotations;
		if (numSyntheticAnnotations == 0) return false;
		log("handleSyntheticArgumentsForInvocation: method: " + asString(method.selector) + ": adding " + //$NON-NLS-1$ //$NON-NLS-2$
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
				if (isCallerFileAnnotation(annotation)) {
					newArguments[i] = makeCallerFile(invocation, scope);
				} else if (isCallerLineAnnotation(annotation)) {
					newArguments[i] = makeCallerLineExpression(invocation, scope);
				} else if (isCallerClassAnnotation(annotation)) {
					newArguments[i] = makeCallerClassExpression(invocation, scope);
				} else if (isCallerMethodAnnotation(annotation)) {
					newArguments[i] = makeCallerMethodExpression(invocation, scope);
				} else if (isCallerArgAnnotation(annotation)) {
					int index = getIntAnnotationParam(annotation, ValueAnnotationParamName);
					newArguments[i] = makeCallerArgExpression(invocation, scope, index);
				} else {
					newArguments[i] = makeDefaultLiteral(method.parameters[i], invocation.sourceEnd, invocation.sourceEnd);
				}
				newArgumentTypes[i] = newArguments[i].resolveType(scope);
				if (newArgumentTypes[i] == null) return false; // cannot resolve a basic literal??				
			}
		}
		invocationArguments.arguments = newArguments;
		invocationArguments.argumentTypes = newArgumentTypes;
		return true;
	}
	
	// adds synthetic arguments required to invoke the method
	public static void handleSyntheticArgumentsForMessageSend(MessageSend invocation, BlockScope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		if (handleSyntheticArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope)) {			
			invocation.arguments = invocationArguments.arguments;
			invocation.argumentTypes = invocationArguments.argumentTypes;
		}
	}
	
	// adds synthetic arguments required to invoke the method
	public static void handleSyntheticArgumentsForAllocation(AllocationExpression invocation, BlockScope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		if (handleSyntheticArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope)) {			
			invocation.arguments = invocationArguments.arguments;
			invocation.argumentTypes = invocationArguments.argumentTypes;
		}
	}
	
	// adds non-provided optional arguments required to invoke the method
	public static boolean handleOptionalArgumentsForInvocation(Expression invocation, InvocationArguments invocationArguments, MethodBinding method, BlockScope scope) {
		method = method.original();
		if (!method.isValidBinding()) return false;
		TypeBinding[] parameters = method.parameters;
		Expression[] oldArguments = invocationArguments.arguments;
		if (oldArguments == null) oldArguments = new Expression[0];
		TypeBinding[] oldArgumentTypes = invocationArguments.argumentTypes;
		if (oldArgumentTypes == null) oldArgumentTypes = new TypeBinding[0];
		initOptionalParameterAnnotations(method);
		if ((parameters == null) || (oldArguments.length >= parameters.length)) return false; // already have enough arguments
		int numMissingArguments = parameters.length - oldArguments.length;
		if (numMissingArguments > method.numOptionalAnnotations) return false; // should not happen after compatibility checks
		log("handleOptionalArgumentsForInvocation: method: " + asString(method.selector) + ": adding " + //$NON-NLS-1$ //$NON-NLS-2$
				numMissingArguments + " missing optional arguments"); //$NON-NLS-1$ 
		Expression[] newArguments = new Expression[parameters.length];
		System.arraycopy(oldArguments, 0, newArguments, 0, oldArguments.length);
		TypeBinding[] newArgumentTypes = new TypeBinding[parameters.length];
		System.arraycopy(oldArgumentTypes, 0, newArgumentTypes, 0, oldArgumentTypes.length);
		for (int i = oldArguments.length; i < parameters.length; i++) {
			AnnotationBinding annotation = method.optionalAnnotations[i];
			if (annotation == null) return false; // should not happen after compatibility checks
			newArguments[i] = makeDefaultLiteral(method.parameters[i], invocation.sourceEnd, invocation.sourceEnd);
			newArgumentTypes[i] = newArguments[i].resolveType(scope);
			if (newArgumentTypes[i] == null) return false; // cannot resolve a basic literal??
		}
		invocationArguments.arguments = newArguments;
		invocationArguments.argumentTypes = newArgumentTypes;
		return true;
	}
	
	// adds non-provided optional arguments required to invoke the method
	public static void handleOptionalArgumentsForMessageSend(MessageSend invocation, BlockScope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		if (handleOptionalArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope)) {			
			invocation.arguments = invocationArguments.arguments;
			invocation.argumentTypes = invocationArguments.argumentTypes;
		}
	}
	
	// adds non-provided optional arguments required to invoke the method
	public static void handleOptionalArgumentsForAllocation(AllocationExpression invocation, BlockScope scope) {
		InvocationArguments invocationArguments = new InvocationArguments(invocation.arguments, invocation.argumentTypes);
		if (handleOptionalArgumentsForInvocation(invocation, invocationArguments, invocation.binding, scope)) {			
			invocation.arguments = invocationArguments.arguments;
			invocation.argumentTypes = invocationArguments.argumentTypes;
		}
	}
	
	public static boolean isMessageSendDisabled(MethodBinding method) {
		AnnotationBinding[] annotations = method.getAnnotations();
		if (annotations == null) return false;
		for (AnnotationBinding annotation: annotations) {
			if (isConditionalAnnotation(annotation)) {
				boolean enabled = getBooleanAnnotationParam(annotation, ValueAnnotationParamName);
				return !enabled;
			}
		}
		return false;
	}
}
