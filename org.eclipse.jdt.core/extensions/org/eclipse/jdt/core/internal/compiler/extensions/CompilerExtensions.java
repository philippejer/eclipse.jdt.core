package org.eclipse.jdt.core.internal.compiler.extensions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.core.extensions.ExtensionsConfig;
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.AllocationExpression;
import org.eclipse.jdt.internal.compiler.ast.Argument;
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
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
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ReturnStatement;
import org.eclipse.jdt.internal.compiler.ast.SingleNameReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Statement;
import org.eclipse.jdt.internal.compiler.ast.StringLiteral;
import org.eclipse.jdt.internal.compiler.ast.ThisReference;
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
import org.eclipse.jdt.internal.compiler.lookup.Scope;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeIds;
import org.eclipse.jdt.internal.compiler.lookup.WildcardBinding;
import org.eclipse.jdt.internal.compiler.util.Util;

import com.rits.cloning.Cloner;

/**
 * Big ugly class that contains all compiler extensions related code.
 * Much of the complex AST manipulation code comes from project Lombok.
 */
public class CompilerExtensions {
	
	public static Cloner cloner = null;
	
	static {
		// create and configure reflection-based object cloner for AST nodes
		cloner = new Cloner();
		// these should be shallow copied
		cloner.setDontCloneInstanceOf(CompilationResult.class);
		// check that only stuff directly derived by the parser is cloned
		cloner.setDumpClonedClasses(false);
	}

	public static long[] copy(long[] array) {
		return array == null ? null : array.clone();
	}
	
	public static long pos(int startPos, int endPos) {
		return ((long) startPos << 32) | (endPos & 0xFFFFFFFFL);
	}
	
	public static long pos(ASTNode node) {
		return pos(node.sourceStart, node.sourceEnd);
	}
	
	public static long[] poss(ASTNode node, int repeat) {
		long p = pos(node);
		long[] out = new long[repeat];
		Arrays.fill(out, p);
		return out;
	}
	
//	public static Annotation copyAnnotation(Annotation annotation, ASTNode source) {
//		int pS = source.sourceStart, pE = source.sourceEnd;
//		
//		if (annotation instanceof MarkerAnnotation) {
//			MarkerAnnotation ann = new MarkerAnnotation(copyType(annotation.type, source), pS);
//			ann.declarationSourceEnd = ann.sourceEnd = ann.statementEnd = pE;
//			return ann;
//		}
//		
//		if (annotation instanceof SingleMemberAnnotation) {
//			SingleMemberAnnotation ann = new SingleMemberAnnotation(copyType(annotation.type, source), pS);
//			ann.declarationSourceEnd = ann.sourceEnd = ann.statementEnd = pE;
//			//TODO memberValue(s) need to be copied as well (same for copying a NormalAnnotation as below).
//			ann.memberValue = ((SingleMemberAnnotation)annotation).memberValue;
//			return ann;
//		}
//		
//		if (annotation instanceof NormalAnnotation) {
//			NormalAnnotation ann = new NormalAnnotation(copyType(annotation.type, source), pS);
//			ann.declarationSourceEnd = ann.statementEnd = ann.sourceEnd = pE;
//			ann.memberValuePairs = ((NormalAnnotation)annotation).memberValuePairs;
//			return ann;
//		}
//		
//		return annotation;
//	}
//	
//	public static Annotation[] copyAnnotations(ASTNode source, Annotation[]... allAnnotations) {
//		List<Annotation> result = null;
//		for (Annotation[] annotations : allAnnotations) {
//			if (annotations != null) {
//				for (Annotation annotation : annotations) {
//					if (result == null) result = new ArrayList<Annotation>();
//					result.add(copyAnnotation(annotation, source));
//				}
//			}
//		}
//		
//		return result == null ? null : result.toArray(new Annotation[0]);
//	}
//	
//	public static TypeParameter[] copyTypeParams(TypeParameter[] params, ASTNode source) {
//		if (params == null) return null;
//		TypeParameter[] out = new TypeParameter[params.length];
//		int idx = 0;
//		for (TypeParameter param : params) {
//			TypeParameter o = new TypeParameter();
//			o.annotations = param.annotations;
//			o.bits = param.bits;
//			o.modifiers = param.modifiers;
//			o.name = param.name;
//			o.type = copyType(param.type, source);
//			o.sourceStart = param.sourceStart;
//			o.sourceEnd = param.sourceEnd;
//			o.declarationEnd = param.declarationEnd;
//			o.declarationSourceStart = param.declarationSourceStart;
//			o.declarationSourceEnd = param.declarationSourceEnd;
//			if (param.bounds != null) {
//				TypeReference[] b = new TypeReference[param.bounds.length];
//				int idx2 = 0;
//				for (TypeReference ref : param.bounds) b[idx2++] = copyType(ref, source);
//				o.bounds = b;
//			}
//			out[idx++] = o;
//		}
//		return out;
//	}
//	
//	public static TypeReference namePlusTypeParamsToTypeReference(char[] typeName, TypeParameter[] params, long p) {
//		if (params != null && params.length > 0) {
//			TypeReference[] refs = new TypeReference[params.length];
//			int idx = 0;
//			for (TypeParameter param : params) {
//				TypeReference typeRef = new SingleTypeReference(param.name, p);
//				refs[idx++] = typeRef;
//			}
//			return new ParameterizedSingleTypeReference(typeName, refs, 0, p);
//		}
//		
//		return new SingleTypeReference(typeName, p);
//	}
//	
//	public static TypeReference[] copyTypes(TypeReference[] refs) {
//		return copyTypes(refs, null);
//	}
//	
//	public static TypeReference[] copyTypes(TypeReference[] refs, ASTNode source) {
//		if (refs == null) return null;
//		TypeReference[] outs = new TypeReference[refs.length];
//		int idx = 0;
//		for (TypeReference ref : refs) {
//			outs[idx++] = copyType(ref, source);
//		}
//		return outs;
//	}
//	
//	public static TypeReference copyType(TypeReference ref) {
//		return copyType(ref, null);
//	}
//	
//	public static TypeReference copyType(TypeReference ref, ASTNode source) {
//		if (ref instanceof ParameterizedQualifiedTypeReference) {
//			ParameterizedQualifiedTypeReference iRef = (ParameterizedQualifiedTypeReference) ref;
//			TypeReference[][] args = null;
//			if (iRef.typeArguments != null) {
//				args = new TypeReference[iRef.typeArguments.length][];
//				int idx = 0;
//				for (TypeReference[] inRefArray : iRef.typeArguments) {
//					if (inRefArray == null) args[idx++] = null;
//					else {
//						TypeReference[] outRefArray = new TypeReference[inRefArray.length];
//						int idx2 = 0;
//						for (TypeReference inRef : inRefArray) {
//							outRefArray[idx2++] = copyType(inRef, source);
//						}
//						args[idx++] = outRefArray;
//					}
//				}
//			}
//			
//			TypeReference typeRef = new ParameterizedQualifiedTypeReference(iRef.tokens, args, iRef.dimensions(), copy(iRef.sourcePositions));
//			return typeRef;
//		}
//		
//		if (ref instanceof ArrayQualifiedTypeReference) {
//			ArrayQualifiedTypeReference iRef = (ArrayQualifiedTypeReference) ref;
//			TypeReference typeRef = new ArrayQualifiedTypeReference(iRef.tokens, iRef.dimensions(), copy(iRef.sourcePositions));
//			return typeRef;
//		}
//		
//		if (ref instanceof QualifiedTypeReference) {
//			QualifiedTypeReference iRef = (QualifiedTypeReference) ref;
//			TypeReference typeRef = new QualifiedTypeReference(iRef.tokens, copy(iRef.sourcePositions));
//			return typeRef;
//		}
//		
//		if (ref instanceof ParameterizedSingleTypeReference) {
//			ParameterizedSingleTypeReference iRef = (ParameterizedSingleTypeReference) ref;
//			TypeReference[] args = null;
//			if (iRef.typeArguments != null) {
//				args = new TypeReference[iRef.typeArguments.length];
//				int idx = 0;
//				for (TypeReference inRef : iRef.typeArguments) {
//					if (inRef == null) args[idx++] = null;
//					else args[idx++] = copyType(inRef, source);
//				}
//			}
//			
//			TypeReference typeRef = new ParameterizedSingleTypeReference(iRef.token, args, iRef.dimensions(), (long)iRef.sourceStart << 32 | iRef.sourceEnd);
//			return typeRef;
//		}
//		
//		if (ref instanceof ArrayTypeReference) {
//			ArrayTypeReference iRef = (ArrayTypeReference) ref;
//			TypeReference typeRef = new ArrayTypeReference(iRef.token, iRef.dimensions(), (long)iRef.sourceStart << 32 | iRef.sourceEnd);
//			return typeRef;
//		}
//		
//		if (ref instanceof Wildcard) {
//			Wildcard original = (Wildcard)ref;
//			
//			Wildcard wildcard = new Wildcard(original.kind);
//			wildcard.sourceStart = original.sourceStart;
//			wildcard.sourceEnd = original.sourceEnd;
//			if (original.bound != null) wildcard.bound = copyType(original.bound, source);
//			return wildcard;
//		}
//		
//		if (ref instanceof SingleTypeReference) {
//			SingleTypeReference iRef = (SingleTypeReference) ref;
//			TypeReference typeRef = new SingleTypeReference(iRef.token, (long)iRef.sourceStart << 32 | iRef.sourceEnd);
//			return typeRef;
//		}
//		
//		return ref;
//	}
	
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
	
	public static final char[] VarName = {'v', 'a', 'r'};
	public static final char[] VoidName = {'v', 'o', 'i', 'd'};

	public static boolean isVar(TypeReference ref) {
		return isSingleTypeReference(ref, VarName);
	}

	public static boolean isVoid(TypeReference ref) {
		return isSingleTypeReference(ref, VoidName);
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
				// may happen if the parser tries to resolve the declaration before parsing the expression
				// (e.g. hovering the mouse over "var")
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
				// may happen if the parser tries to resolve the declaration before parsing the expression
				// (e.g. hovering the mouse over "var")
				ExtensionsConfig.log("Cannot resolve collection"); //$NON-NLS-1$
				TypeReference newType = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(statement.elementVariable.type, 3));	
				statement.elementVariable.type = newType;
			}
			ExtensionsConfig.log("Type inferred: " + statement.elementVariable.type); //$NON-NLS-1$
		}
	}
	
	public static int setPublicAccess(int modifiers) {
		modifiers &= ~ClassFileConstants.AccPrivate;
		modifiers &= ~ClassFileConstants.AccProtected;
		modifiers |= ClassFileConstants.AccPublic;
		return modifiers;
	}
	
	public static void handleStructEndParse(TypeDeclaration type) {
		ExtensionsConfig.log("handleStructEndParse: declaration: " + ExtensionsConfig.asLog(type)); //$NON-NLS-1$
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
				handleStructEndParse(memberType);
			}
		}
	}
	
	public static void handleStructEndParse(CompilationUnitDeclaration unit) {
		ExtensionsConfig.log("handleStructEndParse: unit: " + ExtensionsConfig.asLog(unit)); //$NON-NLS-1$
		if (unit.types != null) {
			for (TypeDeclaration type: unit.types) {
				handleStructEndParse(type);
			}
		}
	}
	
	public static void handleDefaultArgumentsMethod(MethodDeclaration copy, int argumentIndex) {
		int startPos = copy.bodyStart, endPos = copy.bodyEnd;
		copy.bits |= GeneratedBit;
		MessageSend invocation = new MessageSend();
		invocation.sourceStart = startPos;
		invocation.sourceEnd = endPos;
		invocation.statementEnd = endPos;
		invocation.receiver = ThisReference.implicitThis();
		invocation.receiver.sourceStart = startPos;
		invocation.receiver.sourceEnd = endPos;	
		invocation.receiver.statementEnd = endPos;
		invocation.nameSourcePosition = pos(startPos, endPos);
		invocation.selector = copy.selector;
		invocation.arguments = new Expression[copy.arguments.length];
		for (int i = 0; i < invocation.arguments.length; i++) {
			Argument argument = copy.arguments[i];
			if (i < argumentIndex) {
				invocation.arguments[i] = new SingleNameReference(argument.name, pos(startPos, endPos));
			} else {
				invocation.arguments[i] = argument.defaultExpression;
			}
		}
		if (argumentIndex > 0) {
			copy.arguments = Arrays.copyOf(copy.arguments, argumentIndex);
		} else {
			copy.arguments = null;
		}		
		copy.statements = new Statement[1];
		if (!isVoid(copy.returnType)) {
			copy.statements[0] = new ReturnStatement(invocation, startPos, endPos);
		}
		copy.statements[0] = invocation;
	}
	
	public static int handleDefaultArgumentsEndParse(MethodDeclaration method, AbstractMethodDeclaration[] methods, int index) {
		for (int argumentIndex = method.arguments.length - 1; argumentIndex >= 0; argumentIndex--) {
			Argument argument = method.arguments[argumentIndex];
			if (argument.defaultExpression == null) break;	
			MethodDeclaration copy = cloner.deepClone(method);
			handleDefaultArgumentsMethod(copy, argumentIndex);
			methods[index++] = copy;
		}
		return index;
	}
	
	public static void handleDefaultArgumentsConstructor(ConstructorDeclaration copy, int argumentIndex) {
		int startPos = copy.bodyStart, endPos = copy.bodyEnd;	
		copy.bits |= GeneratedBit;	
		ExplicitConstructorCall invocation = new ExplicitConstructorCall(ExplicitConstructorCall.This);
		invocation.sourceStart = startPos;
		invocation.sourceEnd = endPos;
		invocation.arguments = new Expression[copy.arguments.length];
		for (int i = 0; i < invocation.arguments.length; i++) {
			Argument argument = copy.arguments[i];
			if (i < argumentIndex) {
				invocation.arguments[i] = new SingleNameReference(argument.name, pos(startPos, endPos));
			} else {
				invocation.arguments[i] = argument.defaultExpression;
			}
		}
		if (argumentIndex > 0) {
			copy.arguments = Arrays.copyOf(copy.arguments, argumentIndex);
		} else {
			copy.arguments = null;
		}		
		copy.constructorCall = invocation;
	}
	
	public static int handleDefaultArgumentsEndParse(ConstructorDeclaration constructor, AbstractMethodDeclaration[] methods, int index) {
		for (int argumentIndex = constructor.arguments.length - 1; argumentIndex >= 0; argumentIndex--) {
			Argument argument = constructor.arguments[argumentIndex];
			if (argument.defaultExpression == null) break;
			ConstructorDeclaration copy = cloner.deepClone(constructor);
			handleDefaultArgumentsConstructor(copy, argumentIndex);
			methods[index++] = copy;
		}
		return index;
	}
	
	public static void handleDefaultArgumentsEndParse(CompilationUnitDeclaration unit) {
		ExtensionsConfig.log("handleDefaultArgumentsEndParse: unit: " + ExtensionsConfig.asLog(unit)); //$NON-NLS-1$
		if (unit.types != null) {
			for (TypeDeclaration type: unit.types) {
				if (type.methods == null) continue;
				int numDefaultArguments = 0;
				for (int i = 0; i < type.methods.length; i++) {
					AbstractMethodDeclaration method = type.methods[i];
					if (method.arguments == null) continue;
					for (int j = method.arguments.length - 1; j >= 0; j--) {
						Argument argument = method.arguments[j];
						if (argument.defaultExpression == null) break;
						numDefaultArguments += 1;			
					}
				}
				if (numDefaultArguments == 0) continue;
				AbstractMethodDeclaration[] methods = new AbstractMethodDeclaration[type.methods.length + numDefaultArguments];
				System.arraycopy(type.methods, 0, methods, 0, type.methods.length);
				for (int i = 0, j = type.methods.length; i < type.methods.length; i++) {
					AbstractMethodDeclaration method = type.methods[i];	
					if (method.arguments == null) continue;
					if (method instanceof MethodDeclaration) {
						j = handleDefaultArgumentsEndParse((MethodDeclaration) method, methods, j);
					} else if (method instanceof ConstructorDeclaration) {
						j = handleDefaultArgumentsEndParse((ConstructorDeclaration) method, methods, j);
					}
				}
				type.methods = methods;
			}
		}
	}
	
	public static final int ProcessedBit = ASTNode.Bit24;
	
	public static void handleEndParse(CompilationUnitDeclaration unit) {
		if ((unit.bits & ProcessedBit) == 0) {
			handleStructEndParse(unit);
			handleDefaultArgumentsEndParse(unit);
			unit.bits |= ProcessedBit;
		}
	}
	
	public static final int GeneratedBit = ASTNode.Bit24;
	
	public static boolean isGeneratedMethod(AbstractMethodDeclaration method) {
		return (method.bits & GeneratedBit) != 0;
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
	public static final char[][] CallIfAnnotationName = buildName(ExtensionsPackageName, asChars("CallIf")); //$NON-NLS-1$
	public static final char[][] CallFileAnnotationName = buildName(ExtensionsPackageName, asChars("CallFile")); //$NON-NLS-1$
	public static final char[][] CallLineAnnotationName = buildName(ExtensionsPackageName, asChars("CallLine")); //$NON-NLS-1$
	public static final char[][] CallClassAnnotationName = buildName(ExtensionsPackageName, asChars("CallClass")); //$NON-NLS-1$
	public static final char[][] CallMethodAnnotationName = buildName(ExtensionsPackageName, asChars("CallMethod")); //$NON-NLS-1$
	public static final char[][] CallArgAnnotationName = buildName(ExtensionsPackageName, asChars("CallArg")); //$NON-NLS-1$	
	public static final char[][][] SyntheticAnnotationNames = {CallFileAnnotationName, CallLineAnnotationName, CallClassAnnotationName, CallMethodAnnotationName, CallArgAnnotationName };	
	public static final char[] ValueAnnotationParamName = asChars("value"); //$NON-NLS-1$
	
	public static boolean isSyntheticParameterAnnotation(AnnotationBinding annotation) {
		ReferenceBinding type = annotation.getAnnotationType();
		for (int i = 0; i < SyntheticAnnotationNames.length; i++) {
			if (matches(SyntheticAnnotationNames[i], type.compoundName))
				return true;
		}
		return false;
	}
	
	public static boolean isCallIfAnnotation(AnnotationBinding annotation) {
		return matches(CallIfAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallFileAnnotation(AnnotationBinding annotation) {
		return matches(CallFileAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallLineAnnotation(AnnotationBinding annotation) {
		return matches(CallLineAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallClassAnnotation(AnnotationBinding annotation) {
		return matches(CallClassAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallMethodAnnotation(AnnotationBinding annotation) {
		return matches(CallMethodAnnotationName, annotation.getAnnotationType().compoundName);
	}
	
	public static boolean isCallArgAnnotation(AnnotationBinding annotation) {
		return matches(CallArgAnnotationName, annotation.getAnnotationType().compoundName);
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
	
	public static Expression makeCallFile(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		return new StringLiteral(compilationUnit.getFileName(), position, position, 0);
	}
	
	public static Expression makeCallLineExpression(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		int[] separatorPositions = compilationUnit.compilationResult().getLineSeparatorPositions();
		int lineNumber = Util.getLineNumber(position, separatorPositions, 0, separatorPositions.length - 1);
		return IntLiteral.buildIntLiteral(asChars(Integer.toString(lineNumber)), position, position);
	}
	
	public static Expression makeCallClassExpression(Invocation invocation, Scope scope) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		return new StringLiteral(classScope.referenceContext.name, position, position, 0);
	}
	
	public static Expression makeCallMethodExpression(Invocation invocation, Scope scope) {
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
	
	public static Expression makeCallArgExpression(Invocation invocation, Scope scope, int index) {
		int position = invocation.sourceEnd();
		ClassScope classScope = scope.classScope();
		if (classScope == null) return new NullLiteral(position, position);
		CompilationUnitDeclaration compilationUnit = classScope.referenceCompilationUnit();
		if (compilationUnit == null) return new NullLiteral(position, position);
		if ((index < 0) || (index >= invocation.arguments().length)) return new NullLiteral(position, position);
		Expression argument = invocation.arguments()[index];
		char[] contents = compilationUnit.compilationResult().compilationUnit.getContents();
		char[] argumentChars = extractChars(contents, argument.sourceStart, argument.sourceEnd);
		return new StringLiteral(argumentChars, position, position, 0);		
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
					newArguments[i] = makeCallFile(invocation, scope);
				} else if (isCallLineAnnotation(annotation)) {
					newArguments[i] = makeCallLineExpression(invocation, scope);
				} else if (isCallClassAnnotation(annotation)) {
					newArguments[i] = makeCallClassExpression(invocation, scope);
				} else if (isCallMethodAnnotation(annotation)) {
					newArguments[i] = makeCallMethodExpression(invocation, scope);
				} else if (isCallArgAnnotation(annotation)) {
					int index = j;
					newArguments[i] = makeCallArgExpression(invocation, scope, index);
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
	
	public static boolean isMessageSendDisabled(MethodBinding method) {
		AnnotationBinding[] annotations = method.getAnnotations();
		if (annotations == null) return false;
		for (AnnotationBinding annotation: annotations) {
			if (isCallIfAnnotation(annotation)) {
				boolean enabled = getBooleanAnnotationParam(annotation, ValueAnnotationParamName);
				return !enabled;
			}
		}
		return false;
	}
}
