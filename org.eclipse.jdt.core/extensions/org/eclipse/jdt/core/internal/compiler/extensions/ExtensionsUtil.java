package org.eclipse.jdt.core.internal.compiler.extensions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.jdt.core.extensions.ExtensionsConfig;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.Expression;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedSingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.ast.Wildcard;
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding;
import org.eclipse.jdt.internal.compiler.lookup.Binding;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;
import org.eclipse.jdt.internal.compiler.lookup.CaptureBinding;
import org.eclipse.jdt.internal.compiler.lookup.ParameterizedTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeConstants;
import org.eclipse.jdt.internal.compiler.lookup.TypeIds;
import org.eclipse.jdt.internal.compiler.lookup.WildcardBinding;

/**
 * Utility methods for manipulating the Eclipse AST.
 * Shamelessly extracted from project Lombok.
 */
public class ExtensionsUtil {
	
	public static long pos(ASTNode node) {
		return ((long) node.sourceStart << 32) | (node.sourceEnd & 0xFFFFFFFFL);
	}

	public static long[] poss(ASTNode node, int repeat) {
		long p = ((long) node.sourceStart << 32) | (node.sourceEnd & 0xFFFFFFFFL);
		long[] out = new long[repeat];
		Arrays.fill(out, p);
		return out;
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
			ReferenceBinding ref = (ReferenceBinding) binding;
			ReferenceBinding[] supers = ref.superInterfaces();
			if (supers == null || supers.length == 0)
				supers = new ReferenceBinding[] { ref.superclass() };
			if (supers[0] == null) {
				TypeReference result = new QualifiedTypeReference(TypeConstants.JAVA_LANG_OBJECT, poss(pos, 3));
				return result;
			}
			return makeType(supers[0], pos, false);
		}

		if (binding instanceof CaptureBinding) {
			return makeType(((CaptureBinding) binding).wildcard, pos, allowCompound);
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
							tyParams[i] = makeType(paramized.arguments[i], pos, true);
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

	public static boolean matches(String key, char[] array) {
		if (array == null || key.length() != array.length)
			return false;
		for (int i = 0; i < array.length; i++) {
			if (key.charAt(i) != array[i])
				return false;
		}

		return true;
	}

	public static boolean isSingleTypeReference(TypeReference ref, String name) {
		if (ref instanceof SingleTypeReference) {
			char[] token = ((SingleTypeReference) ref).token;
			return matches(name, token);
		}

		return false;
	}

	public static boolean isVar(TypeReference ref) {
		return isSingleTypeReference(ref, "var"); //$NON-NLS-1$
	}

	public static void log(String message) {
		System.err.println(message);
		System.err.flush();
	}

	public static String logObject(Object object) {
		return object.toString() + " [" + System.identityHashCode(object) + "]"; //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public static TypeBinding getForEachComponentType(Expression collection, BlockScope scope) {
		if (collection != null) {
			TypeBinding resolved = collection.resolvedType;
			if (resolved == null) resolved = collection.resolveType(scope);
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
		if (ExtensionsConfig.ENABLE_VAR && (expression.resolvedType != null)) {
			return expression.resolvedType;
		}
		return expression.resolveType(scope);
	}
}
