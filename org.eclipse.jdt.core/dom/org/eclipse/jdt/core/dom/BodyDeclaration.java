/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.eclipse.jdt.core.dom;

import java.util.List;

/**
 * Abstract base class of all AST nodes that represent body declarations 
 * that may appear in the body of some kind of class or interface declaration,
 * including anonymous class declarations, enumeration declarations, and
 * enumeration constant declarations.
 * 
 * <p>
 * <pre>
 * BodyDeclaration:
 *		ClassDeclaration
 *		InterfaceDeclaration
 *		EnumDeclaration
 *		MethodDeclaration
 * 		ConstructorDeclaration
 * 		FieldDeclaration
 * 		Initializer
 *		EnumConstantDeclaration
 *		AnnotationTypeDeclaration
 *		AnnotationTypeMemberDeclaration
 * </pre>
 * </p>
 * <p>
 * All types of body declarations carry modifiers and annotations, although they differ in
 * which modifiers are allowed. Most types of body declarations can carry a
 * doc comment; Initializer is the only ones that does not. The source range
 * for body declarations always includes the doc comment if present.
 * </p>
 * 
 * @since 2.0
 */
public abstract class BodyDeclaration extends ASTNode {
	
	/**
	 * The doc comment, or <code>null</code> if none.
	 * Defaults to none.
	 */
	Javadoc optionalDocComment = null;

	/**
	 * The modifier flags; bit-wise or of Modifier flags.
	 * Defaults to none.
	 * @since 3.0
	 * TBD (jeem) - deprecate
	 */
	private int modifierFlags = Modifier.NONE;
	
	/**
	 * The extended modifiers (element type: <code>ExtendedModifier</code>). 
	 * Defaults to an empty list.
	 * 
	 * @since 3.0
	 */
	ASTNode.NodeList modifiers =
		new ASTNode.NodeList(true, ExtendedModifier.class);

	
	/**
	 * Creates a new AST node for a body declaration node owned by the 
	 * given AST.
	 * <p>
	 * N.B. This constructor is package-private.
	 * </p>
	 * 
	 * @param ast the AST that is to own this node
	 */
	BodyDeclaration(AST ast) {
		super(ast);
	}
	
	/**
	 * Returns the doc comment node.
	 * 
	 * @return the doc comment node, or <code>null</code> if none
	 */
	public Javadoc getJavadoc() {
		return this.optionalDocComment;
	}

	/**
	 * Sets or clears the doc comment node.
	 * 
	 * @param docComment the doc comment node, or <code>null</code> if none
	 * @exception IllegalArgumentException if the doc comment string is invalid
	 */
	public void setJavadoc(Javadoc docComment) {
		replaceChild(this.optionalDocComment, docComment, false);
		this.optionalDocComment = docComment;
	}

	/**
	 * Returns the modifiers explicitly specified on this declaration.
	 * The allowable modifiers differ for each type of body declaration.
	 * 
	 * @return the bit-wise or of <code>Modifier</code> constants
	 * @see Modifier
	 * @since 3.0
	 * TBD (jeem) - deprecate
	 */ 
	public int getModifiers() {
		return this.modifierFlags;
	}

	/**
	 * Sets the modifiers explicitly specified on this declaration.
	 * The allowable modifiers differ for each type of body declaration.
	 * 
	 * @param modifiers the given modifiers (bit-wise or of <code>Modifier</code> constants)
	 * @see Modifier
	 * @exception IllegalArgumentException if the modifiers are illegal
	 * @since 3.0
	 * TBD (jeem) - deprecate
	 */ 
	public void setModifiers(int modifiers) {
		modifying();
		this.modifierFlags = modifiers;
	}

	/**
	 * Returns the live ordered list of modifiers and annotations
	 * of this declaration.
	 * <p>
	 * Note: Support for annotation metadata is an experimental language feature 
	 * under discussion in JSR-175 and under consideration for inclusion
	 * in the 1.5 release of J2SE. The support here is therefore tentative
	 * and subject to change.
	 * </p>
	 * 
	 * @return the live list of modifiers and annotations
	 *    (element type: <code>ExtendedModifier</code>)
	 * @since 3.0
	 */ 
	public List modifiers() {
		return this.modifiers;
	}
	
	/* (omit javadoc for this method)
	 * Method declared on ASTNode.
	 */
	int memSize() {
		return BASE_NODE_SIZE + 3 * 4;
	}
}

