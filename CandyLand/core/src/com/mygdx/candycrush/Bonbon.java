package com.mygdx.candycrush;

import com.badlogic.gdx.graphics.Color;

public abstract class Bonbon {
	
	protected int ligne ;
	protected int colonne;
	protected Color couleur;
	protected float animX;
	protected float animY;

	
	
	/** Obtenir l'abscisse d'un bonbon
	 * 
	 * @return L'abscisse du bonbon
	 */
	int getL() {
		return this.ligne;
	}
	
	/** Obtenir la colonne d'un bonbon
	 * 
	 * @return La colonne du bonbon
	 */
	int getC() {
		return this.colonne;
	};
	
	/** Obtenir la couleur d'un bonbon
	 * 
	 * @return La couleur du bonbon
	 */
	Color getCouleur() {
		return this.couleur;
	}
	
	/** Modifier la ligne d'un bonbon
	 * 
	 * @param nouvellelgn La ligne que l'on souhaite attribuer au bonbon
	 */
	void setX(int nouvellecln) {
		this.colonne = nouvellecln;
	}
	
	/**Modifier la colonne d'un bonbon
	 * 
	 * @param nouvellecln la colonne que l'on souhaite attribuer au bonbon
	 */
	void setY(int nouvelleln) {
		this.ligne = nouvelleln;
	}
	
	/**Détruire le bonbon (en le mettant à null)
	 * 
	 */
	protected static void detruire(Bonbon bn){
		bn = null;
		System.gc();
	}
	
	
	/**
	 * renseigne sue le nom du bonbon spécial 
	 * @return le nom du bonbon spéciale
	 */
	String getNom() { return null; } //(reste de même corps pour les bonbons ordinnaires
	
	/**
	 * genere des bonbons spécials dans la grille du jeu 
	 * @param grl la grille du jeu
	 * @param p,q,r,s 
	 */
	void generer(Grille grl, int p, int q, int r, int s) {} //(reste de corps vide pour les bonbons ordinnaire)
	
	
	/**
	 * active l'effet du bonbon spécial dans la grille en question 
	 * @param grl la grille du jeu
	 */
	void activer(Grille grl) {} //(reste de corps vide pour les bonbons ordinnaires)

	void animate() {
		animY = 1200;
	}
	
}
