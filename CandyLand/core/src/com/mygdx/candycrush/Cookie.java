package com.mygdx.candycrush;

import com.badlogic.gdx.graphics.Color;

public class Cookie extends BonbonSpec {
	
	private final String nom = "Cookie";
	
	
	/**
	 * le constructeur
	 * @param x la ligne
	 * @param y la colonne
	 */
	public Cookie(int x, int y) {
		super.ligne = y;
		super.colonne = x;
		super.couleur = Color.MAGENTA;
		super.animX=x;
		super.animY=y;
	}

	@Override
	String getNom() {
		return this.nom;
	}

	@Override
	void generer(Grille grl, int p, int q, int r, int s) {
		// TODO Auto-generated method stub
		
	}

	@Override
	void activer(Grille grl) {
		// TODO Auto-generated method stub
		
	}

}
