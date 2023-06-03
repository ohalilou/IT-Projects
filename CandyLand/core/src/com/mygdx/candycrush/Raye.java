package com.mygdx.candycrush;

import com.badlogic.gdx.graphics.Color;

public class Raye extends BonbonSpec {
	
	private final String nom = "rayé";
	
	
	/**
	 * le constructeur
	 * @param x la ligne
	 * @param y la colonne
	 */
	public Raye(int x, int y, Color clr) {
		super.ligne = y;
		super.colonne = x;
		super.couleur = clr;
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
