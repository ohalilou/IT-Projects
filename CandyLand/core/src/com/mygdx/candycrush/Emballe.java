package com.mygdx.candycrush;

import com.badlogic.gdx.graphics.Color;

public class Emballe extends BonbonSpec {
	private String nom = "emballé";
	
	
	/**
	 * le constructeur
	 * @param x la ligne
	 * @param y la colonne
	 */
	public Emballe(int x, int y, Color couleur) {
		super.ligne = y;
		super.colonne = x;
		super.couleur = couleur ;
		super.animX=x;
		super.animY=y;
	}

	@Override
	public String getNom() {
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
