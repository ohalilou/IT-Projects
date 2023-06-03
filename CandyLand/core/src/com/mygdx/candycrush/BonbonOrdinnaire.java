package com.mygdx.candycrush;

import com.badlogic.gdx.graphics.Color;
public class BonbonOrdinnaire extends Bonbon {
	
	/**Constructeur
	 * 
	 * @param x la ligne dans la grille
	 * @param y la colonne dans la grille
	 * @param c la couleur du bonbon ordinnaire
	 */
	public BonbonOrdinnaire(int x, int y, Color c) {
		super.ligne=y;
		super.colonne=x;
		super.couleur=c;
		super.animX=x;
		super.animY=y;
	}
}
