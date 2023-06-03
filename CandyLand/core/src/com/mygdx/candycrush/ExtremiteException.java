package com.mygdx.candycrush;

public class ExtremiteException extends Exception {
	int abcisse;
	int ordonnee;
	
	private String probleme;
	
	/** Initialiser ExtremiteException à partir d'une case présentant une opération impossible
	 * et du problème identifié.  Par exemple, l'indisponibilité matérielle d'un voisin.
	 * @param abcisse du bonbon.
	 * @param ordonnée du bonbon.
	 * @param probleme le problème identifié
	 */
	public ExtremiteException(int abcisse, int ordonnee, String probleme) {
		super("Coup invalide car " + probleme + " : " + probleme);
		this.abcisse = abcisse;
		this.ordonnee = ordonnee;
		this.probleme = probleme;
	}

	/** Retourner l'abcisse du bonbon critique.
	  * @return le coup */
	public int getAbcisse() {
		return this.abcisse;
	}
	
	/** Retourner l'ordonnee du bonbon critique.
	  * @return le coup */
	public int getOrdonnee() {
		return this.ordonnee;
	}

	/** Indiquer le problème.
	  * @return le problème */
	public String getProbleme() {
		return this.probleme;
	}
}
