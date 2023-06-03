package com.mygdx.candycrush;


import com.badlogic.gdx.graphics.Color;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

public class Grille {
	public int hauteur;
	public int largeur;
	public Bonbon[][] grille;
	private boolean[][] abolition;
	private Random rand = new Random();
	private static Color[] clrs = { Color.GREEN, Color.RED, Color.BLUE, Color.YELLOW };
	private static Color clrspe =   Color.MAGENTA;
	final int SPACING = 70;

	

	/**
	 * retourne un bonbon selon les informations données en paramètres
	 * @param ligne
	 * @param colonne
	 * @param couleur
	 * @param special
	 * @return
	 */
	static Bonbon creerBonbon( int ligne, int colonne, int couleur, int special) {
		Bonbon leretour = null;
		boolean cookie = (special == 1);
		boolean raye = (special == 2);
		boolean emballe = (special == 3);
		if ( raye ) {
			leretour = new Raye(ligne, colonne, clrs[couleur]);
		} else if ( emballe ){
			leretour = new Emballe(ligne, colonne, clrs[couleur]);
		} else if ( cookie ) {
			leretour = new Cookie(ligne, colonne); 
		} else {
			leretour = new BonbonOrdinnaire(ligne, colonne, clrs[couleur]);
		}
		
		return leretour;
	}
	
	public boolean egalitetype(Bonbon b1, Bonbon b2) {
		return b1.getCouleur().equals(b2.getCouleur());
	}
	
	
	/**
	 * determine si un bonbon est une extremité haute de la grille
	 * @param bn le bonbon en question
	 * @return la valeur de vérité du bonbon étant extrémité haute
	 */
	private Boolean enextremiteh( Bonbon bn ) {
		return (bn.getL()/SPACING - 1 == hauteur);
	}
	
	/**
	 * determine si un bonbon est une extremité basse de la grille
	 * @param bn le bonbon en question
	 * @return la valeur de vérité du bonbon étant extrémité basse
	 */
	private Boolean enextremiteb( Bonbon bn ) {
		return (bn.getL()/SPACING - 1 == 0);
	}
	
	/**
	 * determine si un bonbon est une extremité gauche de la grille
	 * @param bn le bonbon en question
	 * @return la valeur de vérité du bonbon étant extrémité gauche
	 */
	private Boolean enextremiteg( Bonbon bn ) {
		return (transformerEnCoordonneeGrille(bn.getC()) == 0);
	}
	
	/**
	 * determine si un bonbon est une extremité droite de la grille
	 * @param bn le bonbon en question
	 * @return la valeur de vérité du bonbon étant extrémité droite
	 */
	private Boolean enextremited( Bonbon bn ) {
		return (bn.getC()/SPACING - 1 == largeur);
	}
	
	public Grille(int haut, int larg) {
		int special;
		int clr;
		this.hauteur = haut;
		this.largeur = larg;
		this.abolition = new boolean[haut][larg];
		this.grille = new Bonbon[haut][larg];
		for (int i=0; i < haut; i++) {
			for (int j = 0; j < larg; j++) {
				this.abolition[i][j] = false;
				special = rand.nextInt(4);
				if ( special == 0 ) {
					clr = rand.nextInt(4);
					this.grille[i][j] = creerBonbon((j+1)*SPACING, (i+1)*SPACING, clr, special);
				} else {
					clr = rand.nextInt(4);
					this.grille[i][j] = creerBonbon((j+1)*SPACING, (i+1)*SPACING, clr, special);
				}
			}
		}
	}
	
	/**permute deux bonbons.
	 * @param b1 le premier bonbon.
	 * @param b2 le deuxième bonbon.
	 */
	protected void permuter(Bonbon b1, Bonbon b2) {
		permuterCord(b1, b2);
		permuterDansGrille(b1, b2);
	}

	protected void permuterDansGrille(Bonbon b1, Bonbon b2) {
		Bonbon temp = this.grille[transformerEnCoordonneeGrille(b1.getL())][transformerEnCoordonneeGrille(b1.getC())];
		this.grille[transformerEnCoordonneeGrille(b1.getL())][transformerEnCoordonneeGrille(b1.getC())] = this.grille[transformerEnCoordonneeGrille(b2.getL())][transformerEnCoordonneeGrille(b2.getC())] ;
		this.grille[transformerEnCoordonneeGrille(b2.getL())][transformerEnCoordonneeGrille(b2.getC())] = temp;
	}

	private void permuterCouleur(Bonbon b1, Bonbon b2) {
		Color col = b1.getCouleur();
		b1.couleur = b2.getCouleur();
		b2.couleur = col;
	}

	private void permuterCord(Bonbon b1, Bonbon b2) {
		int cln = b1.getC();
		int ln = b1.getL();

		b1.setX(b2.getC());
		b1.setY(b2.getL());

		b2.setX(cln);
		b2.setY(ln);
	}
	
	/**retourn le voisin gauche d'un bonbon b.
	 * @param b le bonbon en question.
	 */
	protected Bonbon rechercherg(BonbonOrdinnaire b) throws ExtremiteException {
	    if (this.enextremiteg(b)){
	    	throw new ExtremiteException(b.getL(),b.getC(), "bonbon limite gauche");
	    }
	    Bonbon voisin = null;
        if (egalitetype( this.grille[b.getL()-1][b.getC()-2], b) ){
        	voisin = grille[b.getL()-2][b.getC()-1];
        }
        return voisin;
    }
	
	/**retourn le voisin droit d'un bonbon b.
	 * @param b le bonbon en question.
	 */
	protected Bonbon rechercherd(BonbonOrdinnaire b) throws ExtremiteException {
		if ( this.enextremited(b) ) {
	        throw new ExtremiteException(b.getL(),b.getC(),"bonbon limite droite");
	    }
		Bonbon voisin =null;
        if (egalitetype( this.grille[b.getL()-1][b.getC()], b )){
        	voisin = grille[b.getL()-1][b.getC()];
        }
        return voisin;
	}
	
	/**retourn le voisin haut d'un bonbon b.
	 * @param b le bonbon en question.
	 */
	protected Bonbon rechercherh(BonbonOrdinnaire b) throws ExtremiteException {
		if (this.enextremiteh(b) ){
			throw new ExtremiteException(b.getL(),b.getC(),"bonbon limite haut");
	    }
		Bonbon voisin = null;
        if (egalitetype( this.grille[b.getL()-2][b.getC()-1], b )){
            voisin = grille[b.getL()-2][b.getC()-1];
        }
        return voisin;
	}
	
	/**retourn le voisin bas d'un bonbon b.
	 * @param b le bonbon en question.
	 */
	protected Bonbon rechercherb(Bonbon b) throws ExtremiteException {
	if ( this.enextremiteb(b) ){
		throw new ExtremiteException(b.getL(), b.getC(), "bonbon limite bas");
	}
	Bonbon voisin = null;
    if ( egalitetype(this.grille[b.getL()][b.getC()-1], b ) ) {
    	voisin = grille[b.getL()][b.getC()-1];
        }
    return voisin;
	}
	
	/**
	 * compte le nombre de voisins (formant une ligne continue d'au moins 2 bonbons) à gauche du bonbon
	 * @param bn le bonbon en question
	 * @return le nombre de voisins à gauche ...
	 */
	private int Nbvoisinsgauche(Bonbon bn) {
		int vg = 0;
		if (!this.enextremiteg(bn)) {
			int h = transformerEnCoordonneeGrille(bn.getC()) - 1;
			while ( h >= 0 &&  grille[transformerEnCoordonneeGrille(bn.getL())][h] != null && egalitetype(grille[transformerEnCoordonneeGrille(bn.getL())][h], bn)) {
				vg++;
				h--;
			}
			
		}
		return vg;
	}
	
	
	
	/**
	 * compte le nombre de voisins (formant une ligne continue d'au moins 2 bonbons) à droite du bonbon
	 * @param bn le bonbon en question
	 * @return le nombre de voisins à droite ...
	 */
	private int Nbvoisinsdroite( Bonbon bn) {
		int vd = 0;
		if (!this.enextremited(bn)) {
			int h = transformerEnCoordonneeGrille(bn.getC()) + 1;
			while ( h < largeur &&  grille[transformerEnCoordonneeGrille(bn.getL())][h] != null && egalitetype(grille[transformerEnCoordonneeGrille(bn.getL())][h], bn)) {
				vd++;
				h++;
			}
			
		}
		return vd;
	}
	
	/**
	 * compte le nombre de voisins (formant une ligne continue d'au moins 2 bonbons) en haut du bonbon
	 * @param bn le bonbon en question
	 * @return le nombre de voisins en haut ...
	 */
	private int Nbvoisinshaut( Bonbon bn) {
		int vh = 0;
		if (!this.enextremiteh(bn)) {
			int h = transformerEnCoordonneeGrille(bn.getL()) + 1;
			while ( h < hauteur  &&  grille[h][transformerEnCoordonneeGrille(bn.getC())] != null && egalitetype(grille[h][transformerEnCoordonneeGrille(bn.getC())], bn)) {
				vh++;
				h++;
			}
			
		}
		return vh;
	}
	
	/**
	 * compte le nombre de voisins (formant une ligne continue d'au moins 2 bonbons) en bas du bonbon
	 * @param bn le bonbon en question
	 * @return le nombre de voisins en bas ...
	 */
	private int Nbvoisinsbas( Bonbon bn) {
		int vb = 0;
		if (!this.enextremiteb(bn)) {
			int h = transformerEnCoordonneeGrille(bn.getL()) - 1;
			while ( h >= 0 && grille[h][transformerEnCoordonneeGrille(bn.getC())] != null && egalitetype(grille[h][transformerEnCoordonneeGrille(bn.getC())], bn)) {
				vb = vb +1;
				h--;
			}
			
		}
		return vb;
	}
	
	/**
	 * retourne le nombre de cases vide dans une colonne j de la grille
	 * @param j le numéro de la colonne
	 * @return le nombre de vides dans la colonne j de la grille
	 */
	protected int nbvides(int j ) {
		int resultat = 0;
		for ( int k = 0; k<hauteur; k++) {
			if ( grille[k][j] == null ) {
				resultat = resultat + 1;
			}
		}
		return resultat;
	}
	
	
	/**
	 * determine le nombre des premières cases basses non vides de la grille dans une certaine colonne j
	 * @param j le numéro de la colonne concernée
	 * @return le nombre des premières cases basses non vides de la grille dans la colonne j
	 */
	protected int nbnonvideshaut(int j) {
		int resultat = 0;
		int k = hauteur - 1;
		while (k >= 0 && grille[k][j] != null) {
			resultat++;
			k = k-1;
		}
		return resultat;
	}
	
	
	/**
	 * remet tous les cases vides d'une colonne de la grille en haut et fait descendre dans la même répartition ceux non vides
	 * @param j le numéro de la colonne concernée
	 */
	protected void videshautverti(int j) {
		int h =  this.nbnonvideshaut(j) ;
		while (  this.nbnonvideshaut(j) != hauteur - this.nbvides(j) ) {
			if ( h == 0 ) {
				h = hauteur - this.nbnonvideshaut(j) - 1;
			} else {
				if (grille[h][j] == null) {
					this.permuter(grille[h][j], grille[h-1][j]);
					h = h-1;
				}
			}
		}
	}

	protected void descendreCol(int j) {
		int h =  this.nbnonvideshaut(j);
		int vid = this.nbvides(j);
		if (h != 0) {
			for (int i=hauteur-h; i<hauteur ;i++) {
				descendreElem(grille[i][j], vid);
			}
		}
	}

	// n'oublie pas de faire l'animation 
	private void descendreElem(Bonbon bb, int n) {
		int i = transformerEnCoordonneeGrille(bb.getL());
		int j = transformerEnCoordonneeGrille(bb.getC());
		bb.setY(bb.getL()-n*SPACING);
		if (i-n >= 0) {
			this.grille[i-n][j] = this.grille[i][j];
			this.grille[i][j] = null;
		}
	}

	
	/**
	 * remet les cases non vides de chacune des colonnes des bonbons formant un alignement horizontal en bas de ces colonnes et fait monter
	 * celles vides en haut.
	 * @param i
	 * @param nbg le nbr de voisins à gauche
	 * @param nbd le nbr de voisins à droite
	 */
	private void videshauthorizon(int i, int nbg, int nbd) {
		for (int k = i-nbg; k <= i+nbd; k++) {
			this.videshautverti(k);
		}
	}
	
	
	
	/*public void changer0(j){//changer les 0 en haut de la grille par des bonbons géneré aleatoirement    
    		int nb0=grille.nombre0(j);
    		int n = this.hauteur;
    		int p = this.largeur;
    		for(i=0;i<=nb0;i++){
         		Random random = new Random();
         		int nb;
         		nb = 1+random.nextInt(4);//générer un entier entre 1 et 4
         		grille[i][j]= clrs[nb];
    		}
    
	} cette md est en cours de construction*/
	
	
	/**supprime les alignements présents dans la grille.
	 */
	protected void supprimer() {
		//coming soon
	}
	
	/**marque en true les cases dont les coordonnées dans coups sont celles de bonbons 
	 * présentant un alignement sur grille.
	 */
	protected void scanner() {
		//coming soon
	}
	
	/**actualisation de l'état de grille et coups après destructtions des alignements.
	 */
	protected void remplir() {
		//coming soon
	}

	protected boolean sontVoisins(Bonbon b1, Bonbon b2) {
		if (((Math.abs(b1.getL() - b2.getL()) == SPACING) & (Math.abs(b1.getC() - b2.getC()) == 0))
		|| ((Math.abs(b1.getC() - b2.getC()) == SPACING) & (Math.abs(b1.getL() - b2.getL()) == 0))) {
			return true;
		}
		return false;
	}


	public static ArrayList<Integer> convertArrayToSet(List<Integer> array)	
    {
		ArrayList<Integer> res = new ArrayList<>();
        Set<Integer> set = new HashSet<>(array);
		
		for (Integer i : set) {
			res.add(i);
		}

        return res;
    }



	public void changer0(int j) { //changer les 0 en haut de la grille par des bonbons géneré aleatoirement    
		int nb0 = this.nbvides(j);
		int randomcolor;
		for (int i = hauteur-1; i >= hauteur-nb0 ; i--) {

			//générer un entier entre 1 et 4 pour le choix de la couleur, et par suite le choix du bonbon
			randomcolor = rand.nextInt(4); 

			//rendre la probabilité d'avoir un bonbon spécial de 1/4 (nope)
			grille[i][j]= creerBonbon((j+1)*SPACING, (i+1)*SPACING, randomcolor, 0);
			grille[i][j].animate();
		}

} 


	/**
	 * combine la remontée des vides en haut et le remplissage des cases vides de la colonne j de la grille
	 * @param j le numéro de la grille
	 */
	public void remplirecol(int j) {
		this.descendreCol(j);
		this.changer0(j);
	}


	/**
		 * renvois une liste (redondances non traités, en fait non considérés) des nombre de 
		 * voisins de même type de chaque coté de bonbon dan la ligne i et colonne j
		 * @param bb bonbon
		 * @return la liste des nombres de voisins de chauqe côté
		 * @throws ExtremiteException
		 */
		public ArrayList<Integer> getNbsVoisins(Bonbon bb) {

			ArrayList<Integer> liste = new ArrayList<>();
			liste.add(this.Nbvoisinsgauche(bb));
			liste.add(this.Nbvoisinsdroite(bb));
			liste.add(this.Nbvoisinshaut(bb));
			liste.add(this.Nbvoisinsbas(bb));
			return liste;
		}


	/**
	 * detruit les alignements (3 bonbons (ordinaires) ou plus de même type alignés)
	 * horizontalement et verticalement et renvoie la liste des colonnes comportant les bonbons détruits
	 * @param i le numéro de ligne du bonbon concerné
	 * @param j le numéro de colonne du bonbon concerné
	 * @return
	 */
	public ArrayList<Integer> detruitali(Bonbon b1){
		if (b1==null) {
			ArrayList<Integer> l = new ArrayList<Integer>();
			l.add(0);
			l.add(0);
			l.add(0);
			l.add(0);
			return l;
		} 
		ArrayList<Integer> leretour = new ArrayList<>();
		ArrayList<Integer> listevoisin = this.getNbsVoisins(b1);
		int i = transformerEnCoordonneeGrille(b1.getL());
		int j = transformerEnCoordonneeGrille(b1.getC());
		int g = listevoisin.get(0); 
		int d = listevoisin.get(1); 
		int h = listevoisin.get(2);
		int b = listevoisin.get(3);
		boolean dethor = false;
		boolean detver = false;
		if (g+d > 1) {
			dethor = true;
			for ( int k = j-g; k < j+d+1; k++) {
				//Bonbon.detruire(grille[i][k]);
				if (grille[i][k] != null) destroyCandyInGrid(grille[i][k]);
			}
		} 
		if (h+b > 1) {
			detver = true;
			for (int k = i-b; k<i+h+1; k++) {
				if (grille[k][j] != null) destroyCandyInGrid(grille[k][j]);
				//Bonbon.detruire(grille[k][j]);
				
			}
		}
		
		if (dethor) {
			//leretour.add(-1);
			for (int k = j-g; k<j+d+1; k++) {
				leretour.add(k);
			}
		} else if (detver) {
			leretour.add(j);
		}
		
		
		//else if (det_ver) {
			//leretour.add(-2);
			//for (int k = i-q-1; k<i+r; k++) {
				//leretour.add(k);
			//}
		//}
		
		return leretour;
	}

	public void remplir(ArrayList<Integer> l) {
		for (Integer j : l) {
			this.remplirecol(j);
		}
	}


	public ArrayList<Integer> detruitali2(int i, int j) {
		return null;
	}

	/**
	 * detruit les alignement possibles au long de la colonne j
	 * @param j le numero de colonne concerné
	 * @return
	 */
	//ici on utilise une detruitali2 améliorée
	public ArrayList<Integer> detruitcolali( int j) {
		ArrayList<Integer> res = new ArrayList<>();
		for(int k = 0; k < hauteur; k++) {
			res.addAll(this.detruitali2(k,j));
		}
		return res;
	}

	public ArrayList<Integer> detruitcolsali(ArrayList<Integer> S) {
		ArrayList<Integer> res = new ArrayList<>();
		for (int k : S) {
			res.addAll(this.detruitcolali(k));
		}
		return null;
	}



	protected void traiter(Bonbon b1, Bonbon b2) {
		ArrayList<Integer> l = detruitali(b1);
		l.addAll(detruitali(b2));
		remplir(l);
		ArrayList<Integer> set_l = convertArrayToSet(l);
		while (!l.isEmpty()) {
			l = detruitcolsali(set_l);
			remplir(set_l);
		}
	}

	protected ArrayList<Integer> detruitAliDouble(Bonbon b1, Bonbon b2) {
		ArrayList<Integer> l = detruitali(b1);
		l.addAll(detruitali(b2));
		return convertArrayToSet(l);
	}
	
	protected void jouer(Bonbon b1, Bonbon b2) {
		permuter(b1,b2);
		traiter(b1,b2);
	} 

	protected int taille_ali(Bonbon b1) {
		ArrayList<Integer> voisins = getNbsVoisins(b1);
		int somme = 0;
		for (int n : voisins) {
			somme += n;
		}
		return somme;
	}

	protected int[] position_ali(Bonbon b1) {
		ArrayList<Integer> voisins = getNbsVoisins(b1);
		int[] position = new int[2];
		int i = position[0] = b1.getL();
		int j = position[1] = b1.getC();
		int seuil = taille_ali(b1); 

		for (int k=position[1] - voisins.get(0); k < position[1] - voisins.get(1) + 1; k++) {
			if (taille_ali(grille[i][k]) > seuil) {
				seuil = taille_ali(grille[i][k]);
				position[1] = k;
			}
		}

		for (int k=position[0] - voisins.get(2); k < position[0] - voisins.get(3) + 1; k++) {
			if (taille_ali(grille[k][j]) > seuil) {
				seuil = taille_ali(grille[k][j]);
				position[0] = k;
			}
		}
		
		return position;
		
	}

	protected ArrayList<Integer> detruitali2(Bonbon b1) {
		ArrayList<Integer> voisins = getNbsVoisins(b1);
		int[] centre = {b1.getC(), b1.getL()};
		if ((voisins.get(0)+voisins.get(1) > 1) || (voisins.get(2)+voisins.get(3) > 1)) {
			centre = position_ali(b1);
		}
		return detruitali(grille[centre[0]][centre[1]]);

	}

    public void destroyCandyInGrid(Bonbon b1) {
		this.grille[b1.getL()/SPACING-1][b1.getC()/SPACING-1] = null;
		System.gc();
    }

	protected int transformerEnCoordonneeGrille(int n) {
		return n/SPACING - 1;
	}


}


