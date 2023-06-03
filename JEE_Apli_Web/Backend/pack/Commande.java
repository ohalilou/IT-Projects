package pack;

import java.util.Collection;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
public class Commande {
	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private int id;

	/*@ManyToOne
	private Personne personne;*/

	private String etatCommande;
	/*@ManyToMany
	private Collection<Produit> produits;*/
	@OneToOne
	private Panier panier;

	public Commande() {
		
	}
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	/*public Personne getPersonne() {
		return personne;
	}
	public void setPersonne(Personne personne) {
		this.personne = personne;
	}*/
	public String getEtatCommande() {
		return etatCommande;
	}
	public Panier getPanier() {
		return panier;
	}
	public void setPanier(Panier panier) {
		this.panier = panier;
	}
	public void setEtatCommande(String etatCommande) {
		this.etatCommande = etatCommande;
	}
	/*public Collection<Produit> getProduits() {
		return produits;
	}
	public void setProduits(Collection<Produit> produits) {
		this.produits = produits;
	}

	public void addProduit(Produit p) {
		this.produits.add(p);
	}*/
	

}
