package pack;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.persistence.*;

@Entity
public class Panier {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;
    @OneToOne(mappedBy="panier",fetch = FetchType.LAZY)
    private Personne personne;
    @OneToMany(mappedBy="panier",cascade = CascadeType.ALL)
    private Collection<Produit> produits;
    @OneToOne(mappedBy="panier",cascade = CascadeType.ALL)
    private Commande commande;
    
    public Panier() {

    }
    
    /*public void ajouterProduit(int idprod) {
        produits.add(idprod);
    }
    */
    public void supprimerProduit(Produit product) {
        produits.remove(product);
    }
    
    public Personne getPersonne() {
		return personne;
	}

	public void setPersonne(Personne personne) {
		this.personne = personne;
	}

	public double calculateTotalPrice() {
        double totalPrice = 0.0;
        for (Produit product : produits) {
            totalPrice += product.getPrix();
        }
        return totalPrice;
    }

    // getters and setters

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public Collection<Produit> getProduits() {
		return produits;
	}

	public void setProduits(Collection<Produit> produits) {
		this.produits = produits;
	}

	public void ajouterProduit(Produit p) {
		produits.add(p);
	}

	public Commande getCommande() {
		return commande;
	}

	public void setCommande(Commande commande) {
		this.commande = commande;
	}   
}



