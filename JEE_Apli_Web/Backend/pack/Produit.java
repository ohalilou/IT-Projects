package pack;
import java.awt.Image;
import java.util.Collection;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
@Entity
public class Produit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    private String nom;
    private double prix;
    private String marque;
    private String description;
    private int nombre;
    //@Lob
    //private byte[] img;
    @ManyToOne
    @JsonIgnore
    Panier panier; 
    /*@ManyToMany
    Collection<Commande> commandes;*/

	public Produit() {
    }
    public Produit(String nom, double prix) {
        this.nom = nom;
        this.prix = prix;
    }

    public Produit(String nom, double prix, String description) {
        this.nom = nom;
        this.prix = prix;
        this.description = description;
        //this.img = img;
    }

	public String getNom() {
		return nom;
	}

	public void setNom(String nom) {
		this.nom = nom;
	}

	public double getPrix() {
		return prix;
	}

	public void setPrix(double prix) {
		this.prix = prix;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}
	public String getMarque() {
		return marque;
	}
	public void setMarque(String marque) {
		this.marque = marque;
	}

    public Panier getPanier() {
		return panier;
	}
	public void setPanier(Panier panier) {
		this.panier = panier;
	}
	public int getNombre() {
		return nombre;
	}
	public void setNombre(int nombre) {
		this.nombre = nombre;
	}

	/*public Image getImg() {
		return img;
	}

	public void setImg(Image img) {
		this.img = img;
	}*/
    
   
}    

